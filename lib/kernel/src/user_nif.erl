%%
%% %CopyrightBegin%
%% 
%% Copyright Ericsson AB 1996-2021. All Rights Reserved.
%% 
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%% %CopyrightEnd%
%%
-module(user_nif).

%% Basic interface to a port.

-export([start/0,server/2,server/3]).

-export([interfaces/1]).

-include_lib("kernel/include/logger.hrl").

%% start()
%%  Start the user driver server. The arguments to start/1 are slightly
%%  strange as this may be called both at start up from the command line
%%  and explicitly from other code.

-spec start() -> pid().

start() ->					%Default line editing shell
    spawn(?MODULE, server, [[{canon,false}, {echo,false}], {shell, start, [init]}]).

%% Return the pid of the active group process.
%% Note: We can't ask the user_drv process for this info since it
%% may be busy waiting for data from the port.

-spec interfaces(pid()) -> [{'current_group', pid()}].

interfaces(UserDrv) ->
    case process_info(UserDrv, dictionary) of
	{dictionary,Dict} ->
	    case lists:keysearch(current_group, 1, Dict) of
		{value,Gr={_,Group}} when is_pid(Group) ->
		    [Gr];
		_ ->
		    []
	    end;
	_ ->
	    []
    end.

%% server(Pid, Shell)
%% server(Pname, Shell)
%% server(Iname, Oname, Shell)
%%  The initial calls to run the user driver. These start the port(s)
%%  then call server1/3 to set everything else up.

server(Pid, Shell) when is_pid(Pid) ->
    server1(Pid, Pid, Shell);
server(Options, Shell) ->
    process_flag(trap_exit, true),
    case prim_tty:init(Options) of
        {ok, Ref} ->
            server1(Ref, Ref, Shell);
        {error, _Reason} ->
            user:start()
    end.

server(Iname, Oname, Shell) ->
    process_flag(trap_exit, true),
    case catch open_port({spawn,Iname}, [eof]) of
	{'EXIT', _} -> %% It might be a dumb terminal lets start dumb user
	    user:start();
	Iport ->
	    Oport = open_port({spawn,Oname}, [eof]),
	    server1(Iport, Oport, Shell)
    end.

server1(Input, Output, Shell) ->
    put(eof, false),
    %% Start user and initial shell.
    User = start_user(),
    Gr1 = gr_add_cur(gr_new(), User, {}),

    {Curr,Shell1} =
	case init:get_argument(remsh) of
	    {ok,[[Node]]} ->
                ANode =
                    if
                        node() =:= nonode@nohost ->
                            %% We try to connect to the node if the current node is not
                            %% a distributed node yet. If this succeeds it means that we
                            %% are running using "-sname undefined".
                            _ = net_kernel:start([undefined, shortnames]),
                            NodeName = append_hostname(Node, net_kernel:nodename()),
                            case net_kernel:connect_node(NodeName) of
                                true ->
                                    NodeName;
                                _Else ->
                                    ?LOG_ERROR("Could not connect to ~p",[Node])
                            end;
                        true ->
                            append_hostname(Node, node())
                    end,

		RShell = {ANode,shell,start,[]},
		RGr = group:start(self(), RShell, rem_sh_opts(ANode)),
		{RGr,RShell};
	    E when E =:= error ; E =:= {ok,[[]]} ->
		{group:start(self(), Shell, [{user_mod, ?MODULE}]),Shell}
	end,

    put(current_group, Curr),
    Gr = gr_add_cur(Gr1, Curr, Shell1),
    %% Print some information.
    io_request({put_chars, unicode,
		flatten(io_lib:format("~ts\n",
				      [erlang:system_info(system_version)]))},
	       Input, Output),

    %% Enter the server loop.
    try
        server_loop(Input, Output, Curr, User, Gr, {false, queue:new()})
    catch Er:R:ST ->
            erlang:display({Er,R,ST}),
            erlang:raise(Er,R,ST)
    end.

append_hostname(Node, LocalNode) ->
    case string:find(Node,"@") of
        nomatch ->
            list_to_atom(Node ++ string:find(atom_to_list(LocalNode),"@"));
        _ ->
            list_to_atom(Node)
    end.

rem_sh_opts(Node) ->
    [{expand_fun,fun(B)-> rpc:call(Node,edlin_expand,expand,[B]) end}].

%% start_user()
%%  Start a group leader process and register it as 'user', unless,
%%  of course, a 'user' already exists.

start_user() ->
    case whereis(?MODULE) of
	undefined ->
	    register(?MODULE, self());
	_ ->
	    ok
    end,
    case whereis(user) of
	undefined ->
	    User = group:start(self(), {}),
	    register(user, User),
	    User;
	User ->
	    User
    end.
   
server_loop(Input, Output, User, Gr, IOQueue) ->
    Curr = gr_cur_pid(Gr),
    put(current_group, Curr),
    server_loop(Input, Output, Curr, User, Gr, IOQueue).

server_loop(Input, Output, Curr, User, Gr, {Resp, IOQ} = IOQueue) ->
    receive
	{Input,{data,Bs}} ->
	    BsBin = iolist_to_binary(Bs),
	    Unicode = case unicode:characters_to_list(BsBin) of
                          {error, _, _} ->
                              erlang:display({failed, BsBin}),
                              "";
                          U -> U
                      end,
            %% erlang:display({server_loop, input, Unicode}),
	    port_bytes(Unicode, Input, Output, Curr, User, Gr, IOQueue);
	{Input,eof} ->
	    Curr ! {self(),eof},
	    server_loop(Input, Output, Curr, User, Gr, IOQueue);

        %% We always handle geometry and unicode requests
        {Requester,tty_geometry} ->
            Requester ! {self(),tty_geometry,get_tty_geometry(Input)},
            server_loop(Input, Output, Curr, User, Gr, IOQueue);
        {Requester,get_unicode_state} ->
            Requester ! {self(),get_unicode_state,get_unicode_state(Input)},
            server_loop(Input, Output, Curr, User, Gr, IOQueue);
        {Requester,set_unicode_state, Bool} ->
            Requester ! {self(),set_unicode_state,set_unicode_state(Input,Bool)},
            server_loop(Input, Output, Curr, User, Gr, IOQueue);

        Req when element(1,Req) =:= User orelse element(1,Req) =:= Curr,
                 tuple_size(Req) =:= 2 orelse tuple_size(Req) =:= 3 ->
            %% We match {User|Curr,_}|{User|Curr,_,_}
            NewQ = handle_req(Req, Input, Output, IOQueue),
            server_loop(Input, Output, Curr, User, Gr, NewQ);
        {Output,ok} ->
            %% We get this ok from the port, in io_request we store
            %% info about where to send reply at head of queue
            {Origin,Reply} = Resp,
            Origin ! {reply,Reply},
            NewQ = handle_req(next, Input, Output, {false, IOQ}),
            server_loop(Input, Output, Curr, User, Gr, NewQ);
	{'EXIT',Input,_R} ->
	    server_loop(Input, Output, Curr, User, Gr, IOQueue);
	{'EXIT',Output,_R} ->
	    server_loop(Input, Output, Curr, User, Gr, IOQueue);
        {'EXIT',User,shutdown} ->               % force data to port
	    server_loop(Input, Output, Curr, User, Gr, IOQueue);
	{'EXIT',User,_R} ->			% keep 'user' alive
	    NewU = start_user(),
	    server_loop(Input, Output, Curr, NewU, gr_set_num(Gr, 1, NewU, {}), IOQueue);
	{'EXIT',Pid,R} ->			% shell and group leader exit
	    case gr_cur_pid(Gr) of
		Pid when R =/= die ,
			 R =/= terminated  ->	% current shell exited
		    if R =/= normal ->
			    io_requests([{put_chars,unicode,"*** ERROR: "}], Input, Output);
		       true -> % exit not caused by error
			    io_requests([{put_chars,unicode,"*** "}], Input, Output)
		    end,
		    io_requests([{put_chars,unicode,"Shell process terminated! "}], Input, Output),
		    Gr1 = gr_del_pid(Gr, Pid),		    
		    case gr_get_info(Gr, Pid) of
			{Ix,{shell,start,Params}} -> % 3-tuple == local shell
			    io_requests([{put_chars,unicode,"***\n"}], Input, Output),	    
			    %% restart group leader and shell, same index
			    Pid1 = group:start(self(), {shell,start,Params}),
			    {ok,Gr2} = gr_set_cur(gr_set_num(Gr1, Ix, Pid1, 
							     {shell,start,Params}), Ix),
			    put(current_group, Pid1),
			    server_loop(Input, Output, Pid1, User, Gr2, IOQueue);
			_ -> % remote shell
			    io_requests([{put_chars,unicode,"(^G to start new job) ***\n"}],
					Input, Output),	    
			    server_loop(Input, Output, Curr, User, Gr1, IOQueue)
		    end;
		_ ->				% not current, just remove it
		    server_loop(Input, Output, Curr, User, gr_del_pid(Gr, Pid), IOQueue)
	    end;
        {Requester, {put_chars_sync, _, _, Reply}} ->
            %% We need to ack the Req otherwise originating process will hang forever
            %% Do discard the output to non visible shells (as was done previously)
            Requester ! {reply, Reply},
            server_loop(Input, Output, Curr, User, Gr, IOQueue);
	_X ->
            %% Ignore unknown messages.
            server_loop(Input, Output, Curr, User, Gr, IOQueue)
    end.

handle_req(next,Input,Output,{false,IOQ}=IOQueue) ->
    case queue:out(IOQ) of
        {empty,_} ->
	    IOQueue;
        {{value,{Origin,Req}},ExecQ} ->
            case io_request(Req, Input, Output) of
                ok ->
		    handle_req(next,Input,Output,{false,ExecQ});
                Reply ->
		    {{Origin,Reply}, ExecQ}
            end
    end;
handle_req(Msg,Input,Output,{false,IOQ}=IOQueue) ->
    empty = queue:peek(IOQ),
    {Origin,Req} = Msg,
    case io_request(Req, Input, Output) of
	ok ->
	    IOQueue;
	Reply ->
	    {{Origin,Reply}, IOQ}
    end;
handle_req(Msg,_Input,_Output,{Resp, IOQ}) ->
    %% All requests are queued when we have outstanding sync put_chars
    {Resp, queue:in(Msg,IOQ)}.

%% port_bytes(Bytes, InPort, OutPort, CurrentProcess, UserProcess, Group)
%%  Check the Bytes from the port to see if it contains a ^G. If so,
%%  either escape to switch_loop or restart the shell. Otherwise send 
%%  the bytes to Curr.

port_bytes([$\^G|_Bs], Input, Output, _Curr, User, Gr, IOQueue) ->
    handle_escape(Input, Output, User, Gr, IOQueue);

port_bytes([$\^C|_Bs], Input, Output, Curr, User, Gr, IOQueue) ->
    interrupt_shell(Input, Output, Curr, User, Gr, IOQueue);

port_bytes([B], Input, Output, Curr, User, Gr, IOQueue) ->
    Curr ! {self(),{data,[B]}},
    server_loop(Input, Output, Curr, User, Gr, IOQueue);
port_bytes(Bs, Input, Output, Curr, User, Gr, IOQueue) ->
    case member($\^G, Bs) of
	true ->
	    handle_escape(Input, Output, User, Gr, IOQueue);
	false ->
	    Curr ! {self(),{data,Bs}},
	    server_loop(Input, Output, Curr, User, Gr, IOQueue)
    end.

interrupt_shell(Input, Output, Curr, User, Gr, IOQueue) ->
    case gr_get_info(Gr, Curr) of
	undefined -> 
	    ok;					% unknown
	_ ->
	    exit(Curr, interrupt)
    end,
    server_loop(Input, Output, Curr, User, Gr, IOQueue).

handle_escape(Input, Output, User, Gr, IOQueue) ->
    case application:get_env(stdlib, shell_esc) of
	{ok,abort} ->
	    Pid = gr_cur_pid(Gr),
	    exit(Pid, die),
	    Gr1 =
		case gr_get_info(Gr, Pid) of
		    {_Ix,{}} ->			% no shell
			Gr;
		    _ ->
			receive {'EXIT',Pid,_} ->
				gr_del_pid(Gr, Pid)
			after 1000 ->
				Gr
			end
		end,
	    Pid1 = group:start(self(), {shell,start,[]}),
	    io_request({put_chars,unicode,"\n"}, Input, Output),
	    server_loop(Input, Output, User, 
			gr_add_cur(Gr1, Pid1, {shell,start,[]}), IOQueue);

	_ ->					% {ok,jcl} | undefined
	    io_request({put_chars,unicode,"\nUser switch command\n"}, Input, Output),
	    %% init edlin used by switch command and have it copy the
	    %% text buffer from current group process
	    edlin:init(gr_cur_pid(Gr)),
	    server_loop(Input, Output, User, switch_loop(Input, Output, Gr), IOQueue)
    end.

switch_loop(Input, Output, Gr) ->
    Line = get_line(edlin:start(" --> "), Input, Output),
    switch_cmd(erl_scan:string(Line), Input, Output, Gr).

switch_cmd({ok,[{atom,_,c},{integer,_,I}],_}, Input, Output, Gr0) ->
    case gr_set_cur(Gr0, I) of
	{ok,Gr} -> Gr;
	undefined -> unknown_group(Input, Output, Gr0)
    end;
switch_cmd({ok,[{atom,_,c}],_}, Input, Output, Gr) ->
    case gr_get_info(Gr, gr_cur_pid(Gr)) of
	undefined -> 
	    unknown_group(Input, Output, Gr);
	_ ->
	    Gr
    end;
switch_cmd({ok,[{atom,_,i},{integer,_,I}],_}, Input, Output, Gr) ->
    case gr_get_num(Gr, I) of
	{pid,Pid} ->
	    exit(Pid, interrupt),
	    switch_loop(Input, Output, Gr);
	undefined ->
	    unknown_group(Input, Output, Gr)
    end;
switch_cmd({ok,[{atom,_,i}],_}, Input, Output, Gr) ->
    Pid = gr_cur_pid(Gr),
    case gr_get_info(Gr, Pid) of
	undefined -> 
	    unknown_group(Input, Output, Gr);
	_ ->
	    exit(Pid, interrupt),
	    switch_loop(Input, Output, Gr)
    end;
switch_cmd({ok,[{atom,_,k},{integer,_,I}],_}, Input, Output, Gr) ->
    case gr_get_num(Gr, I) of
	{pid,Pid} ->
	    exit(Pid, die),
	    case gr_get_info(Gr, Pid) of
		{_Ix,{}} ->			% no shell
		    switch_loop(Input, Output, Gr);
		_ ->
		    Gr1 = 
			receive {'EXIT',Pid,_} ->
				gr_del_pid(Gr, Pid)
			after 1000 ->
				Gr
			end,
		    switch_loop(Input, Output, Gr1)
	    end;
	undefined ->
	    unknown_group(Input, Output, Gr)
    end;
switch_cmd({ok,[{atom,_,k}],_}, Input, Output, Gr) ->
    Pid = gr_cur_pid(Gr),
    Info = gr_get_info(Gr, Pid), 
    case Info of
	undefined ->
	    unknown_group(Input, Output, Gr);
	{_Ix,{}} ->				% no shell
	    switch_loop(Input, Output, Gr);
	_ ->
	    exit(Pid, die),
	    Gr1 = 
		receive {'EXIT',Pid,_} ->
			gr_del_pid(Gr, Pid)
		after 1000 ->
			Gr
		end,
	    switch_loop(Input, Output, Gr1)
    end;
switch_cmd({ok,[{atom,_,j}],_}, Input, Output, Gr) ->
    io_requests(gr_list(Gr), Input, Output),
    switch_loop(Input, Output, Gr);
switch_cmd({ok,[{atom,_,s},{atom,_,Shell}],_}, Input, Output, Gr0) ->
    Pid = group:start(self(), {Shell,start,[]}),
    Gr = gr_add_cur(Gr0, Pid, {Shell,start,[]}),
    switch_loop(Input, Output, Gr);
switch_cmd({ok,[{atom,_,s}],_}, Input, Output, Gr0) ->
    Pid = group:start(self(), {shell,start,[]}),
    Gr = gr_add_cur(Gr0, Pid, {shell,start,[]}),
    switch_loop(Input, Output, Gr);
switch_cmd({ok,[{atom,_,r}],_}, Input, Output, Gr0) ->
    case is_alive() of
	true ->
	    Node = pool:get_node(),
	    Pid = group:start(self(), {Node,shell,start,[]}),
	    Gr = gr_add_cur(Gr0, Pid, {Node,shell,start,[]}),
	    switch_loop(Input, Output, Gr);
	false ->
	    io_request({put_chars,unicode,"Not alive\n"}, Input, Output),
	    switch_loop(Input, Output, Gr0)
    end;
switch_cmd({ok,[{atom,_,r},{atom,_,Node}],_}, Input, Output, Gr0) ->
    Pid = group:start(self(), {Node,shell,start,[]}),
    Gr = gr_add_cur(Gr0, Pid, {Node,shell,start,[]}),
    switch_loop(Input, Output, Gr);
switch_cmd({ok,[{atom,_,r},{atom,_,Node},{atom,_,Shell}],_},
	   Input, Output, Gr0) ->
    Pid = group:start(self(), {Node,Shell,start,[]}),
    Gr = gr_add_cur(Gr0, Pid, {Node,Shell,start,[]}),
    switch_loop(Input, Output, Gr);
switch_cmd({ok,[{atom,_,q}],_}, Input, Output, Gr) ->
    case erlang:system_info(break_ignored) of
	true ->					% noop
	    io_request({put_chars,unicode,"Unknown command\n"}, Input, Output),
	    switch_loop(Input, Output, Gr);
	false ->
	    halt()
    end;
switch_cmd({ok,[{atom,_,h}],_}, Input, Output, Gr) ->
    list_commands(Input, Output),
    switch_loop(Input, Output, Gr);
switch_cmd({ok,[{'?',_}],_}, Input, Output, Gr) ->
    list_commands(Input, Output),
    switch_loop(Input, Output, Gr);
switch_cmd({ok,[],_}, Input, Output, Gr) ->
    switch_loop(Input, Output, Gr);
switch_cmd({ok,_Ts,_}, Input, Output, Gr) ->
    io_request({put_chars,unicode,"Unknown command\n"}, Input, Output),
    switch_loop(Input, Output, Gr);
switch_cmd(_Ts, Input, Output, Gr) ->
    io_request({put_chars,unicode,"Illegal input\n"}, Input, Output),
    switch_loop(Input, Output, Gr).

unknown_group(Input, Output, Gr) ->
    io_request({put_chars,unicode,"Unknown job\n"}, Input, Output),
    switch_loop(Input, Output, Gr).

list_commands(Input, Output) ->
    QuitReq = case erlang:system_info(break_ignored) of
		  true -> 
		      [];
		  false ->
		      [{put_chars, unicode,"  q                 - quit erlang\n"}]
	      end,
    io_requests([{put_chars, unicode,"  c [nn]            - connect to job\n"},
		 {put_chars, unicode,"  i [nn]            - interrupt job\n"},
		 {put_chars, unicode,"  k [nn]            - kill job\n"},
		 {put_chars, unicode,"  j                 - list all jobs\n"},
		 {put_chars, unicode,"  s [shell]         - start local shell\n"},
		 {put_chars, unicode,"  r [node [shell]]  - start remote shell\n"}] ++
		QuitReq ++
		[{put_chars, unicode,"  ? | h             - this message\n"}],
		Input, Output).

get_line({done,Line,_Rest,Rs}, Input, Output) ->
    io_requests(Rs, Input, Output),
    Line;
get_line({undefined,_Char,Cs,Cont,Rs}, Input, Output) ->
    io_requests(Rs, Input, Output),
    io_request(beep, Input, Output),
    get_line(edlin:edit_line(Cs, Cont), Input, Output);
get_line({What,Cont0,Rs}, Input, Output) ->
    io_requests(Rs, Input, Output),
    receive
	{Input,{data,Cs}} ->
        Unicode = case unicode:characters_to_list(Cs) of
            {error, _, _} ->
                erlang:display({failed, Cs}),
                "";
            U -> U
        end,
	    get_line(edlin:edit_line(Unicode, Cont0), Input, Output);
	{Input,eof} ->
	    get_line(edlin:edit_line(eof, Cont0), Input, Output)
    after
	get_line_timeout(What) ->
	    get_line(edlin:edit_line([], Cont0), Input, Output)
    end.

get_line_timeout(blink) -> 1000;
get_line_timeout(more_chars) -> infinity.

% Let driver report window geometry,
% definitely outside of the common interface
get_tty_geometry(Input) ->
    case prim_tty:window_size(Input) of
        {ok, WidthAndHeight} ->
            WidthAndHeight;
        {error, _Reason} ->
            error
    end.

get_unicode_state(Input) ->
    case prim_tty:unicode(Input) of
	{ok, Bool} when is_boolean(Bool) ->
            Bool;
	{error, _Reason} ->
	    error
    end.

set_unicode_state(Input, Bool) ->
    case prim_tty:unicode(Input, Bool) of
	{ok, utf8} ->
	    {unicode, utf8};
        {ok, latin1} ->
	    {unicode, false};
	{error, _Reason} ->
	    error
    end.

%% io_request(Request, InPort, OutPort)
%% io_requests(Requests, InPort, OutPort)
%% Note: InPort is unused.
io_request({requests,Rs}, Input, Output) ->
    io_requests(Rs, Input, Output);
io_request(Request, _Input, Output) ->
    case io_command(Request) of
        {Func, Data, Reply} ->
            ok = prim_tty:Func(Output, Data),
            Reply;
        {Func, Reply} ->
            ok = prim_tty:Func(Output),
            Reply;
        unhandled ->
            ok
    end.

io_requests([{put_chars,Encoding,PC1},{put_chars,Encoding,PC2}|Rs], Input, Output) ->
    io_requests([{put_chars,Encoding,[PC1,PC2]}|Rs], Input, Output);
io_requests([R|Rs], Input, Output) ->
    io_request(R, Input, Output),
    io_requests(Rs, Input, Output);
io_requests([], _Input, _Output) ->
    ok.

%% put_int16(N, Tail) ->
%%     [(N bsr 8)band 255,N band 255|Tail].

%% When a put_chars_sync command is used, user_drv guarantees that
%% the bytes have been put in the buffer of the port before an acknowledgement
%% is sent back to the process sending the request. This command was added in
%% OTP 18 to make sure that data sent from io:format is actually printed
%% to the console before the vm stops when calling erlang:halt(integer()).
-dialyzer({no_improper_lists, io_command/1}).
io_command({put_chars_sync, unicode,Cs,Reply}) ->
    {putc_sync, unicode:characters_to_binary(Cs,utf8), Reply};
io_command({put_chars, unicode,Cs}) ->
    {putc,unicode:characters_to_binary(Cs,utf8), ok};
io_command({move_rel,N}) ->
    {move, N, ok};
io_command({insert_chars,unicode,Cs}) ->
    {insert, unicode:characters_to_binary(Cs,utf8), ok};
io_command({delete_chars,N}) ->
    {delete,N, ok};
io_command(beep) ->
    {beep, ok};
io_command(_) ->
    unhandled.

%% gr_new()
%% gr_get_num(Group, Index)
%% gr_get_info(Group, Pid)
%% gr_add_cur(Group, Pid, Shell)
%% gr_set_cur(Group, Index)
%% gr_cur_pid(Group)
%% gr_del_pid(Group, Pid)
%%  Manage the group list. The group structure has the form:
%%	{NextIndex,CurrIndex,CurrPid,GroupList}
%%
%%  where each element in the group list is:
%%	{Index,GroupPid,Shell}

gr_new() ->
    {0,0,none,[]}.

gr_get_num({_Next,_CurI,_CurP,Gs}, I) ->
    gr_get_num1(Gs, I).

gr_get_num1([{I,_Pid,{}}|_Gs], I) ->
    undefined;
gr_get_num1([{I,Pid,_S}|_Gs], I) ->
    {pid,Pid};
gr_get_num1([_G|Gs], I) ->
    gr_get_num1(Gs, I);
gr_get_num1([], _I) ->
    undefined.

gr_get_info({_Next,_CurI,_CurP,Gs}, Pid) ->
    gr_get_info1(Gs, Pid).

gr_get_info1([{I,Pid,S}|_Gs], Pid) ->
    {I,S};
gr_get_info1([_G|Gs], I) ->
    gr_get_info1(Gs, I);
gr_get_info1([], _I) ->
    undefined.

gr_add_cur({Next,_CurI,_CurP,Gs}, Pid, Shell) ->
    {Next+1,Next,Pid,append(Gs, [{Next,Pid,Shell}])}.

gr_set_cur({Next,_CurI,_CurP,Gs}, I) ->
    case gr_get_num1(Gs, I) of
	{pid,Pid} -> {ok,{Next,I,Pid,Gs}};
	undefined -> undefined
    end.

gr_set_num({Next,CurI,CurP,Gs}, I, Pid, Shell) ->
    {Next,CurI,CurP,gr_set_num1(Gs, I, Pid, Shell)}.

gr_set_num1([{I,_Pid,_Shell}|Gs], I, NewPid, NewShell) ->
    [{I,NewPid,NewShell}|Gs];
gr_set_num1([{I,Pid,Shell}|Gs], NewI, NewPid, NewShell) when NewI > I ->
    [{I,Pid,Shell}|gr_set_num1(Gs, NewI, NewPid, NewShell)];
gr_set_num1(Gs, NewI, NewPid, NewShell) ->
    [{NewI,NewPid,NewShell}|Gs].

gr_del_pid({Next,CurI,CurP,Gs}, Pid) ->
    {Next,CurI,CurP,gr_del_pid1(Gs, Pid)}.

gr_del_pid1([{_I,Pid,_S}|Gs], Pid) ->
    Gs;
gr_del_pid1([G|Gs], Pid) ->
    [G|gr_del_pid1(Gs, Pid)];
gr_del_pid1([], _Pid) ->
    [].

gr_cur_pid({_Next,_CurI,CurP,_Gs}) ->
    CurP.

gr_list({_Next,CurI,_CurP,Gs}) ->
    gr_list(Gs, CurI, []).

gr_list([{_I,_Pid,{}}|Gs], Cur, Jobs) ->
    gr_list(Gs, Cur, Jobs);
gr_list([{Cur,_Pid,Shell}|Gs], Cur, Jobs) ->
    gr_list(Gs, Cur, [{put_chars, unicode,flatten(io_lib:format("~4w* ~w\n", [Cur,Shell]))}|Jobs]);
gr_list([{I,_Pid,Shell}|Gs], Cur, Jobs) ->
    gr_list(Gs, Cur, [{put_chars, unicode,flatten(io_lib:format("~4w  ~w\n", [I,Shell]))}|Jobs]);
gr_list([], _Cur, Jobs) ->
    lists:reverse(Jobs).

append([H|T], X) ->
    [H|append(T, X)];
append([], X) ->
    X.

member(X, [X|_Rest]) -> true;
member(X, [_H|Rest]) ->
    member(X, Rest);
member(_X, []) -> false.

flatten(List) ->
    flatten(List, [], []).

flatten([H|T], Cont, Tail) when is_list(H) ->
    flatten(H, [T|Cont], Tail);
flatten([H|T], Cont, Tail) ->
    [H|flatten(T, Cont, Tail)];
flatten([], [H|Cont], Tail) ->
    flatten(H, Cont, Tail);
flatten([], [], Tail) ->
    Tail.
