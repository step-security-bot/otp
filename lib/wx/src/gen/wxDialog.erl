%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2020. All Rights Reserved.
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
%% This file is generated DO NOT EDIT

-module(wxDialog).
-moduledoc """
Functions for wxDialog class

A dialog box is a window with a title bar and sometimes a system menu, which can be moved around the screen. It can contain controls and other windows and is often used to allow the user to make some choice or to answer a question.

Dialogs can be made scrollable, automatically, for computers with low resolution screens: please see overview_dialog_autoscrolling for further details.

Dialogs usually contain either a single button allowing to close the dialog or two buttons, one accepting the changes and the other one discarding them (such button, if present, is automatically activated if the user presses the "Esc" key). By default, buttons with the standard wxID_OK and wxID_CANCEL identifiers behave as expected. Starting with wxWidgets 2.7 it is also possible to use a button with a different identifier instead, see `setAffirmativeId/2` and `SetEscapeId()` (not implemented in wx).

Also notice that the `createButtonSizer/2` should be used to create the buttons appropriate for the current platform and positioned correctly (including their order which is platform-dependent).

Modal and Modeless

There are two kinds of dialog, modal and modeless. A modal dialog blocks program flow and user input on other windows until it is dismissed, whereas a modeless dialog behaves more like a frame in that program flow continues, and input in other windows is still possible. To show a modal dialog you should use the `showModal/1` method while to show a dialog modelessly you simply use `show/2`, just as with frames.

Note that the modal dialog is one of the very few examples of wxWindow-derived objects which may be created on the stack and not on the heap. In other words, while most windows would be created like this:

You can achieve the same result with dialogs by using simpler code:

An application can define a `m:wxCloseEvent` handler for the dialog to respond to system close events.

Styles

This class supports the following styles:

See: [Overview dialog](https://docs.wxwidgets.org/3.1/overview_dialog.html#overview_dialog), `m:wxFrame`, [Overview validator](https://docs.wxwidgets.org/3.1/overview_validator.html#overview_validator)

This class is derived (and can use functions) from: `m:wxTopLevelWindow` `m:wxWindow` `m:wxEvtHandler`

wxWidgets docs: [wxDialog](https://docs.wxwidgets.org/3.1/classwx_dialog.html)

## Events

Event types emitted from this class: [`close_window`](`m:wxCloseEvent`), [`init_dialog`](`m:wxInitDialogEvent`)
""".
-include("wxe.hrl").
-export([create/4,create/5,createButtonSizer/2,createStdDialogButtonSizer/2,
  destroy/1,endModal/2,getAffirmativeId/1,getReturnCode/1,isModal/1,
  new/0,new/3,new/4,setAffirmativeId/2,setReturnCode/2,show/1,show/2,showModal/1]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centerOnScreen/1,centerOnScreen/2,
  centre/1,centre/2,centreOnParent/1,centreOnParent/2,centreOnScreen/1,
  centreOnScreen/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,enable/1,enable/2,findWindow/2,fit/1,fitInside/1,
  freeze/1,getAcceleratorTable/1,getBackgroundColour/1,getBackgroundStyle/1,
  getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,
  getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,getCursor/1,
  getDPI/1,getDPIScaleFactor/1,getDropTarget/1,getExtraStyle/1,getFont/1,
  getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getIcon/1,getIcons/1,getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,
  getParent/1,getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,
  getScrollPos/2,getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,
  getTextExtent/2,getTextExtent/3,getThemeEnabled/1,getTitle/1,getToolTip/1,
  getUpdateRegion/1,getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,
  hasCapture/1,hasScrollbar/2,hasTransparentBackground/1,hide/1,iconize/1,
  iconize/2,inheritAttributes/1,initDialog/1,invalidateBestSize/1,isActive/1,
  isDoubleBuffered/1,isEnabled/1,isExposed/2,isExposed/3,isExposed/5,
  isFrozen/1,isFullScreen/1,isIconized/1,isMaximized/1,isRetained/1,
  isShown/1,isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,
  lower/1,maximize/1,maximize/2,move/2,move/3,move/4,moveAfterInTabOrder/2,
  moveBeforeInTabOrder/2,navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,
  popupMenu/2,popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,
  refreshRect/3,releaseMouse/1,removeChild/2,reparent/2,requestUserAttention/1,
  requestUserAttention/2,screenToClient/1,screenToClient/2,scrollLines/2,
  scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setIcon/2,setIcons/2,
  setId/2,setLabel/2,setMaxSize/2,setMinSize/2,setName/2,setOwnBackgroundColour/2,
  setOwnFont/2,setOwnForegroundColour/2,setPalette/2,setScrollPos/3,
  setScrollPos/4,setScrollbar/5,setScrollbar/6,setShape/2,setSize/2,
  setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,setSizeHints/4,
  setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,setThemeEnabled/2,
  setTitle/2,setToolTip/2,setTransparent/2,setVirtualSize/2,setVirtualSize/3,
  setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,shouldInheritColours/1,
  showFullScreen/2,showFullScreen/3,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-doc "".
-type wxDialog() :: wx:wx_object().
-export_type([wxDialog/0]).
%% @hidden
parent_class(wxTopLevelWindow) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogwxdialog">external documentation</a>.
-doc "Default constructor.".
-spec new() -> wxDialog().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxDialog_new_0),
  wxe_util:rec(?wxDialog_new_0).

%% @equiv new(Parent,Id,Title, [])
-doc "".
-spec new(Parent, Id, Title) -> wxDialog() when
	Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata().

new(Parent,Id,Title)
 when is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Title) ->
  new(Parent,Id,Title, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogwxdialog">external documentation</a>.
-doc """
Constructor.

See: `create/5`
""".
-spec new(Parent, Id, Title, [Option]) -> wxDialog() when
	Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent,Id,Title, Options)
 when is_integer(Id),?is_chardata(Title),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  Title_UC = unicode:characters_to_binary(Title),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id,Title_UC, Opts,?get_env(),?wxDialog_new_4),
  wxe_util:rec(?wxDialog_new_4).

%% @equiv create(This,Parent,Id,Title, [])
-doc "".
-spec create(This, Parent, Id, Title) -> boolean() when
	This::wxDialog(), Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata().

create(This,Parent,Id,Title)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id),?is_chardata(Title) ->
  create(This,Parent,Id,Title, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogcreate">external documentation</a>.
-doc """
Used for two-step dialog box construction.

See: `new/4`
""".
-spec create(This, Parent, Id, Title, [Option]) -> boolean() when
	This::wxDialog(), Parent::wxWindow:wxWindow(), Id::integer(), Title::unicode:chardata(),
	Option :: {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id,Title, Options)
 when is_integer(Id),?is_chardata(Title),is_list(Options) ->
  ?CLASS(ThisT,wxDialog),
  ?CLASS(ParentT,wxWindow),
  Title_UC = unicode:characters_to_binary(Title),
  MOpts = fun({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id,Title_UC, Opts,?get_env(),?wxDialog_Create),
  wxe_util:rec(?wxDialog_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogcreatebuttonsizer">external documentation</a>.
-doc """
Creates a sizer with standard buttons.

`flags` is a bit list of the following flags: wxOK, wxCANCEL, wxYES, wxNO, wxAPPLY, wxCLOSE, wxHELP, wxNO_DEFAULT.

The sizer lays out the buttons in a manner appropriate to the platform.

This function uses `createStdDialogButtonSizer/2` internally for most platforms but doesn't create the sizer at all for the platforms with hardware buttons (such as smartphones) for which it sets up the hardware buttons appropriately and returns NULL, so don't forget to test that the return value is valid before using it.
""".
-spec createButtonSizer(This, Flags) -> wxSizer:wxSizer() when
	This::wxDialog(), Flags::integer().
createButtonSizer(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxDialog),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxDialog_CreateButtonSizer),
  wxe_util:rec(?wxDialog_CreateButtonSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogcreatestddialogbuttonsizer">external documentation</a>.
-doc """
Creates a `m:wxStdDialogButtonSizer` with standard buttons.

`flags` is a bit list of the following flags: wxOK, wxCANCEL, wxYES, wxNO, wxAPPLY, wxCLOSE, wxHELP, wxNO_DEFAULT.

The sizer lays out the buttons in a manner appropriate to the platform.
""".
-spec createStdDialogButtonSizer(This, Flags) -> wxStdDialogButtonSizer:wxStdDialogButtonSizer() when
	This::wxDialog(), Flags::integer().
createStdDialogButtonSizer(#wx_ref{type=ThisT}=This,Flags)
 when is_integer(Flags) ->
  ?CLASS(ThisT,wxDialog),
  wxe_util:queue_cmd(This,Flags,?get_env(),?wxDialog_CreateStdDialogButtonSizer),
  wxe_util:rec(?wxDialog_CreateStdDialogButtonSizer).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogendmodal">external documentation</a>.
-doc """
Ends a modal dialog, passing a value to be returned from the `showModal/1` invocation.

See: `showModal/1`, `getReturnCode/1`, `setReturnCode/2`
""".
-spec endModal(This, RetCode) -> 'ok' when
	This::wxDialog(), RetCode::integer().
endModal(#wx_ref{type=ThisT}=This,RetCode)
 when is_integer(RetCode) ->
  ?CLASS(ThisT,wxDialog),
  wxe_util:queue_cmd(This,RetCode,?get_env(),?wxDialog_EndModal).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialoggetaffirmativeid">external documentation</a>.
-doc """
Gets the identifier of the button which works like standard OK button in this dialog.

See: `setAffirmativeId/2`
""".
-spec getAffirmativeId(This) -> integer() when
	This::wxDialog().
getAffirmativeId(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxDialog_GetAffirmativeId),
  wxe_util:rec(?wxDialog_GetAffirmativeId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialoggetreturncode">external documentation</a>.
-doc """
Gets the return code for this window.

Remark: A return code is normally associated with a modal dialog, where `showModal/1` returns a code to the application.

See: `setReturnCode/2`, `showModal/1`, `endModal/2`
""".
-spec getReturnCode(This) -> integer() when
	This::wxDialog().
getReturnCode(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxDialog_GetReturnCode),
  wxe_util:rec(?wxDialog_GetReturnCode).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogismodal">external documentation</a>.
-doc "Returns true if the dialog box is modal, false otherwise.".
-spec isModal(This) -> boolean() when
	This::wxDialog().
isModal(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxDialog_IsModal),
  wxe_util:rec(?wxDialog_IsModal).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogsetaffirmativeid">external documentation</a>.
-doc """
Sets the identifier to be used as OK button.

When the button with this identifier is pressed, the dialog calls `wxWindow:validate/1` and `wxWindow:transferDataFromWindow/1` and, if they both return true, closes the dialog with the affirmative id return code.

Also, when the user presses a hardware OK button on the devices having one or the special OK button in the PocketPC title bar, an event with this id is generated.

By default, the affirmative id is wxID_OK.

See: `getAffirmativeId/1`, `SetEscapeId()` (not implemented in wx)
""".
-spec setAffirmativeId(This, Id) -> 'ok' when
	This::wxDialog(), Id::integer().
setAffirmativeId(#wx_ref{type=ThisT}=This,Id)
 when is_integer(Id) ->
  ?CLASS(ThisT,wxDialog),
  wxe_util:queue_cmd(This,Id,?get_env(),?wxDialog_SetAffirmativeId).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogsetreturncode">external documentation</a>.
-doc """
Sets the return code for this window.

A return code is normally associated with a modal dialog, where `showModal/1` returns a code to the application. The function `endModal/2` calls `setReturnCode/2`.

See: `getReturnCode/1`, `showModal/1`, `endModal/2`
""".
-spec setReturnCode(This, RetCode) -> 'ok' when
	This::wxDialog(), RetCode::integer().
setReturnCode(#wx_ref{type=ThisT}=This,RetCode)
 when is_integer(RetCode) ->
  ?CLASS(ThisT,wxDialog),
  wxe_util:queue_cmd(This,RetCode,?get_env(),?wxDialog_SetReturnCode).

%% @equiv show(This, [])
-doc "".
-spec show(This) -> boolean() when
	This::wxDialog().

show(This)
 when is_record(This, wx_ref) ->
  show(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogshow">external documentation</a>.
-doc """
Hides or shows the dialog.

The preferred way of dismissing a modal dialog is to use `endModal/2`.
""".
-spec show(This, [Option]) -> boolean() when
	This::wxDialog(),
	Option :: {'show', boolean()}.
show(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxDialog),
  MOpts = fun({show, _show} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxDialog_Show),
  wxe_util:rec(?wxDialog_Show).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxdialog.html#wxdialogshowmodal">external documentation</a>.
-doc """
Shows an application-modal dialog.

Program flow does not return until the dialog has been dismissed with `endModal/2`.

Notice that it is possible to call `showModal/1` for a dialog which had been previously shown with `show/2`, this allows making an existing modeless dialog modal. However `showModal/1` can't be called twice without intervening `endModal/2` calls.

Note that this function creates a temporary event loop which takes precedence over the application's main event loop (see `wxEventLoopBase` (not implemented in wx)) and which is destroyed when the dialog is dismissed. This also results in a call to `wxApp::ProcessPendingEvents()` (not implemented in wx).

Return: The value set with `setReturnCode/2`.

See: `ShowWindowModal()` (not implemented in wx), `ShowWindowModalThenDo()` (not implemented in wx), `endModal/2`, `getReturnCode/1`, `setReturnCode/2`
""".
-spec showModal(This) -> integer() when
	This::wxDialog().
showModal(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxDialog),
  wxe_util:queue_cmd(This,?get_env(),?wxDialog_ShowModal),
  wxe_util:rec(?wxDialog_ShowModal).

%% @doc Destroys this object, do not use object again
-doc """
Destructor.

Deletes any child windows before deleting the physical window.

See overview_windowdeletion for more info.
""".
-spec destroy(This::wxDialog()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxDialog),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxTopLevelWindow
%% @hidden
showFullScreen(This,Show, Options) -> wxTopLevelWindow:showFullScreen(This,Show, Options).
%% @hidden
showFullScreen(This,Show) -> wxTopLevelWindow:showFullScreen(This,Show).
%% @hidden
setTitle(This,Title) -> wxTopLevelWindow:setTitle(This,Title).
%% @hidden
setShape(This,Region) -> wxTopLevelWindow:setShape(This,Region).
%% @hidden
centreOnScreen(This, Options) -> wxTopLevelWindow:centreOnScreen(This, Options).
%% @hidden
centerOnScreen(This, Options) -> wxTopLevelWindow:centerOnScreen(This, Options).
%% @hidden
centreOnScreen(This) -> wxTopLevelWindow:centreOnScreen(This).
%% @hidden
centerOnScreen(This) -> wxTopLevelWindow:centerOnScreen(This).
%% @hidden
setIcons(This,Icons) -> wxTopLevelWindow:setIcons(This,Icons).
%% @hidden
setIcon(This,Icon) -> wxTopLevelWindow:setIcon(This,Icon).
%% @hidden
requestUserAttention(This, Options) -> wxTopLevelWindow:requestUserAttention(This, Options).
%% @hidden
requestUserAttention(This) -> wxTopLevelWindow:requestUserAttention(This).
%% @hidden
maximize(This, Options) -> wxTopLevelWindow:maximize(This, Options).
%% @hidden
maximize(This) -> wxTopLevelWindow:maximize(This).
%% @hidden
isMaximized(This) -> wxTopLevelWindow:isMaximized(This).
%% @hidden
isIconized(This) -> wxTopLevelWindow:isIconized(This).
%% @hidden
isFullScreen(This) -> wxTopLevelWindow:isFullScreen(This).
%% @hidden
iconize(This, Options) -> wxTopLevelWindow:iconize(This, Options).
%% @hidden
iconize(This) -> wxTopLevelWindow:iconize(This).
%% @hidden
isActive(This) -> wxTopLevelWindow:isActive(This).
%% @hidden
getTitle(This) -> wxTopLevelWindow:getTitle(This).
%% @hidden
getIcons(This) -> wxTopLevelWindow:getIcons(This).
%% @hidden
getIcon(This) -> wxTopLevelWindow:getIcon(This).
 %% From wxWindow
%% @hidden
getDPI(This) -> wxWindow:getDPI(This).
%% @hidden
getContentScaleFactor(This) -> wxWindow:getContentScaleFactor(This).
%% @hidden
setDoubleBuffered(This,On) -> wxWindow:setDoubleBuffered(This,On).
%% @hidden
isDoubleBuffered(This) -> wxWindow:isDoubleBuffered(This).
%% @hidden
canSetTransparent(This) -> wxWindow:canSetTransparent(This).
%% @hidden
setTransparent(This,Alpha) -> wxWindow:setTransparent(This,Alpha).
%% @hidden
warpPointer(This,X,Y) -> wxWindow:warpPointer(This,X,Y).
%% @hidden
validate(This) -> wxWindow:validate(This).
%% @hidden
updateWindowUI(This, Options) -> wxWindow:updateWindowUI(This, Options).
%% @hidden
updateWindowUI(This) -> wxWindow:updateWindowUI(This).
%% @hidden
update(This) -> wxWindow:update(This).
%% @hidden
transferDataToWindow(This) -> wxWindow:transferDataToWindow(This).
%% @hidden
transferDataFromWindow(This) -> wxWindow:transferDataFromWindow(This).
%% @hidden
thaw(This) -> wxWindow:thaw(This).
%% @hidden
shouldInheritColours(This) -> wxWindow:shouldInheritColours(This).
%% @hidden
setWindowVariant(This,Variant) -> wxWindow:setWindowVariant(This,Variant).
%% @hidden
setWindowStyleFlag(This,Style) -> wxWindow:setWindowStyleFlag(This,Style).
%% @hidden
setWindowStyle(This,Style) -> wxWindow:setWindowStyle(This,Style).
%% @hidden
setVirtualSize(This,Width,Height) -> wxWindow:setVirtualSize(This,Width,Height).
%% @hidden
setVirtualSize(This,Size) -> wxWindow:setVirtualSize(This,Size).
%% @hidden
setToolTip(This,TipString) -> wxWindow:setToolTip(This,TipString).
%% @hidden
setThemeEnabled(This,Enable) -> wxWindow:setThemeEnabled(This,Enable).
%% @hidden
setSizerAndFit(This,Sizer, Options) -> wxWindow:setSizerAndFit(This,Sizer, Options).
%% @hidden
setSizerAndFit(This,Sizer) -> wxWindow:setSizerAndFit(This,Sizer).
%% @hidden
setSizer(This,Sizer, Options) -> wxWindow:setSizer(This,Sizer, Options).
%% @hidden
setSizer(This,Sizer) -> wxWindow:setSizer(This,Sizer).
%% @hidden
setSizeHints(This,MinW,MinH, Options) -> wxWindow:setSizeHints(This,MinW,MinH, Options).
%% @hidden
setSizeHints(This,MinW,MinH) -> wxWindow:setSizeHints(This,MinW,MinH).
%% @hidden
setSizeHints(This,MinSize) -> wxWindow:setSizeHints(This,MinSize).
%% @hidden
setSize(This,X,Y,Width,Height, Options) -> wxWindow:setSize(This,X,Y,Width,Height, Options).
%% @hidden
setSize(This,X,Y,Width,Height) -> wxWindow:setSize(This,X,Y,Width,Height).
%% @hidden
setSize(This,Width,Height) -> wxWindow:setSize(This,Width,Height).
%% @hidden
setSize(This,Rect) -> wxWindow:setSize(This,Rect).
%% @hidden
setScrollPos(This,Orientation,Pos, Options) -> wxWindow:setScrollPos(This,Orientation,Pos, Options).
%% @hidden
setScrollPos(This,Orientation,Pos) -> wxWindow:setScrollPos(This,Orientation,Pos).
%% @hidden
setScrollbar(This,Orientation,Position,ThumbSize,Range, Options) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range, Options).
%% @hidden
setScrollbar(This,Orientation,Position,ThumbSize,Range) -> wxWindow:setScrollbar(This,Orientation,Position,ThumbSize,Range).
%% @hidden
setPalette(This,Pal) -> wxWindow:setPalette(This,Pal).
%% @hidden
setName(This,Name) -> wxWindow:setName(This,Name).
%% @hidden
setLabel(This,Label) -> wxWindow:setLabel(This,Label).
%% @hidden
setId(This,Winid) -> wxWindow:setId(This,Winid).
%% @hidden
setHelpText(This,HelpText) -> wxWindow:setHelpText(This,HelpText).
%% @hidden
setForegroundColour(This,Colour) -> wxWindow:setForegroundColour(This,Colour).
%% @hidden
setFont(This,Font) -> wxWindow:setFont(This,Font).
%% @hidden
setFocusFromKbd(This) -> wxWindow:setFocusFromKbd(This).
%% @hidden
setFocus(This) -> wxWindow:setFocus(This).
%% @hidden
setExtraStyle(This,ExStyle) -> wxWindow:setExtraStyle(This,ExStyle).
%% @hidden
setDropTarget(This,Target) -> wxWindow:setDropTarget(This,Target).
%% @hidden
setOwnForegroundColour(This,Colour) -> wxWindow:setOwnForegroundColour(This,Colour).
%% @hidden
setOwnFont(This,Font) -> wxWindow:setOwnFont(This,Font).
%% @hidden
setOwnBackgroundColour(This,Colour) -> wxWindow:setOwnBackgroundColour(This,Colour).
%% @hidden
setMinSize(This,Size) -> wxWindow:setMinSize(This,Size).
%% @hidden
setMaxSize(This,Size) -> wxWindow:setMaxSize(This,Size).
%% @hidden
setCursor(This,Cursor) -> wxWindow:setCursor(This,Cursor).
%% @hidden
setContainingSizer(This,Sizer) -> wxWindow:setContainingSizer(This,Sizer).
%% @hidden
setClientSize(This,Width,Height) -> wxWindow:setClientSize(This,Width,Height).
%% @hidden
setClientSize(This,Size) -> wxWindow:setClientSize(This,Size).
%% @hidden
setCaret(This,Caret) -> wxWindow:setCaret(This,Caret).
%% @hidden
setBackgroundStyle(This,Style) -> wxWindow:setBackgroundStyle(This,Style).
%% @hidden
setBackgroundColour(This,Colour) -> wxWindow:setBackgroundColour(This,Colour).
%% @hidden
setAutoLayout(This,AutoLayout) -> wxWindow:setAutoLayout(This,AutoLayout).
%% @hidden
setAcceleratorTable(This,Accel) -> wxWindow:setAcceleratorTable(This,Accel).
%% @hidden
scrollWindow(This,Dx,Dy, Options) -> wxWindow:scrollWindow(This,Dx,Dy, Options).
%% @hidden
scrollWindow(This,Dx,Dy) -> wxWindow:scrollWindow(This,Dx,Dy).
%% @hidden
scrollPages(This,Pages) -> wxWindow:scrollPages(This,Pages).
%% @hidden
scrollLines(This,Lines) -> wxWindow:scrollLines(This,Lines).
%% @hidden
screenToClient(This,Pt) -> wxWindow:screenToClient(This,Pt).
%% @hidden
screenToClient(This) -> wxWindow:screenToClient(This).
%% @hidden
reparent(This,NewParent) -> wxWindow:reparent(This,NewParent).
%% @hidden
removeChild(This,Child) -> wxWindow:removeChild(This,Child).
%% @hidden
releaseMouse(This) -> wxWindow:releaseMouse(This).
%% @hidden
refreshRect(This,Rect, Options) -> wxWindow:refreshRect(This,Rect, Options).
%% @hidden
refreshRect(This,Rect) -> wxWindow:refreshRect(This,Rect).
%% @hidden
refresh(This, Options) -> wxWindow:refresh(This, Options).
%% @hidden
refresh(This) -> wxWindow:refresh(This).
%% @hidden
raise(This) -> wxWindow:raise(This).
%% @hidden
popupMenu(This,Menu,X,Y) -> wxWindow:popupMenu(This,Menu,X,Y).
%% @hidden
popupMenu(This,Menu, Options) -> wxWindow:popupMenu(This,Menu, Options).
%% @hidden
popupMenu(This,Menu) -> wxWindow:popupMenu(This,Menu).
%% @hidden
pageUp(This) -> wxWindow:pageUp(This).
%% @hidden
pageDown(This) -> wxWindow:pageDown(This).
%% @hidden
navigate(This, Options) -> wxWindow:navigate(This, Options).
%% @hidden
navigate(This) -> wxWindow:navigate(This).
%% @hidden
moveBeforeInTabOrder(This,Win) -> wxWindow:moveBeforeInTabOrder(This,Win).
%% @hidden
moveAfterInTabOrder(This,Win) -> wxWindow:moveAfterInTabOrder(This,Win).
%% @hidden
move(This,X,Y, Options) -> wxWindow:move(This,X,Y, Options).
%% @hidden
move(This,X,Y) -> wxWindow:move(This,X,Y).
%% @hidden
move(This,Pt) -> wxWindow:move(This,Pt).
%% @hidden
lower(This) -> wxWindow:lower(This).
%% @hidden
lineUp(This) -> wxWindow:lineUp(This).
%% @hidden
lineDown(This) -> wxWindow:lineDown(This).
%% @hidden
layout(This) -> wxWindow:layout(This).
%% @hidden
isShownOnScreen(This) -> wxWindow:isShownOnScreen(This).
%% @hidden
isTopLevel(This) -> wxWindow:isTopLevel(This).
%% @hidden
isShown(This) -> wxWindow:isShown(This).
%% @hidden
isRetained(This) -> wxWindow:isRetained(This).
%% @hidden
isExposed(This,X,Y,W,H) -> wxWindow:isExposed(This,X,Y,W,H).
%% @hidden
isExposed(This,X,Y) -> wxWindow:isExposed(This,X,Y).
%% @hidden
isExposed(This,Pt) -> wxWindow:isExposed(This,Pt).
%% @hidden
isEnabled(This) -> wxWindow:isEnabled(This).
%% @hidden
isFrozen(This) -> wxWindow:isFrozen(This).
%% @hidden
invalidateBestSize(This) -> wxWindow:invalidateBestSize(This).
%% @hidden
initDialog(This) -> wxWindow:initDialog(This).
%% @hidden
inheritAttributes(This) -> wxWindow:inheritAttributes(This).
%% @hidden
hide(This) -> wxWindow:hide(This).
%% @hidden
hasTransparentBackground(This) -> wxWindow:hasTransparentBackground(This).
%% @hidden
hasScrollbar(This,Orient) -> wxWindow:hasScrollbar(This,Orient).
%% @hidden
hasCapture(This) -> wxWindow:hasCapture(This).
%% @hidden
getWindowVariant(This) -> wxWindow:getWindowVariant(This).
%% @hidden
getWindowStyleFlag(This) -> wxWindow:getWindowStyleFlag(This).
%% @hidden
getVirtualSize(This) -> wxWindow:getVirtualSize(This).
%% @hidden
getUpdateRegion(This) -> wxWindow:getUpdateRegion(This).
%% @hidden
getToolTip(This) -> wxWindow:getToolTip(This).
%% @hidden
getThemeEnabled(This) -> wxWindow:getThemeEnabled(This).
%% @hidden
getTextExtent(This,String, Options) -> wxWindow:getTextExtent(This,String, Options).
%% @hidden
getTextExtent(This,String) -> wxWindow:getTextExtent(This,String).
%% @hidden
getSizer(This) -> wxWindow:getSizer(This).
%% @hidden
getSize(This) -> wxWindow:getSize(This).
%% @hidden
getScrollThumb(This,Orientation) -> wxWindow:getScrollThumb(This,Orientation).
%% @hidden
getScrollRange(This,Orientation) -> wxWindow:getScrollRange(This,Orientation).
%% @hidden
getScrollPos(This,Orientation) -> wxWindow:getScrollPos(This,Orientation).
%% @hidden
getScreenRect(This) -> wxWindow:getScreenRect(This).
%% @hidden
getScreenPosition(This) -> wxWindow:getScreenPosition(This).
%% @hidden
getRect(This) -> wxWindow:getRect(This).
%% @hidden
getPosition(This) -> wxWindow:getPosition(This).
%% @hidden
getParent(This) -> wxWindow:getParent(This).
%% @hidden
getName(This) -> wxWindow:getName(This).
%% @hidden
getMinSize(This) -> wxWindow:getMinSize(This).
%% @hidden
getMaxSize(This) -> wxWindow:getMaxSize(This).
%% @hidden
getLabel(This) -> wxWindow:getLabel(This).
%% @hidden
getId(This) -> wxWindow:getId(This).
%% @hidden
getHelpText(This) -> wxWindow:getHelpText(This).
%% @hidden
getHandle(This) -> wxWindow:getHandle(This).
%% @hidden
getGrandParent(This) -> wxWindow:getGrandParent(This).
%% @hidden
getForegroundColour(This) -> wxWindow:getForegroundColour(This).
%% @hidden
getFont(This) -> wxWindow:getFont(This).
%% @hidden
getExtraStyle(This) -> wxWindow:getExtraStyle(This).
%% @hidden
getDPIScaleFactor(This) -> wxWindow:getDPIScaleFactor(This).
%% @hidden
getDropTarget(This) -> wxWindow:getDropTarget(This).
%% @hidden
getCursor(This) -> wxWindow:getCursor(This).
%% @hidden
getContainingSizer(This) -> wxWindow:getContainingSizer(This).
%% @hidden
getClientSize(This) -> wxWindow:getClientSize(This).
%% @hidden
getChildren(This) -> wxWindow:getChildren(This).
%% @hidden
getCharWidth(This) -> wxWindow:getCharWidth(This).
%% @hidden
getCharHeight(This) -> wxWindow:getCharHeight(This).
%% @hidden
getCaret(This) -> wxWindow:getCaret(This).
%% @hidden
getBestSize(This) -> wxWindow:getBestSize(This).
%% @hidden
getBackgroundStyle(This) -> wxWindow:getBackgroundStyle(This).
%% @hidden
getBackgroundColour(This) -> wxWindow:getBackgroundColour(This).
%% @hidden
getAcceleratorTable(This) -> wxWindow:getAcceleratorTable(This).
%% @hidden
freeze(This) -> wxWindow:freeze(This).
%% @hidden
fitInside(This) -> wxWindow:fitInside(This).
%% @hidden
fit(This) -> wxWindow:fit(This).
%% @hidden
findWindow(This,Id) -> wxWindow:findWindow(This,Id).
%% @hidden
enable(This, Options) -> wxWindow:enable(This, Options).
%% @hidden
enable(This) -> wxWindow:enable(This).
%% @hidden
dragAcceptFiles(This,Accept) -> wxWindow:dragAcceptFiles(This,Accept).
%% @hidden
disable(This) -> wxWindow:disable(This).
%% @hidden
destroyChildren(This) -> wxWindow:destroyChildren(This).
%% @hidden
convertPixelsToDialog(This,Sz) -> wxWindow:convertPixelsToDialog(This,Sz).
%% @hidden
convertDialogToPixels(This,Sz) -> wxWindow:convertDialogToPixels(This,Sz).
%% @hidden
close(This, Options) -> wxWindow:close(This, Options).
%% @hidden
close(This) -> wxWindow:close(This).
%% @hidden
clientToScreen(This,X,Y) -> wxWindow:clientToScreen(This,X,Y).
%% @hidden
clientToScreen(This,Pt) -> wxWindow:clientToScreen(This,Pt).
%% @hidden
clearBackground(This) -> wxWindow:clearBackground(This).
%% @hidden
centreOnParent(This, Options) -> wxWindow:centreOnParent(This, Options).
%% @hidden
centerOnParent(This, Options) -> wxWindow:centerOnParent(This, Options).
%% @hidden
centreOnParent(This) -> wxWindow:centreOnParent(This).
%% @hidden
centerOnParent(This) -> wxWindow:centerOnParent(This).
%% @hidden
centre(This, Options) -> wxWindow:centre(This, Options).
%% @hidden
center(This, Options) -> wxWindow:center(This, Options).
%% @hidden
centre(This) -> wxWindow:centre(This).
%% @hidden
center(This) -> wxWindow:center(This).
%% @hidden
captureMouse(This) -> wxWindow:captureMouse(This).
%% @hidden
cacheBestSize(This,Size) -> wxWindow:cacheBestSize(This,Size).
 %% From wxEvtHandler
%% @hidden
disconnect(This,EventType, Options) -> wxEvtHandler:disconnect(This,EventType, Options).
%% @hidden
disconnect(This,EventType) -> wxEvtHandler:disconnect(This,EventType).
%% @hidden
disconnect(This) -> wxEvtHandler:disconnect(This).
%% @hidden
connect(This,EventType, Options) -> wxEvtHandler:connect(This,EventType, Options).
%% @hidden
connect(This,EventType) -> wxEvtHandler:connect(This,EventType).

