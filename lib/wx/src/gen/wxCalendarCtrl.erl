%%
%% %CopyrightBegin%
%%
%% Copyright Ericsson AB 2008-2021. All Rights Reserved.
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

-module(wxCalendarCtrl).
-moduledoc """
Functions for wxCalendarCtrl class

The calendar control allows the user to pick a date. The user can move the current selection using the keyboard and select the date (generating `EVT_CALENDAR` event) by pressing `<Return>` or double clicking it.

Generic calendar has advanced possibilities for the customization of its display, described below. If you want to use these possibilities on every platform, use wxGenericCalendarCtrl instead of `m:wxCalendarCtrl`.

All global settings (such as colours and fonts used) can, of course, be changed. But also, the display style for each day in the month can be set independently using `m:wxCalendarDateAttr` class.

An item without custom attributes is drawn with the default colours and font and without border, but setting custom attributes with `setAttr/3` allows modifying its appearance. Just create a custom attribute object and set it for the day you want to be displayed specially (note that the control will take ownership of the pointer, i.e. it will delete it itself). A day may be marked as being a holiday, even if it is not recognized as one by [`wx_datetime()`](`t:wx:wx_datetime/0`) using the `wxCalendarDateAttr:setHoliday/2` method.

As the attributes are specified for each day, they may change when the month is changed, so you will often want to update them in `EVT_CALENDAR_PAGE_CHANGED` event handler.

If neither the `wxCAL_SUNDAY_FIRST` or `wxCAL_MONDAY_FIRST` style is given, the first day of the week is determined from operating system's settings, if possible. The native wxGTK calendar chooses the first weekday based on locale, and these styles have no effect on it.

Styles

This class supports the following styles:

Note: Changing the selected date will trigger an EVT_CALENDAR_DAY, MONTH or YEAR event as well as an EVT_CALENDAR_SEL_CHANGED event.

See: [Examples](https://docs.wxwidgets.org/3.1/page_samples.html#page_samples_calendar), `m:wxCalendarDateAttr`, `m:wxCalendarEvent`, `m:wxDatePickerCtrl`

This class is derived (and can use functions) from: `m:wxControl` `m:wxWindow` `m:wxEvtHandler`

wxWidgets docs: [wxCalendarCtrl](https://docs.wxwidgets.org/3.1/classwx_calendar_ctrl.html)

## Events

Event types emitted from this class: [`calendar_sel_changed`](`m:wxCalendarEvent`), [`calendar_weekday_clicked`](`m:wxCalendarEvent`)
""".
-include("wxe.hrl").
-export([create/3,create/4,destroy/1,enableHolidayDisplay/1,enableHolidayDisplay/2,
  enableMonthChange/1,enableMonthChange/2,enableYearChange/1,enableYearChange/2,
  getAttr/2,getDate/1,getHeaderColourBg/1,getHeaderColourFg/1,getHighlightColourBg/1,
  getHighlightColourFg/1,getHolidayColourBg/1,getHolidayColourFg/1,
  hitTest/2,new/0,new/2,new/3,resetAttr/2,setAttr/3,setDate/2,setHeaderColours/3,
  setHighlightColours/3,setHoliday/2,setHolidayColours/3]).

%% inherited exports
-export([cacheBestSize/2,canSetTransparent/1,captureMouse/1,center/1,center/2,
  centerOnParent/1,centerOnParent/2,centre/1,centre/2,centreOnParent/1,
  centreOnParent/2,clearBackground/1,clientToScreen/2,clientToScreen/3,
  close/1,close/2,connect/2,connect/3,convertDialogToPixels/2,convertPixelsToDialog/2,
  destroyChildren/1,disable/1,disconnect/1,disconnect/2,disconnect/3,
  dragAcceptFiles/2,enable/1,enable/2,findWindow/2,fit/1,fitInside/1,
  freeze/1,getAcceleratorTable/1,getBackgroundColour/1,getBackgroundStyle/1,
  getBestSize/1,getCaret/1,getCharHeight/1,getCharWidth/1,getChildren/1,
  getClientSize/1,getContainingSizer/1,getContentScaleFactor/1,getCursor/1,
  getDPI/1,getDPIScaleFactor/1,getDropTarget/1,getExtraStyle/1,getFont/1,
  getForegroundColour/1,getGrandParent/1,getHandle/1,getHelpText/1,
  getId/1,getLabel/1,getMaxSize/1,getMinSize/1,getName/1,getParent/1,
  getPosition/1,getRect/1,getScreenPosition/1,getScreenRect/1,getScrollPos/2,
  getScrollRange/2,getScrollThumb/2,getSize/1,getSizer/1,getTextExtent/2,
  getTextExtent/3,getThemeEnabled/1,getToolTip/1,getUpdateRegion/1,
  getVirtualSize/1,getWindowStyleFlag/1,getWindowVariant/1,hasCapture/1,
  hasScrollbar/2,hasTransparentBackground/1,hide/1,inheritAttributes/1,
  initDialog/1,invalidateBestSize/1,isDoubleBuffered/1,isEnabled/1,
  isExposed/2,isExposed/3,isExposed/5,isFrozen/1,isRetained/1,isShown/1,
  isShownOnScreen/1,isTopLevel/1,layout/1,lineDown/1,lineUp/1,lower/1,
  move/2,move/3,move/4,moveAfterInTabOrder/2,moveBeforeInTabOrder/2,
  navigate/1,navigate/2,pageDown/1,pageUp/1,parent_class/1,popupMenu/2,
  popupMenu/3,popupMenu/4,raise/1,refresh/1,refresh/2,refreshRect/2,refreshRect/3,
  releaseMouse/1,removeChild/2,reparent/2,screenToClient/1,screenToClient/2,
  scrollLines/2,scrollPages/2,scrollWindow/3,scrollWindow/4,setAcceleratorTable/2,
  setAutoLayout/2,setBackgroundColour/2,setBackgroundStyle/2,setCaret/2,
  setClientSize/2,setClientSize/3,setContainingSizer/2,setCursor/2,
  setDoubleBuffered/2,setDropTarget/2,setExtraStyle/2,setFocus/1,setFocusFromKbd/1,
  setFont/2,setForegroundColour/2,setHelpText/2,setId/2,setLabel/2,setMaxSize/2,
  setMinSize/2,setName/2,setOwnBackgroundColour/2,setOwnFont/2,setOwnForegroundColour/2,
  setPalette/2,setScrollPos/3,setScrollPos/4,setScrollbar/5,setScrollbar/6,
  setSize/2,setSize/3,setSize/5,setSize/6,setSizeHints/2,setSizeHints/3,
  setSizeHints/4,setSizer/2,setSizer/3,setSizerAndFit/2,setSizerAndFit/3,
  setThemeEnabled/2,setToolTip/2,setTransparent/2,setVirtualSize/2,
  setVirtualSize/3,setWindowStyle/2,setWindowStyleFlag/2,setWindowVariant/2,
  shouldInheritColours/1,show/1,show/2,thaw/1,transferDataFromWindow/1,
  transferDataToWindow/1,update/1,updateWindowUI/1,updateWindowUI/2,
  validate/1,warpPointer/3]).

-doc "".
-type wxCalendarCtrl() :: wx:wx_object().
-export_type([wxCalendarCtrl/0]).
-deprecated([{enableYearChange,1,"not available in wxWidgets-2.9 and later"},
             
  {enableYearChange,2,"not available in wxWidgets-2.9 and later"}]).

%% @hidden
parent_class(wxControl) -> true;
parent_class(wxWindow) -> true;
parent_class(wxEvtHandler) -> true;
parent_class(_Class) -> erlang:error({badtype, ?MODULE}).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlwxcalendarctrl">external documentation</a>.
-doc "Default constructor.".
-spec new() -> wxCalendarCtrl().
new() ->
  wxe_util:queue_cmd(?get_env(), ?wxCalendarCtrl_new_0),
  wxe_util:rec(?wxCalendarCtrl_new_0).

%% @equiv new(Parent,Id, [])
-doc "".
-spec new(Parent, Id) -> wxCalendarCtrl() when
	Parent::wxWindow:wxWindow(), Id::integer().

new(Parent,Id)
 when is_record(Parent, wx_ref),is_integer(Id) ->
  new(Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlwxcalendarctrl">external documentation</a>.
-doc "Does the same as `create/4` method.".
-spec new(Parent, Id, [Option]) -> wxCalendarCtrl() when
	Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'date', wx:wx_datetime()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
new(#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({date, {{DateY,DateMo,DateD},{DateH,DateMi,DateS}}}) -> {date,{DateD,DateMo,DateY,DateH,DateMi,DateS}};
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(Parent,Id, Opts,?get_env(),?wxCalendarCtrl_new_3),
  wxe_util:rec(?wxCalendarCtrl_new_3).

%% @equiv create(This,Parent,Id, [])
-doc "".
-spec create(This, Parent, Id) -> boolean() when
	This::wxCalendarCtrl(), Parent::wxWindow:wxWindow(), Id::integer().

create(This,Parent,Id)
 when is_record(This, wx_ref),is_record(Parent, wx_ref),is_integer(Id) ->
  create(This,Parent,Id, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlcreate">external documentation</a>.
-doc """
Creates the control.

See `wxWindow:new/3` for the meaning of the parameters and the control overview for the possible styles.
""".
-spec create(This, Parent, Id, [Option]) -> boolean() when
	This::wxCalendarCtrl(), Parent::wxWindow:wxWindow(), Id::integer(),
	Option :: {'date', wx:wx_datetime()}
		 | {'pos', {X::integer(), Y::integer()}}
		 | {'size', {W::integer(), H::integer()}}
		 | {'style', integer()}.
create(#wx_ref{type=ThisT}=This,#wx_ref{type=ParentT}=Parent,Id, Options)
 when is_integer(Id),is_list(Options) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  ?CLASS(ParentT,wxWindow),
  MOpts = fun({date, {{DateY,DateMo,DateD},{DateH,DateMi,DateS}}}) -> {date,{DateD,DateMo,DateY,DateH,DateMi,DateS}};
          ({pos, {_posX,_posY}} = Arg) -> Arg;
          ({size, {_sizeW,_sizeH}} = Arg) -> Arg;
          ({style, _style} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This,Parent,Id, Opts,?get_env(),?wxCalendarCtrl_Create),
  wxe_util:rec(?wxCalendarCtrl_Create).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlsetdate">external documentation</a>.
-doc """
Sets the current date.

The `date` parameter must be valid and in the currently valid range as set by `SetDateRange()` (not implemented in wx), otherwise the current date is not changed and the function returns false and, additionally, triggers an assertion failure if the date is invalid.
""".
-spec setDate(This, Date) -> boolean() when
	This::wxCalendarCtrl(), Date::wx:wx_datetime().
setDate(#wx_ref{type=ThisT}=This,{{DateY,DateMo,DateD},{DateH,DateMi,DateS}})
 when is_integer(DateD),is_integer(DateMo),is_integer(DateY),is_integer(DateH),is_integer(DateMi),is_integer(DateS) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,{DateD,DateMo,DateY,DateH,DateMi,DateS},?get_env(),?wxCalendarCtrl_SetDate),
  wxe_util:rec(?wxCalendarCtrl_SetDate).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlgetdate">external documentation</a>.
-doc "Gets the currently selected date.".
-spec getDate(This) -> wx:wx_datetime() when
	This::wxCalendarCtrl().
getDate(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarCtrl_GetDate),
  wxe_util:rec(?wxCalendarCtrl_GetDate).

%% @equiv enableYearChange(This, [])
-doc "".
-doc(#{deprecated =>
           <<"wxCalendarCtrl:enableYearChange/1 is deprecated; not available in wxWidgets-2.9 and later">>}).
-spec enableYearChange(This) -> 'ok' when
	This::wxCalendarCtrl().

enableYearChange(This)
 when is_record(This, wx_ref) ->
  enableYearChange(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlenableyearchange">external documentation</a>.
-doc """
Deprecated:

This function should be used instead of changing `wxCAL_NO_YEAR_CHANGE` style bit directly. It allows or disallows the user to change the year interactively. Only in generic `m:wxCalendarCtrl`.
""".
-doc(#{deprecated =>
           <<"wxCalendarCtrl:enableYearChange/2 is deprecated; not available in wxWidgets-2.9 and later">>}).
-spec enableYearChange(This, [Option]) -> 'ok' when
	This::wxCalendarCtrl(),
	Option :: {'enable', boolean()}.
enableYearChange(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxCalendarCtrl_EnableYearChange).

%% @equiv enableMonthChange(This, [])
-doc "".
-spec enableMonthChange(This) -> boolean() when
	This::wxCalendarCtrl().

enableMonthChange(This)
 when is_record(This, wx_ref) ->
  enableMonthChange(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlenablemonthchange">external documentation</a>.
-doc """
This function should be used instead of changing `wxCAL_NO_MONTH_CHANGE` style bit.

It allows or disallows the user to change the month interactively. Note that if the month cannot be changed, the year cannot be changed neither.

Return: true if the value of this option really changed or false if it was already set to the requested value.
""".
-spec enableMonthChange(This, [Option]) -> boolean() when
	This::wxCalendarCtrl(),
	Option :: {'enable', boolean()}.
enableMonthChange(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  MOpts = fun({enable, _enable} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxCalendarCtrl_EnableMonthChange),
  wxe_util:rec(?wxCalendarCtrl_EnableMonthChange).

%% @equiv enableHolidayDisplay(This, [])
-doc "".
-spec enableHolidayDisplay(This) -> 'ok' when
	This::wxCalendarCtrl().

enableHolidayDisplay(This)
 when is_record(This, wx_ref) ->
  enableHolidayDisplay(This, []).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlenableholidaydisplay">external documentation</a>.
-doc """
This function should be used instead of changing `wxCAL_SHOW_HOLIDAYS` style bit directly.

It enables or disables the special highlighting of the holidays.
""".
-spec enableHolidayDisplay(This, [Option]) -> 'ok' when
	This::wxCalendarCtrl(),
	Option :: {'display', boolean()}.
enableHolidayDisplay(#wx_ref{type=ThisT}=This, Options)
 when is_list(Options) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  MOpts = fun({display, _display} = Arg) -> Arg;
          (BadOpt) -> erlang:error({badoption, BadOpt}) end,
  Opts = lists:map(MOpts, Options),
  wxe_util:queue_cmd(This, Opts,?get_env(),?wxCalendarCtrl_EnableHolidayDisplay).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlsetheadercolours">external documentation</a>.
-doc """
Set the colours used for painting the weekdays at the top of the control.

This method is currently only implemented in generic `m:wxCalendarCtrl` and does nothing in the native versions.
""".
-spec setHeaderColours(This, ColFg, ColBg) -> 'ok' when
	This::wxCalendarCtrl(), ColFg::wx:wx_colour(), ColBg::wx:wx_colour().
setHeaderColours(#wx_ref{type=ThisT}=This,ColFg,ColBg)
 when ?is_colordata(ColFg),?is_colordata(ColBg) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(ColFg),wxe_util:color(ColBg),?get_env(),?wxCalendarCtrl_SetHeaderColours).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlgetheadercolourfg">external documentation</a>.
-doc """
Gets the foreground colour of the header part of the calendar window.

This method is currently only implemented in generic `m:wxCalendarCtrl` and always returns `wxNullColour` in the native versions.

See: `setHeaderColours/3`
""".
-spec getHeaderColourFg(This) -> wx:wx_colour4() when
	This::wxCalendarCtrl().
getHeaderColourFg(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarCtrl_GetHeaderColourFg),
  wxe_util:rec(?wxCalendarCtrl_GetHeaderColourFg).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlgetheadercolourbg">external documentation</a>.
-doc """
Gets the background colour of the header part of the calendar window.

This method is currently only implemented in generic `m:wxCalendarCtrl` and always returns `wxNullColour` in the native versions.

See: `setHeaderColours/3`
""".
-spec getHeaderColourBg(This) -> wx:wx_colour4() when
	This::wxCalendarCtrl().
getHeaderColourBg(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarCtrl_GetHeaderColourBg),
  wxe_util:rec(?wxCalendarCtrl_GetHeaderColourBg).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlsethighlightcolours">external documentation</a>.
-doc """
Set the colours to be used for highlighting the currently selected date.

This method is currently only implemented in generic `m:wxCalendarCtrl` and does nothing in the native versions.
""".
-spec setHighlightColours(This, ColFg, ColBg) -> 'ok' when
	This::wxCalendarCtrl(), ColFg::wx:wx_colour(), ColBg::wx:wx_colour().
setHighlightColours(#wx_ref{type=ThisT}=This,ColFg,ColBg)
 when ?is_colordata(ColFg),?is_colordata(ColBg) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(ColFg),wxe_util:color(ColBg),?get_env(),?wxCalendarCtrl_SetHighlightColours).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlgethighlightcolourfg">external documentation</a>.
-doc """
Gets the foreground highlight colour.

Only in generic `m:wxCalendarCtrl`.

This method is currently only implemented in generic `m:wxCalendarCtrl` and always returns `wxNullColour` in the native versions.

See: `setHighlightColours/3`
""".
-spec getHighlightColourFg(This) -> wx:wx_colour4() when
	This::wxCalendarCtrl().
getHighlightColourFg(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarCtrl_GetHighlightColourFg),
  wxe_util:rec(?wxCalendarCtrl_GetHighlightColourFg).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlgethighlightcolourbg">external documentation</a>.
-doc """
Gets the background highlight colour.

Only in generic `m:wxCalendarCtrl`.

This method is currently only implemented in generic `m:wxCalendarCtrl` and always returns `wxNullColour` in the native versions.

See: `setHighlightColours/3`
""".
-spec getHighlightColourBg(This) -> wx:wx_colour4() when
	This::wxCalendarCtrl().
getHighlightColourBg(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarCtrl_GetHighlightColourBg),
  wxe_util:rec(?wxCalendarCtrl_GetHighlightColourBg).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlsetholidaycolours">external documentation</a>.
-doc """
Sets the colours to be used for the holidays highlighting.

This method is only implemented in the generic version of the control and does nothing in the native ones. It should also only be called if the window style includes `wxCAL_SHOW_HOLIDAYS` flag or `enableHolidayDisplay/2` had been called.
""".
-spec setHolidayColours(This, ColFg, ColBg) -> 'ok' when
	This::wxCalendarCtrl(), ColFg::wx:wx_colour(), ColBg::wx:wx_colour().
setHolidayColours(#wx_ref{type=ThisT}=This,ColFg,ColBg)
 when ?is_colordata(ColFg),?is_colordata(ColBg) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,wxe_util:color(ColFg),wxe_util:color(ColBg),?get_env(),?wxCalendarCtrl_SetHolidayColours).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlgetholidaycolourfg">external documentation</a>.
-doc """
Return the foreground colour currently used for holiday highlighting.

Only useful with generic `m:wxCalendarCtrl` as native versions currently don't support holidays display at all and always return `wxNullColour`.

See: `setHolidayColours/3`
""".
-spec getHolidayColourFg(This) -> wx:wx_colour4() when
	This::wxCalendarCtrl().
getHolidayColourFg(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarCtrl_GetHolidayColourFg),
  wxe_util:rec(?wxCalendarCtrl_GetHolidayColourFg).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlgetholidaycolourbg">external documentation</a>.
-doc """
Return the background colour currently used for holiday highlighting.

Only useful with generic `m:wxCalendarCtrl` as native versions currently don't support holidays display at all and always return `wxNullColour`.

See: `setHolidayColours/3`
""".
-spec getHolidayColourBg(This) -> wx:wx_colour4() when
	This::wxCalendarCtrl().
getHolidayColourBg(#wx_ref{type=ThisT}=This) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,?get_env(),?wxCalendarCtrl_GetHolidayColourBg),
  wxe_util:rec(?wxCalendarCtrl_GetHolidayColourBg).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlgetattr">external documentation</a>.
-doc """
Returns the attribute for the given date (should be in the range 1...31).

The returned pointer may be NULL. Only in generic `m:wxCalendarCtrl`.
""".
-spec getAttr(This, Day) -> wxCalendarDateAttr:wxCalendarDateAttr() when
	This::wxCalendarCtrl(), Day::integer().
getAttr(#wx_ref{type=ThisT}=This,Day)
 when is_integer(Day) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,Day,?get_env(),?wxCalendarCtrl_GetAttr),
  wxe_util:rec(?wxCalendarCtrl_GetAttr).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlsetattr">external documentation</a>.
-doc """
Associates the attribute with the specified date (in the range 1...31).

If the pointer is NULL, the items attribute is cleared. Only in generic `m:wxCalendarCtrl`.
""".
-spec setAttr(This, Day, Attr) -> 'ok' when
	This::wxCalendarCtrl(), Day::integer(), Attr::wxCalendarDateAttr:wxCalendarDateAttr().
setAttr(#wx_ref{type=ThisT}=This,Day,#wx_ref{type=AttrT}=Attr)
 when is_integer(Day) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  ?CLASS(AttrT,wxCalendarDateAttr),
  wxe_util:queue_cmd(This,Day,Attr,?get_env(),?wxCalendarCtrl_SetAttr).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlsetholiday">external documentation</a>.
-doc """
Marks the specified day as being a holiday in the current month.

This method is only implemented in the generic version of the control and does nothing in the native ones.
""".
-spec setHoliday(This, Day) -> 'ok' when
	This::wxCalendarCtrl(), Day::integer().
setHoliday(#wx_ref{type=ThisT}=This,Day)
 when is_integer(Day) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,Day,?get_env(),?wxCalendarCtrl_SetHoliday).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlresetattr">external documentation</a>.
-doc """
Clears any attributes associated with the given day (in the range 1...31).

Only in generic `m:wxCalendarCtrl`.
""".
-spec resetAttr(This, Day) -> 'ok' when
	This::wxCalendarCtrl(), Day::integer().
resetAttr(#wx_ref{type=ThisT}=This,Day)
 when is_integer(Day) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,Day,?get_env(),?wxCalendarCtrl_ResetAttr).

%% @doc See <a href="http://www.wxwidgets.org/manuals/2.8.12/wx_wxcalendarctrl.html#wxcalendarctrlhittest">external documentation</a>.
%%<br /> Wd = ?wxDateTime_Sun | ?wxDateTime_Mon | ?wxDateTime_Tue | ?wxDateTime_Wed | ?wxDateTime_Thu | ?wxDateTime_Fri | ?wxDateTime_Sat | ?wxDateTime_Inv_WeekDay
%%<br /> Res = ?wxCAL_HITTEST_NOWHERE | ?wxCAL_HITTEST_HEADER | ?wxCAL_HITTEST_DAY | ?wxCAL_HITTEST_INCMONTH | ?wxCAL_HITTEST_DECMONTH | ?wxCAL_HITTEST_SURROUNDING_WEEK | ?wxCAL_HITTEST_WEEK
-doc """
Returns one of wxCalendarHitTestResult constants and fills either `date` or `wd` pointer with the corresponding value depending on the hit test code.

Not implemented in wxGTK currently.
""".
-spec hitTest(This, Pos) -> Result when
	Result ::{Res ::wx:wx_enum(), Date::wx:wx_datetime(), Wd::wx:wx_enum()},
	This::wxCalendarCtrl(), Pos::{X::integer(), Y::integer()}.
hitTest(#wx_ref{type=ThisT}=This,{PosX,PosY} = Pos)
 when is_integer(PosX),is_integer(PosY) ->
  ?CLASS(ThisT,wxCalendarCtrl),
  wxe_util:queue_cmd(This,Pos,?get_env(),?wxCalendarCtrl_HitTest),
  wxe_util:rec(?wxCalendarCtrl_HitTest).

%% @doc Destroys this object, do not use object again
-doc "Destroys the control.".
-spec destroy(This::wxCalendarCtrl()) -> 'ok'.
destroy(Obj=#wx_ref{type=Type}) ->
  ?CLASS(Type,wxCalendarCtrl),
  wxe_util:queue_cmd(Obj, ?get_env(), ?DESTROY_OBJECT),
  ok.
 %% From wxControl
%% @hidden
setLabel(This,Label) -> wxControl:setLabel(This,Label).
%% @hidden
getLabel(This) -> wxControl:getLabel(This).
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
show(This, Options) -> wxWindow:show(This, Options).
%% @hidden
show(This) -> wxWindow:show(This).
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

