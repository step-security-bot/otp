# wx the erlang binding of wxWidgets

The *wx* application is an erlang binding of *wxWidgets*. This document describes the erlang mapping to wxWidgets and it's implementation. It is not a complete users guide to wxWidgets. If you need that, you will have to read the wxWidgets documentation instead. *wx* tries to keep a one-to-one mapping with the original API so that the original documentation and examples shall be as easy as possible to use.

wxErlang examples and test suite can be found in the erlang src release. They can also provide some help on how to use the API.

This is currently a very brief introduction to *wx*. The application is still under development, which means the interface may change, and the test suite currently have a poor coverage ratio.

[](){: id=Contents }
## Contents

* [Introduction](chapter.md#introduction)
* [Multiple processes and memory handling](chapter.md#multiple_processes_and_memory_handling)
* [Event Handling](chapter.md#event_handling)
* [Acknowledgments](chapter.md#acknowledgments)

[](){: id=Introduction }
## Introduction

The original *wxWidgets* is an object-oriented (C++) API and that is reflected in the erlang mapping. In most cases each class in wxWidgets is represented as a module in erlang. This gives the *wx* application a huge interface, spread over several modules, and it all starts with the *wx* module. The *wx* module contains functions to create and destroy the GUI, i.e. `wx:new/0`, `wx:destroy/0`, and some other useful functions.

Objects or object references in *wx* should be seen as erlang processes rather than erlang terms. When you operate on them they can change state, e.g. they are not functional objects as erlang terms are. Each object has a type or rather a class, which is manipulated with the corresponding module or by sub-classes of that object. Type checking is done so that a module only operates on it's objects or inherited classes.

An object is created with *new* and destroyed with *destroy*. Most functions in the classes are named the same as their C++ counterpart, except that for convenience, in erlang they start with a lowercase letter and the first argument is the object reference. Optional arguments are last and expressed as tagged tuples in any order.

For example the *wxWindow* C++ class is implemented in the *wxWindow* erlang module and the member *wxWindow::CenterOnParent* is thus *wxWindow:centerOnParent*. The following C++ code:

```text
  wxWindow MyWin = new wxWindow();
  MyWin.CenterOnParent(wxVERTICAL);
  ...
  delete MyWin;
```

would in erlang look like:

```text
  MyWin = wxWindow:new(),
  wxWindow:centerOnParent(MyWin, [{dir,?wxVERTICAL}]),
  ...
  wxWindow:destroy(MyWin),
```

When you are reading wxWidgets documentation or the examples, you will notice that some of the most basic classes are missing in *wx*, they are directly mapped to corresponding erlang terms:

* __*wxPoint* is represented by \{Xcoord,Ycoord\}__  

* __*wxSize* is represented by \{Width,Height\}__  

* __*wxRect* is represented by \{Xcoord,Ycoord,Width,Height\}__  

* __*wxColour* is represented by \{Red,Green,Blue\[,Alpha]\}__  

* __*wxString* is represented by [unicode:charlist()](`m:unicode#type-charlist`)__  

* __*wxGBPosition* is represented by \{Row,Column\}__  

* __*wxGBSpan* is represented by \{RowSpan,ColumnSPan\}__  

* __*wxGridCellCoords* is represented by \{Row,Column\}__  

In the places where the erlang API differs from the original one it should be obvious from the erlang documentation which representation has been used. E.g. the C++ arrays and/or lists are sometimes represented as erlang lists and sometimes as tuples.

Colours are represented with \{Red,Green,Blue\[,Alpha]\}, the Alpha value is optional when used as an argument to functions, but it will always be returned from *wx* functions.

Defines, enumerations and global variables exists in `wx.hrl` as defines. Most of these defines are constants but not all. Some are platform dependent and therefore the global variables must be instantiated during runtime. These will be acquired from the driver with a call, so not all defines can be used in matching statements. Class local enumerations will be prefixed with the class name and a underscore as in `ClassName_Enum`.

Additionally some global functions, i.e. non-class functions, exist in the `wx_misc` module.

*wxErlang* is implemented as a (threaded) driver and a rather direct interface to the C++ API, with the drawback that if the erlang programmer does an error, it might crash the emulator.

Since the driver is threaded it requires a *smp* enabled emulator, that provides a thread safe interface to the driver.

[](){: id=Multiple_processes_and_memory_handling }
## Multiple processes and memory handling

The intention is that each erlang application calls wx:new() once to setup it's GUI which creates an environment and a memory mapping. To be able to use *wx* from several processes in your application, you must share the environment. You can get the active environment with `wx:get_env/0` and set it in the new processes with `wx:set_env/1`. Two processes or applications which have both called wx:new() will not be able use each others objects.

```text
  wx:new(), 
  MyWin = wxFrame:new(wx:null(), 42, "Example", []),
  Env = wx:get_env(),
  spawn(fun() -> 
           wx:set_env(Env),
           %% Here you can do wx calls from your helper process.
           ...
        end),
  ...
```

When `wx:destroy/0` is invoked or when all processes in the application have died, the memory is deleted and all windows created by that application are closed.

The *wx* application never cleans or garbage collects memory as long as the user application is alive. Most of the objects are deleted when a window is closed, or at least all the objects which have a parent argument that is non null. By using `wxCLASS:destroy/1` when possible you can avoid an increasing memory usage. This is especially important when *wxWidgets* assumes or recommends that you (or rather the C++ programmer) have allocated the object on the stack since that will never be done in the erlang binding. For example `wxDC` class or its sub-classes or `wxSizerFlags`.

Currently the dialogs show modal function freezes wxWidgets until the dialog is closed. That is intended but in erlang where you can have several GUI applications running at the same time it causes trouble. This will hopefully be fixed in future *wxWidgets* releases.

[](){: id=Event_Handling }
## Event Handling

Event handling in *wx* differs most from the original API. You must specify every event you want to handle in *wxWidgets*, that is the same in the erlang binding but you can choose to receive the events as messages or handle them with callback *funs*.

Otherwise the event subscription is handled as *wxWidgets* dynamic event-handler connection. You subscribe to events of a certain type from objects with an *ID* or within a range of *ID*s. The callback *fun* is optional, if not supplied the event will be sent to the process that called *connect/2*. Thus, a handler is a callback *fun* or a process which will receive an event message.

Events are handled in order from bottom to top, in the widgets hierarchy, by the last subscribed handler first. Depending on if `wxEvent:skip()` is called the event will be handled by the other handler(s) afterwards. Most of the events have default event handler(s) installed.

Message events looks like [\#wx\{id=integer(), obj=wx:wxObject(), userData=term(), event=Rec](`m:wxEvtHandler#type-wx`) \}. The *id* is the identifier of the object that received the event. The *obj* field contains the object that you used *connect* on. The *userData* field contains a user supplied term, this is an option to *connect*. And the *event* field contains a record with event type dependent information. The first element in the event record is always the type you subscribed to. For example if you subscribed to *key_up* events you will receive the `#wx{event=Event}` where *Event* will be a *wxKey* event record where `Event#wxKey.type = key_up`.

In *wxWidgets* the developer has to call `wxEvent:skip()` if he wants the event to be processed by other handlers. You can do the same in *wx* if you use callbacks. If you want the event as messages you just don't supply a callback and you can set the *skip* option in *connect* call to true or false, the default it is false. True means that you get the message but let the subsequent handlers also handle the event. If you want to change this behavior dynamically you must use callbacks and call `wxEvent:skip()`.

Callback event handling is done by using the optional callback *fun/2* when attaching the handler. The *fun(#wx\{\},wxObject()* must take two arguments where the first is the same as with message events described above and the second is an object reference to the actual event object. With the event object you can call `wxEvent:skip()` and access all the data. When using callbacks you must call `wxEvent:skip()` by yourself if you want any of the events to be forwarded to the following handlers. The actual event objects are deleted after the *fun* returns.

The callbacks are always invoked by another process and have exclusive usage of the GUI when invoked. This means that a callback *fun* cannot use the process dictionary and should not make calls to other processes. Calls to another process inside a callback *fun* may cause a deadlock if the other process is waiting on completion of his call to the GUI.

[](){: id=Acknowledgments }
## Acknowledgments

Mats-Ola Persson wrote the initial *wxWidgets* binding as part of his master thesis. The current version is a total re-write but many ideas have been reused. The reason for the re-write was mostly due to the limited requirements he had been given by us.

Also thanks to the *wxWidgets* team that develops and supports it so we have something to use.
