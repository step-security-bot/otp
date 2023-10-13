# Specialized Decodes

[](){: id=SpecializedDecodes }
When performance is of highest priority and you are interested in a limited part of the ASN.1 encoded message before deciding what to do with the rest of it, an option is to decode only this small part. The situation can be a server that has to decide the addressee of a message. The addressee can be interested in the entire message, but the server can be a bottleneck that you want to spare any unnecessary load.

Instead of making two *complete decodes* (the normal case of decode), one in the server and one in the addressee, it is only necessary to make one *specialized decode*(in the server) and another complete decode(in the addressee). This section describes the following two specialized decodes, which support to solve this and similar problems:

* *Exclusive decode*
* *Selected decode*

This functionality is only provided when using `BER` (option `ber`).

## Exclusive Decode

The basic idea with exclusive decode is to specify which parts of the message you want to exclude from being decoded. These parts remain encoded and are returned in the value structure as binaries. They can be decoded in turn by passing them to a certain `decode_part/2` function. The performance gain is high for large messages. You can do an exclusive decode and later one or more decodes of the parts, or a second complete decode instead of two or more complete decodes.

### Procedure

To perform an exclusive decode:

* *Step 1:* Decide the name of the function for the exclusive decode.
* *Step 2:* Include the following instructions in a configuration file:

  * The name of the exclusive decode function
  * The name of the ASN.1 specification
  * A notation that tells which parts of the message structure to be excluded from decode
* *Step 3* Compile with the additional option `asn1config`. The compiler searches for a configuration file with the same name as the ASN.1 specification but with extension `.asn1config`. This configuration file is not the same as used for compilation of a set of files. See Section [Writing an Exclusive Decode Instruction.](asn1_spec.md#undecodedpart)

### User Interface

The runtime user interface for exclusive decode consists of the following two functions:

* A function for an exclusive decode, whose name the user decides in the configuration file
* The compiler generates a `decode_part/2` function when exclusive decode is chosen. This function decodes the parts that were left undecoded during the exclusive decode.

Both functions are described in the following.

If the exclusive decode function has, for example, the name `decode_exclusive` and an ASN.1 encoded message `Bin` is to be exclusive decoded, the call is as follows:

```text
{ok,Excl_Message} = 'MyModule':decode_exclusive(Bin)
```

[](){: id=UndecodedPart }
The result `Excl_Message` has the same structure as a complete decode would have, except for the parts of the top type that were not decoded. The undecoded parts are on their places in the structure on format `{Type_Key,Undecoded_Value}`.

Each undecoded part that is to be decoded must be fed into function `decode_part/2` as follows:

```text
{ok,Part_Message} = 'MyModule':decode_part(Type_Key,Undecoded_Value)
```

[](){: id="Exclusive Instruction" }
### Writing an Exclusive Decode Instruction

This instruction is written in the configuration file in the following format:

```text
Exclusive_Decode_Instruction = {exclusive_decode,{Module_Name,Decode_Instructions}}.

Module_Name = atom()

Decode_Instructions = [Decode_Instruction]+

Decode_Instruction = {Exclusive_Decode_Function_Name,Type_List}

Exclusive_Decode_Function_Name = atom()

Type_List = [Top_Type,Element_List]

Element_List = [Element]+

Element = {Name,parts} |
          {Name,undecoded} |
          {Name,Element_List}

Top_Type = atom()

Name = atom()
```

The instruction must be a valid Erlang term ended by a dot.

In `Type_List` the "path" from the top type to each undecoded subcomponents is described. The top type of the path is an atom, the name of it. The action on each component/type that follows is described by one of `{Name,parts}, {Name,undecoded}, {Name,Element_List}`.

The use and effect of the actions are as follows:

* `{Name,undecoded}` \- Tells that the element is left undecoded during the exclusive decode. The type of `Name` can be any ASN.1 type. The value of element `Name` is returned as a tuple (as mentioned in the previous section) in the value structure of the top type.
* `{Name,parts}` \- The type of `Name` can be one of `SEQUENCE OF` or `SET OF`. The action implies that the different components of `Name` are left undecoded. The value of `Name` is returned as a tuple (as mentioned in the previous section) where the second element is a list of binaries. This is because the representation of a `SEQUENCE OF` or a `SET OF` in Erlang is a list of its internal type. Any of the elements in this list or the entire list can be decoded by function `decode_part`.
* `{Name,Element_List}` \- This action is used when one or more of the subtypes of `Name` is exclusive decoded.

`Name` in these actions can be a component name of a `SEQUENCE OF` or a `SET OF`, or a name of an alternative in a `CHOICE`.

### Example

In this examples, the definitions from the following ASN.1 specification are used:

```text

GUI DEFINITIONS AUTOMATIC TAGS ::=

BEGIN

Action ::= SEQUENCE 
 { 
   number  INTEGER DEFAULT 15,
   handle  [0] Handle DEFAULT {number  12, on  TRUE}
 }

Key ::= [11] EXPLICIT Button
Handle ::= [12] Key
Button ::= SEQUENCE 
 {
   number  INTEGER,
   on  BOOLEAN
 }

Window ::= CHOICE 
 {
   vsn INTEGER,
   status E
 }

Status ::= SEQUENCE 
 {
   state INTEGER,
   buttonList SEQUENCE OF Button,
   enabled BOOLEAN OPTIONAL,
   actions CHOICE {
     possibleActions SEQUENCE OF Action,
     noOfActions INTEGER
   }
 }


END
```
{: id=Asn1spec }

If `Button` is a top type and it is needed to exclude component `number` from decode, `Type_List` in the instruction in the configuration file is `['Button',[{number,undecoded}]]`. If you call the decode function `decode_Button_exclusive`, `Decode_Instruction` is `{decode_Button_exclusive,['Button',[{number,undecoded}]]}`.

Another top type is `Window` whose subcomponent actions in type `Status` and the parts of component `buttonList` are to be left undecoded. For this type, the function is named `decode__Window_exclusive`. The complete `Exclusive_Decode_Instruction` configuration is as follows:

```text

{exclusive_decode,{'GUI',
	[{decode_Window_exclusive,['Window',[{status,[{buttonList,parts},{actions,undecoded}]}]]},
	 {decode_Button_exclusive,['Button',[{number,undecoded}]]}]}}.
```

The following figure shows the bytes of a `Window:status` message. The components `buttonList` and `actions` are excluded from decode. Only `state` and `enabled` are decoded when `decode__Window_exclusive` is called.



![Bytes of a Window:status Message](assets/exclusive_Win_But.gif "Bytes of a Window:status Message")



Compiling `GUI.asn` including the configuration file is done as follows:

```text
unix> erlc -bber +asn1config GUI.asn

erlang> asn1ct:compile('GUI', [ber,asn1config]).
```

The module can be used as follows:

```text
1> Button_Msg = {'Button',123,true}.
{'Button',123,true}
2> {ok,Button_Bytes} = 'GUI':encode('Button',Button_Msg).
{ok,[<<48>>,
     [6],
     [<<128>>,
      [1],
      123],
     [<<129>>,
      [1],
      255]]}
3> {ok,Exclusive_Msg_Button} = 'GUI':decode_Button_exclusive(list_to_binary(Button_Bytes)).
{ok,{'Button',{'Button_number',<<28,1,123>>},
         true}}
4> 'GUI':decode_part('Button_number',<<128,1,123>>).
{ok,123}
5> Window_Msg = 
{'Window',{status,{'Status',35,
              [{'Button',3,true},
               {'Button',4,false},
               {'Button',5,true},
               {'Button',6,true},
               {'Button',7,false},
               {'Button',8,true},
               {'Button',9,true},
               {'Button',10,false},
               {'Button',11,true},
               {'Button',12,true},
               {'Button',13,false},
               {'Button',14,true}],
              false,
              {possibleActions,[{'Action',16,{'Button',17,true}}]}}}}. 
{'Window',{status,{'Status',35,
              [{'Button',3,true},
               {'Button',4,false},
               {'Button',5,true},
               {'Button',6,true},
               {'Button',7,false},
               {'Button',8,true},
               {'Button',9,true},
               {'Button',10,false},
               {'Button',11,true},
               {'Button',12,true},
               {'Button',13,false},
               {'Button',14,true}],
              false,
              {possibleActions,[{'Action',16,{'Button',17,true}}]}}}}
6> {ok,Window_Bytes}='GUI':encode('Window',Window_Msg).
{ok,[<<161>>,
     [127],
     [<<128>>, ...


8> {ok,{status,{'Status',Int,{Type_Key_SeqOf,Val_SEQOF},
BoolOpt,{Type_Key_Choice,Val_Choice}}}}=
'GUI':decode_Window_status_exclusive(list_to_binary(Window_Bytes)).
{ok,{status,{'Status',35,
        {'Status_buttonList',[<<48,6,128,1,3,129,1,255>>,
                              <<48,6,128,1,4,129,1,0>>,
                              <<48,6,128,1,5,129,1,255>>,
                              <<48,6,128,1,6,129,1,255>>,
                              <<48,6,128,1,7,129,1,0>>,
                              <<48,6,128,1,8,129,1,255>>,
                              <<48,6,128,1,9,129,1,255>>,
                              <<48,6,128,1,10,129,1,0>>,
                              <<48,6,128,1,11,129,1,255>>,
                              <<48,6,128,1,12,129,1,255>>,
                              <<48,6,128,1,13,129,1,0>>,
                              <<48,6,128,1,14,129,1,255>>]},
        false,
        {'Status_actions',
<<163,21,160,19,48,17,2,1,16,160,12,172,10,171,8,48,6,128,1,...>>}}}}
10> 'GUI':decode_part(Type_Key_SeqOf,Val_SEQOF).
{ok,[{'Button',3,true},
     {'Button',4,false},
     {'Button',5,true},
     {'Button',6,true},
     {'Button',7,false},
     {'Button',8,true},
     {'Button',9,true},
     {'Button',10,false},
     {'Button',11,true},
     {'Button',12,true},
     {'Button',13,false},
     {'Button',14,true}]}
11> 'GUI':decode_part(Type_Key_SeqOf,hd(Val_SEQOF)).
{ok,{'Button',3,true}}
12> 'GUI':decode_part(Type_Key_Choice,Val_Choice).  
{ok,{possibleActions,[{'Action',16,{'Button',17,true}}]}}
```

## Selective Decode

This specialized decode decodes a subtype of a constructed value and is the fastest method to extract a subvalue. This decode is typically used when you want to inspect, for example, a version number, to be able to decide what to do with the entire value. The result is returned as `{ok,Value}` or `{error,Reason}`.

### Procedure

To perform a selective decode:

* *Step 1:* Include the following instructions in the configuration file:

  * The name of the user function
  * The name of the ASN.1 specification
  * A notation that tells which part of the type to be decoded
* *Step 2:* Compile with the additional option `asn1config`. The compiler searches for a configuration file with the same name as the ASN.1 specification, but with extension `.asn1config`. In the same file you can also provide configuration specifications for exclusive decode. The generated Erlang module has the usual functionality for encode/decode preserved and the specialized decode functionality added.

### User Interface

The only new user interface function is the one provided by the user in the configuration file. The function is started by the `ModuleName:FunctionName` notation.

For example, if the configuration file includes the specification `{selective_decode,{'ModuleName',[{selected_decode_Window,TypeList}]}}` do the selective decode by `{ok,Result}='ModuleName':selected_decode_Window(EncodedBinary).`

[](){: id="Selective Instruction" }
### Writing a Selective Decode Instruction

One or more selective decode functions can be described in a configuration file. Use the following notation:

```text
Selective_Decode_Instruction = {selective_decode,{Module_Name,Decode_Instructions}}.

Module_Name = atom()

Decode_Instructions = [Decode_Instruction]+

Decode_Instruction = {Selective_Decode_Function_Name,Type_List}

Selective_Decode_Function_Name = atom()

Type_List = [Top_Type|Element_List]

Element_List = Name|List_Selector

Name = atom()

List_Selector = [integer()]
```

The instruction must be a valid Erlang term ended by a dot.

* `Module_Name` is the same as the name of the ASN.1 specification, but without the extension.
* `Decode_Instruction` is a tuple with your chosen function name and the components from the top type that leads to the single type you want to decode. Ensure to choose a name of your function that is not the same as any of the generated functions.
* The first element of `Type_List` is the top type of the encoded message. In `Element_List`, it is followed by each of the component names that leads to selected type.
* Each name in `Element_List` must be a constructed type except the last name, which can be any type.
* `List_Selector` makes it possible to choose one of the encoded components in a a `SEQUENCE OF` or a `SET OF`. It is also possible to go further in that component and pick a subtype of that to decode. So, in the `Type_List`: `['Window',status,buttonList,[1],number]`, component `buttonList` must be of type `SEQUENCE OF` or `SET OF`.

In the example, component `number` of the first of the encoded elements in the `SEQUENCE OF` `buttonList` is selected. This applies on the ASN.1 specification in Section [Writing an Exclusive Decode Instruction](asn1_spec.md#asn1spec).

### Another Example

In this example, the same ASN.1 specification as in Section [Writing an Exclusive Decode Instruction](asn1_spec.md#asn1spec) is used. The following is a valid selective decode instruction:

```text
{selective_decode,
    {'GUI',
        [{selected_decode_Window1,
            ['Window',status,buttonList, 
             [1],
             number]},
 {selected_decode_Action,
     ['Action',handle,number]},
 {selected_decode_Window2,
     ['Window',
      status,
      actions,
      possibleActions,
      [1],
      handle,number]}]}}.
```

The first instruction, `{selected_decode_Window1,['Window',status,buttonList,[1],number]}` is described in the previous section.

The second instruction, `{selected_decode_Action,['Action',handle,number]}`, takes component `number` in the `handle` component of type `Action`. If the value is `ValAction = {'Action',17,{'Button',4711,false}}`, the internal value 4711 is to be picked by `selected_decode_Action`. In an Erlang terminal it looks as follows:

```text
ValAction = {'Action',17,{'Button',4711,false}}.
{'Action',17,{'Button',4711,false}}
7> {ok,Bytes}='GUI':encode('Action',ValAction).
...
8> BinBytes = list_to_binary(Bytes).
<<48,18,2,1,17,160,13,172,11,171,9,48,7,128,2,18,103,129,1,0>>
9> 'GUI':selected_decode_Action(BinBytes).
{ok,4711}
10>
```

The third instruction, `['Window',status,actions,possibleActions,[1],handle,number]`, works as follows:

* *Step 1:* Starts with type `Window`.
* *Step 2:* Takes component `status` of `Window` that is of type `Status`.
* *Step 3:* Takes *actions* of type `Status`.
* *Step 4:* Takes `possibleActions` of the internally defined `CHOICE` type.
* *Step 5:* Goes into the first component of `SEQUENCE OF` by `[1]`. That component is of type `Action`.
* *Step 6:* Takes component `handle`.
* *Step 7:* Takes component `number` of type `Button`.

The following figure shows which components are in `TypeList` `['Window',status,actions,possibleActions,[1],handle,number]`:



![Elements Specified in Configuration File for Selective Decode of a Subvalue in a Window Message](assets/selective_TypeList.gif "Elements Specified in Configuration File for Selective Decode of a Subvalue in a Window Message")

In the following figure, only the marked element is decoded by `selected_decode_Window2`:



![Bytes of a Window:status Message](assets/selective_Window2.gif "Bytes of a Window:status Message")

With the following example, you can examine that both `selected_decode_Window2` and `selected_decode_Window1` decodes the intended subvalue of value `Val`:

```text
1> Val = {'Window',{status,{'Status',12,
                    [{'Button',13,true},
                     {'Button',14,false},
                     {'Button',15,true},
                     {'Button',16,false}],
                    true,
                    {possibleActions,[{'Action',17,{'Button',18,false}},
                                      {'Action',19,{'Button',20,true}},
                                      {'Action',21,{'Button',22,false}}]}}}}
2> {ok,Bytes}='GUI':encode('Window',Val).
...
3> Bin = list_to_binary(Bytes).
<<161,101,128,1,12,161,32,48,6,128,1,13,129,1,255,48,6,128,1,14,129,1,0,48,6,128,1,15,129,...>>
4> 'GUI':selected_decode_Window1(Bin).
{ok,13}
5> 'GUI':selected_decode_Window2(Bin).
{ok,18}
```

Notice that the value fed into the selective decode functions must be a binary.

## Performance

To give an indication on the possible performance gain using the specialized decodes, some measures have been performed. The relative figures in the outcome between selective, exclusive, and complete decode (the normal case) depend on the structure of the type, the size of the message, and on what level the selective and exclusive decodes are specified.

### ASN.1 Specifications, Messages, and Configuration

The specifications [GUI](asn1_spec.md#asn1spec) and [MEDIA-GATEWAY-CONTROL](http://www.itu.int/ITU-T/asn1/database/itu-t/h/h248/2002/MEDIA-GATEWAY-CONTROL.html) were used in the test.

For the `GUI` specification the configuration was as follows:

```text
{selective_decode,
  {'GUI',
    [{selected_decode_Window1,
         ['Window',
          status,buttonList,
          [1],
          number]},
     {selected_decode_Window2,
         ['Window',
          status,
          actions,
          possibleActions,
          [1],
          handle,number]}]}}.
     {exclusive_decode,
         {'GUI',
            [{decode_Window_status_exclusive,
                ['Window',
                 [{status,
                     [{buttonList,parts},
                      {actions,undecoded}]}]]}]}}.
```

The `MEDIA-GATEWAY-CONTROL` configuration was as follows:

```text
{exclusive_decode,
  {'MEDIA-GATEWAY-CONTROL',
    [{decode_MegacoMessage_exclusive,
        ['MegacoMessage',
         [{authHeader,undecoded},
          {mess,
             [{mId,undecoded},
              {messageBody,undecoded}]}]]}]}}.
{selective_decode,
  {'MEDIA-GATEWAY-CONTROL',
    [{decode_MegacoMessage_selective,
         ['MegacoMessage',mess,version]}]}}.
```

The corresponding values were as follows:

```text
{'Window',{status,{'Status',12,
              [{'Button',13,true},
               {'Button',14,false},
               {'Button',15,true},
               {'Button',16,false},
               {'Button',13,true},
               {'Button',14,false},
               {'Button',15,true},
               {'Button',16,false},
               {'Button',13,true},
               {'Button',14,false},
               {'Button',15,true},
               {'Button',16,false}],
              true,
              {possibleActions,
                 [{'Action',17,{'Button',18,false}},
                  {'Action',19,{'Button',20,true}},
                  {'Action',21,{'Button',22,false}},
                  {'Action',17,{'Button',18,false}},
                  {'Action',19,{'Button',20,true}},
                  {'Action',21,{'Button',22,false}},
                  {'Action',17,{'Button',18,false}},
                  {'Action',19,{'Button',20,true}},
                  {'Action',21,{'Button',22,false}},
                  {'Action',17,{'Button',18,false}},
                  {'Action',19,{'Button',20,true}},
                  {'Action',21,{'Button',22,false}},
                  {'Action',17,{'Button',18,false}},
                  {'Action',19,{'Button',20,true}},
                  {'Action',21,{'Button',22,false}},
                  {'Action',17,{'Button',18,false}},
                  {'Action',19,{'Button',20,true}},
                  {'Action',21,{'Button',22,false}}]}}}}


{'MegacoMessage',asn1_NOVALUE,
  {'Message',1,
    {ip4Address,
      {'IP4Address',[125,125,125,111],55555}},
  {transactions,
    [{transactionReply,
      {'TransactionReply',50007,asn1_NOVALUE,
       {actionReplies,
        [{'ActionReply',0,asn1_NOVALUE,asn1_NOVALUE,
          [{auditValueReply,{auditResult,{'AuditResult',
            {'TerminationID',[],[255,255,255]},
             [{mediaDescriptor,
               {'MediaDescriptor',asn1_NOVALUE,
                {multiStream,
                 [{'StreamDescriptor',1,
                   {'StreamParms',
                    {'LocalControlDescriptor',
                     sendRecv,
                     asn1_NOVALUE,
                     asn1_NOVALUE,
                     [{'PropertyParm',
                       [0,11,0,7],
                       [[52,48]],
                       asn1_NOVALUE}]},
                    {'LocalRemoteDescriptor',
                     [[{'PropertyParm',
                        [0,0,176,1],
                        [[48]],
                        asn1_NOVALUE},
                       {'PropertyParm',
                         [0,0,176,8],
                         [[73,78,32,73,80,52,32,49,50,53,46,49,
                           50,53,46,49,50,53,46,49,49,49]],
                         asn1_NOVALUE},
                       {'PropertyParm',
                         [0,0,176,15],
                         [[97,117,100,105,111,32,49,49,49,49,32,
                           82,84,80,47,65,86,80,32,32,52]],
                         asn1_NOVALUE},
                       {'PropertyParm',
                         [0,0,176,12],
                         [[112,116,105,109,101,58,51,48]],
                         asn1_NOVALUE}]]},
                    {'LocalRemoteDescriptor',
                     [[{'PropertyParm',
                         [0,0,176,1],
                         [[48]],
                         asn1_NOVALUE},
                       {'PropertyParm',
                         [0,0,176,8],
                         [[73,78,32,73,80,52,32,49,50,52,46,49,50,
                           52,46,49,50,52,46,50,50,50]],
                         asn1_NOVALUE},
                       {'PropertyParm',
                         [0,0,176,15],
                         [[97,117,100,105,111,32,50,50,50,50,32,82,
                           84,80,47,65,86,80,32,32,52]],
                         asn1_NOVALUE},
                       {'PropertyParm',
                         [0,0,176,12],
                         [[112,116,105,109,101,58,51,48]],
                         asn1_NOVALUE}]]}}}]}}},
              {packagesDescriptor,
               [{'PackagesItem',[0,11],1},
                {'PackagesItem',[0,11],1}]},
              {statisticsDescriptor,
               [{'StatisticsParameter',[0,12,0,4],[[49,50,48,48]]},
                {'StatisticsParameter',[0,11,0,2],[[54,50,51,48,48]]},
                {'StatisticsParameter',[0,12,0,5],[[55,48,48]]},
                {'StatisticsParameter',[0,11,0,3],[[52,53,49,48,48]]},
                {'StatisticsParameter',[0,12,0,6],[[48,46,50]]},
                {'StatisticsParameter',[0,12,0,7],[[50,48]]},
                {'StatisticsParameter',[0,12,0,8],[[52,48]]}]}]}}}]}]}}}]}}}
```

The size of the encoded values was 458 bytes for `GUI` and 464 bytes for `MEDIA-GATEWAY-CONTROL`.

### Results

The ASN.1 specifications in the test were compiled with options `ber_bin, optimize, driver` and `asn1config`. Omitting option `driver` gives higher values for `decode` and `decode_part`. These tests have not been rerun using NIFs, but are expected to perform about 5% better than the linked-in driver.

The test program runs 10000 decodes on the value, resulting in an output with the elapsed time in microseconds for the total number of decodes.

| *Function* | *Time* (microseconds) | *Decode Type* | *ASN.1 Specification* | *% of Time versus Complete Decode* |
|------------|-----------------------|---------------|-----------------------|------------------------------------|
| `decode_MegacoMessage_selective/1` | `374045` | `Selective` | `MEDIA-GATEWAY-CONTROL` | *8\.3* |
| `decode_MegacoMessage_exclusive/1` | `621107` | `Exclusive` | `MEDIA-GATEWAY-CONTROL` | *13\.8* |
| `decode/2` | `4507457` | `Complete` | `MEDIA-GATEWAY-CONTROL` | *100* |
| `selected_decode_Window1/1` | `449585` | `Selective` | `GUI` | *7\.6* |
| `selected_decode_Window2/1` | `890666` | `Selective` | `GUI` | *15\.1* |
| `decode_Window_status_exclusive/1` | `1251878` | `Exclusive` | `GUI` | *21\.3* |
| `decode/2` | `5889197` | `Complete` | `GUI` | *100* |


*Table: Results of Complete, Exclusive, and Selective Decode*

It is also of interest to know the relation is between a complete decode, an exclusive decode followed by `decode_part` of the excluded parts, and a selective decode followed by a complete decode. Some situations can be compared to this simulation, for example, inspect a subvalue and later inspect the entire value. The following table shows figures from this test. The number of loops and the time unit are the same as in the previous test.

| *Actions* | *Function*     | *Time* (microseconds) | *ASN.1 Specification* | *% of Time vs. Complete Decode* |
|-----------|----------------|-----------------------|-----------------------|---------------------------------|
| `Complete` | `decode/2` | `4507457` | `MEDIA-GATEWAY-CONTROL` | *100* |
| `Selective and Complete` | `decode_­MegacoMessage_­selective/1` | `4881502` | `MEDIA-GATEWAY-CONTROL` | *108\.3* |
| `Exclusive and decode_part` | `decode_­MegacoMessage_­exclusive/1` | `5481034` | `MEDIA-GATEWAY-CONTROL` | *112\.3* |
| `Complete` | `decode/2` | `5889197` | `GUI` | *100* |
| `Selective and Complete` | `selected_­decode_­Window1/1` | `6337636` | `GUI` | *107\.6* |
| `Selective and Complete` | `selected_­decode_­Window2/1` | `6795319` | `GUI` | *115\.4* |
| `Exclusive and decode_part` | `decode_­Window_­status_­exclusive/1` | `6249200` | `GUI` | *106\.1* |


*Table: Results of Complete, Exclusive + decode_part, and Selective + complete decodes*

Other ASN.1 types and values can differ much from these figures. It is therefore important that you, in every case where you intend to use either of these decodes, perform some tests that show if you will benefit your purpose.

### Final Remarks

* The gain of using selective and exclusive decode instead of a complete decode is greater the bigger the value and the less deep in the structure you have to decode.
* Use selective decode instead of exclusive decode if you are interested in only a single subvalue.
* Exclusive decode followed by `decode_part` decodes is attractive if the parts are sent to different servers for decoding, or if you in some cases are not interested in all parts.
* The fastest selective decode is when the decoded type is a primitive type and not so deep in the structure of the top type. `selected_decode_Window2` decodes a high constructed value, which explains why this operation is relatively slow.
* It can vary from case to case which combination of selective/complete decode or exclusive/part decode is the fastest.
