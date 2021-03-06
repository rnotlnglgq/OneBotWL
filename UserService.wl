(* ::Package:: *)

BeginPackage["OneBot`UserService`", {"OneBot`", "Rubi`", "MaTeX`"}]


<<RubiSteps.wl


ClearAll["`*"]


PrivateHandler


ApplyHandler
QuickReplyResponse
$NoActionResponse


$Administrator


AdminEvaluate
WLEvaluate
StringEvaluate
TeXEvaluate
CallInt
IntEvaluate
FigureEvaluate


Begin["`Private`"]


OneBot`Utilities`$ContextWhiteList = {"Global`", "Rubi`"};
OneBot`Utilities`$UserSymbolWhiteList = {MaTeX`MaTeX};


ClearAll["`*"]


(* ::Section:: *)
(*Dispatch*)


OneBot`$MainHandler = PrivateHandler;


(* ::Section:: *)
(*Tool*)


MessagePattern["help"] = <|
	"data" -> <|"text" -> s_String /; StringMatchQ[WhitespaceCharacter...~~"help"~~WhitespaceCharacter...]@s|>,
	"type" -> "text"
|>;


MessagePattern["log"] = <|
	"data" -> <|"text" -> s_String /; StringMatchQ[WhitespaceCharacter...~~"log"~~Whitespace~~__]@s|>,
	"type" -> "text"
|>;


MessagePattern["admin"] = <|
	"data" -> <|"text" -> s_String /; StringMatchQ[WhitespaceCharacter...~~"admin"~~Whitespace~~__]@s|>,
	"type" -> "text"
|>;


MessagePattern["wl"] = <|
	"data" -> <|"text" -> s_String /; StringMatchQ[WhitespaceCharacter...~~"wl"~~Whitespace~~__]@s|>,
	"type" -> "text"
|>;


MessagePattern["tex"] = <|
	"data" -> <|"text" -> s_String /; StringMatchQ[WhitespaceCharacter...~~"tex"~~Whitespace~~__]@s|>,
	"type" -> "text"
|>;


MessagePattern["int"] = <|
	"data" -> <|"text" -> s_String /; StringMatchQ[WhitespaceCharacter...~~"int"~~Whitespace~~__~~Whitespace~~Except[WhitespaceCharacter]..]@s|>,
	"type" -> "text"
|>;


MessagePattern["fig"] = <|
	"data" -> <|"text" -> s_String /; StringMatchQ[WhitespaceCharacter...~~"fig"~~Whitespace~~__]@s|>,
	"type" -> "text"
|>;


(* ::Section:: *)
(*Function*)


(* ::Text:: *)
(*To do: judge whether a message will run out of size.*)


AdminEvaluate[message_] := MessageTemplate["text"]@ToString[
	ToExpression@First@StringCases[message[[-1]]["data", "text"], "admin"~~Whitespace~~e__~~WhitespaceCharacter...~~EndOfString :> e, 1]
, InputForm]


LogEvaluate[message_] := ToExpression@First@StringCases[message[[-1]]["data", "text"],
	"log"~~Whitespace~~e__~~WhitespaceCharacter...~~EndOfString :> e
, 1] //Switch[#,
	_?StringQ,
		#,
	_,
		Switch[$DebugLevel,
			2, Print["StringEvaluate: ", #," is not a string"], 
			_, Print@"StringEvaluate: Not a string"
		]; Failure["StringFailure", <||>]
]& //MessageTemplate["text"]


WLEvaluate[message_] := MessageTemplate["text"]@ToString[
	OneBot`Utilities`SafeToExpression@
		First@StringCases[message[[-1]]["data", "text"], "wl"~~Whitespace~~e__~~WhitespaceCharacter...~~EndOfString :> e, 1]
, InputForm] //OneBot`Utilities`ConstrainedEvaluate


CallInt[expr_, var_] := MaTeX`MaTeX[
	RubiSteps`ShowIntSteps[Rubi`Int[expr, var], FormatType -> TeXForm]
, Magnification -> 1.5]


IntEvaluate[message_] := CallInt @@ OneBot`Utilities`SafeToExpression /@ StringCases[
		message[[-1]]["data", "text"],
		"int"~~Whitespace~~expr__~~Whitespace~~var:Except[WhitespaceCharacter]..~~WhitespaceCharacter...~~EndOfString :> {expr, var}
	, 1][[1]] //Switch[#,
	_Graphics,
		If[$DebugLevel > 0, Print@"IntEvaluate: Rasterizing"];
		MessageTemplate["img"]@Rasterize[#, ImageResolution -> 200],
	$Failed,
		If[$DebugLevel > 0, Print@"IntEvaluate: MaTeXFailure"];
		MessageTemplate["text"]@Failure["MaTeXFailure", <||>],
	_,
		If[$DebugLevel > 0, Print@"IntEvaluate: UnexpectedFailure"];
		MessageTemplate["text"]@Failure["UnexpectedFailure", <||>]
]& //OneBot`Utilities`ConstrainedEvaluate


FigureEvaluate[message_] := First@StringCases[message[[-1]]["data", "text"], "fig"~~Whitespace~~e__~~WhitespaceCharacter...~~EndOfString :> e, 1] \
	//OneBot`Utilities`SafeToExpression //Switch[#,
	_Graphics|_Graphics3D|_Image,
		If[$DebugLevel > 0, Print@"FigureEvaluate: Rasterizing"];
		MessageTemplate["img"]@Rasterize[#, ImageResolution -> 200],
	_Failure,
		If[$DebugLevel > 0, Print@"FigureEvaluate: Failure"];
		MessageTemplate["text"]@#,
	_,
		If[$DebugLevel > 0, Print@"FigureEvaluate: Unexpected failure"];
		MessageTemplate["text"]@Failure["NotAFigure", <|"Content" -> ToString[#, InputForm]|>]
]& //OneBot`Utilities`ConstrainedEvaluate


TeXEvaluate[message_] := Switch[#,
	_Graphics,
		If[$DebugLevel > 0, Print@"TeXEvaluate: Rasterizing"];
		MessageTemplate["img"]@Rasterize[#, ImageResolution -> 200],
	$Failed,
		If[$DebugLevel > 0, Print@"TeXEvaluate: MaTeXFailure"];
		MessageTemplate["text"]@Failure["MaTeXFailure", <||>],
	_,
		If[$DebugLevel > 0, Print@"TeXEvaluate: UnexpectedFailure"];
		MessageTemplate["text"]@Failure["UnexpectedFailure", <||>]
]&@MaTeX`MaTeX[
	First@StringCases[message[[-1]]["data", "text"], "tex"~~Whitespace~~e__~~WhitespaceCharacter...~~EndOfString :> e, 1]
, Magnification -> 1.5] //OneBot`Utilities`ConstrainedEvaluate


$HelpMessage = MessageTemplate["text"]@StringTrim@"
\:529f\:80fd\:ff1a
help: \:663e\:793a\:5e2e\:52a9
wl expr_: \:8ba1\:7b97Wolfram\:8bed\:8a00\:8868\:8fbe\:5f0fexpr
int expr_ sym_: \:5bf9Wolfram\:8bed\:8a00\:8868\:8fbe\:5f0fexpr\:5173\:4e8eWolfram\:8bed\:8a00\:7b26\:53f7sym\:6c42\:53cd\:5bfc\:6570
tex formula_: \:6e32\:67d3TeX\:516c\:5f0fformula
fig expr_: \:5141\:8bb8\:56fe\:50cf\:8f93\:51fa

\:6ce8\:610f\:4e8b\:9879\:ff1a
1. Wolfram\:8bed\:8a00\:8868\:8fbe\:5f0f\:4e0d\:652f\:6301\:81ea\:7136\:8bed\:8a00\:8f93\:5165\:548c\:6709\:6b67\:4e49\:7684TraditionalForm\:3002
2. \:8981\:5728\:7fa4\:804a\:4e2d\:4f7f\:7528\:ff0c\:52a1\:5fc5\:8981\:5728\:6d88\:606f\:6700\:524d\:65b9at\:6211\:ff0c\:800c\:4e14\:8981\:5f62\:6210\:94fe\:63a5\:ff0c\:4e0d\:80fd\:662f\:7c98\:8d34\:800c\:6765\:7684\:7eaf\:6587\:672c\:3002
3. \:6d88\:8017\:8fc7\:591a\:8d44\:6e90\:6216\:5371\:9669\:7684\:547d\:4ee4\:662f\:4e0d\:88ab\:5141\:8bb8\:7684\:3002

\:793a\:4f8b\:6d88\:606f\:ff1a
@XXX help
@XXX wl 1+1
@XXX int 1/(1+x^6) x
@XXX tex \\frac{2}{3}
@XXX fig Graphics@Circle[]
"


(* ::Section:: *)
(*Handler*)


ApplyHandler[handler_, sourceMsg_] := handler@sourceMsg//Function[
	Switch[$DebugLevel,
		1,
			Print["ApplyHandler: ", handler, sourceMsg],
		2,
			Print["ApplyHandler: ", handler, sourceMsg];
			Print["Result: ", #]
	];
	#
]


QuickReplyResponse[handler_, sourceMsg_] := ExportString[GenerateHTTPResponse@HTTPResponse[
		ExportForm[{"reply" -> ApplyHandler[handler, sourceMsg]}, "JSON", "Compact" -> True]
	, <|"StatusCode" -> 200|>], "HTTPResponse"]

QuickReplyResponse[handler_, sourceMsg_, sourceMsgID_] := ExportString[GenerateHTTPResponse@HTTPResponse[
	ExportForm[{"reply" -> {MessageTemplate["reply"]@sourceMsgID, ApplyHandler[handler, sourceMsg]}}, "JSON", "Compact" -> True]
, <|"StatusCode" -> 200|>], "HTTPResponse"]


$NoActionResponse = ExportString[HTTPResponse["", <|"StatusCode" -> 204|>], "HTTPResponse"];


PrivateHandler[assoc_] := Module[{receive, messageType, message, messageID, selfID, senderID, response},
	receive = ImportByteArray[
		ImportByteArray[#DataByteArray,"HTTPRequest"]@"BodyByteArray",
		"RawJSON"
	, CharacterEncoding -> "UTF8"];
	{messageType, message, messageID, selfID, senderID} = receive/@{"message_type", "message", "message_id", "self_id", "user_id"};
	message = OneBot`Utilities`CatenateTextMessage@message;
	response = Switch[{messageType, message, senderID},
		{"private", {MessagePattern["help"]}},
			QuickReplyResponse[$HelpMessage&, message],
		{"private", {MessagePattern["admin"]}, $Administrator},
			QuickReplyResponse[AdminEvaluate, message],
		{"private", {MessagePattern["log"]}, $Administrator},
			QuickReplyResponse[LogEvaluate, message],
		{"private", {MessagePattern["wl"]}, _},
			QuickReplyResponse[WLEvaluate, message],
		{"private", {MessagePattern["tex"]}, _},
			QuickReplyResponse[TeXEvaluate, message],
		{"private", {MessagePattern["int"]}, _},
			QuickReplyResponse[IntEvaluate, message],
		{"private", {MessagePattern["fig"]}, _},
			QuickReplyResponse[FigureEvaluate, message],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["help"]}, _},
			QuickReplyResponse[$HelpMessage&, message],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["admin"]}, $Administrator},
			QuickReplyResponse[AdminEvaluate, message, messageID],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["log"]}, $Administrator},
			QuickReplyResponse[LogEvaluate, message, messageID],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["wl"]}, _},
			QuickReplyResponse[WLEvaluate, message, messageID],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["tex"]}, _},
			QuickReplyResponse[TeXEvaluate, message, messageID],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["int"]}, _},
			QuickReplyResponse[IntEvaluate, message, messageID],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["fig"]}, _},
			QuickReplyResponse[FigureEvaluate, message, messageID],
		_,
			If[$DebugLevel > 1, Print@"No action"];
			$NoActionResponse
	];
	Switch[$DebugLevel,
		1,
			Print["Writing response for: ", ToString[message, OutputForm], "\n"],
		2,
			Print["Writing response for: ", ToString[message, InputForm], "\n"],
		_,
			Print["Writing response\n"]
	];
	WriteString[#SourceSocket, response];
	Close@#SourceSocket;
]&@assoc;


(* ::Section:: *)
(*End*)


End[]


EndPackage[]
