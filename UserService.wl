(* ::Package:: *)

BeginPackage["OneBot`UserService`", {"OneBot`", "Rubi`", "MaTeX`"}]


<<RubiSteps.wl


ClearAll["`*"]


PrivateHandler


QuickReplyResponse
$NoActionResponse


$Administrator


AdminEvaluate
WLEvaluate
TeXEvaluate
CallInt
IntEvaluate


$Debug = True;


Begin["`Private`"]


ClearAll["`*"]


(* ::Section:: *)
(*Dispatch*)


OneBot`$MainHandler = PrivateHandler;


(* ::Section:: *)
(*Tool*)


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


(* ::Section:: *)
(*Function*)


AdminEvaluate[message_] := MessageTemplate["text"]@ToString[
	ToExpression@First@StringCases[message[[-1]]["data", "text"], "admin"~~Whitespace~~e__~~WhitespaceCharacter...~~EndOfString :> e, 1]
, InputForm]


WLEvaluate[message_] := MessageTemplate["text"]@ToString[
	OneBot`Utilities`AbsorbAbort@OneBot`Utilities`SafeToExpression@
		First@StringCases[message[[-1]]["data", "text"], "wl"~~Whitespace~~e__~~WhitespaceCharacter...~~EndOfString :> e, 1]
, InputForm] //OneBot`Utilities`ConstrainedEvaluate


CallInt[expr_, var_] := MaTeX`MaTeX[
	RubiSteps`ShowIntSteps[Rubi`Int[expr, var], FormatType -> TeXForm]
, Magnification -> 1.5]


IntEvaluate[message_] := CallInt @@ OneBot`Utilities`AbsorbAbort@*OneBot`Utilities`SafeToExpression /@ StringCases[
		message[[-1]]["data", "text"],
		"int"~~Whitespace~~expr__~~Whitespace~~var:Except[WhitespaceCharacter]..~~WhitespaceCharacter...~~EndOfString :> {expr, var}
	, 1][[1]] //Switch[#,
	_Graphics,
		MessageTemplate["img"]@Rasterize[#, ImageResolution -> 200],
	$Failed,
		MessageTemplate["text"]@Failure["MaTeXFailure", <||>],
	_,
		MessageTemplate["text"]@Failure["UnexpectedFailure", <||>]
]& //OneBot`Utilities`ConstrainedEvaluate


TeXEvaluate[message_] := Switch[#,
	_Graphics,
		MessageTemplate["img"]@Rasterize[#, ImageResolution -> 200],
	$Failed,
		MessageTemplate["text"]@Failure["MaTeXFailure", <||>],
	_,
		MessageTemplate["text"]@Failure["UnexpectedFailure", <||>]
]&@MaTeX`MaTeX[
	First@StringCases[message[[-1]]["data", "text"], "tex"~~Whitespace~~e__~~WhitespaceCharacter...~~EndOfString :> e, 1]
, Magnification -> 1.5] //OneBot`Utilities`ConstrainedEvaluate


(* ::Section:: *)
(*Handler*)


QuickReplyResponse[handler_, sourceMsg_] := ExportString[GenerateHTTPResponse@HTTPResponse[
	ExportForm[{"reply" -> handler@sourceMsg}, "JSON", "Compact" -> True]
, <|"StatusCode" -> 200|>], "HTTPResponse"]

QuickReplyResponse[handler_, sourceMsg_, sourceMsgID_] := ExportString[GenerateHTTPResponse@HTTPResponse[
	ExportForm[{"reply" -> {MessageTemplate["reply"]@sourceMsgID, handler@sourceMsg}}, "JSON", "Compact" -> True]
, <|"StatusCode" -> 200|>], "HTTPResponse"]


$NoActionResponse = ExportString[HTTPResponse["", <|"StatusCode" -> 204|>], "HTTPResponse"];


PrivateHandler[assoc_] := Module[{receive, messageType, message, messageID, selfID, senderID, response},
	receive = ImportByteArray[
		ImportByteArray[#DataByteArray,"HTTPRequest"]@"BodyByteArray",
		"RawJSON"
	, CharacterEncoding -> "UTF8"];
	{messageType, message, messageID, selfID, senderID} = receive/@{"message_type", "message", "message_id", "self_id", "user_id"};
	response = Switch[{messageType, message, senderID},
		{"private", {MessagePattern["admin"]}, $Administrator},
			QuickReplyResponse[AdminEvaluate, message],
		{"private", {MessagePattern["wl"]}, _},
			QuickReplyResponse[WLEvaluate, message],
		{"private", {MessagePattern["tex"]}, _},
			QuickReplyResponse[TeXEvaluate, message],
		{"private", {MessagePattern["int"]}, _},
			QuickReplyResponse[IntEvaluate, message],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["admin"]}, _},
			QuickReplyResponse[AdminEvaluate, message, messageID],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["wl"]}, _},
			QuickReplyResponse[WLEvaluate, message, messageID],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["tex"]}, _},
			QuickReplyResponse[TeXEvaluate, message, messageID],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["int"]}, _},
			QuickReplyResponse[IntEvaluate, message, messageID],
		_,
			$NoActionResponse
	];
	If[TrueQ@$Debug,
		Print@ToString[message, InputForm]
	];
	WriteString[#SourceSocket, response];
	Close@#SourceSocket;
]&@assoc;


(* ::Section:: *)
(*End*)


End[]


EndPackage[]
