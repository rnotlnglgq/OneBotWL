(* ::Package:: *)

BeginPackage["OneBot`UserService`", {"OneBot`", "Rubi`", "MaTeX`"}]


<<RubiSteps.wl


ClearAll["`*"]


PrivateHandler


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
, InputForm] //OneBot`Utilities`ConstrainedEvaluate


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


PrivateHandler[assoc_] := Module[{receive, messageType, message, messageID, selfID, senderID, response},
	receive = ImportByteArray[
		ByteArray@ImportString[#Data,"HTTPRequest"]@"BodyBytes",
		"RawJSON"
	, CharacterEncoding -> "UTF8"];
	{messageType, message, messageID, selfID, senderID} = receive/@{"message_type", "message", "message_id", "self_id", "user_id"};
	response = Switch[{messageType, message, senderID},
		{"private", {MessagePattern["admin"]}, $Administrator},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> AdminEvaluate@message}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		{"private", {MessagePattern["wl"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> WLEvaluate@message}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		{"private", {MessagePattern["tex"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> TeXEvaluate@message}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		{"private", {MessagePattern["int"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> IntEvaluate@message}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["wl"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> {MessageTemplate["reply"]@messageID, WLEvaluate@message}}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["tex"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> {MessageTemplate["reply"]@messageID, TeXEvaluate@message}}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["int"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> {MessageTemplate["reply"]@messageID, IntEvaluate@message}}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		_,
			ExportString[HTTPResponse["", <|"StatusCode" -> 204|>], "HTTPResponse"]
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
