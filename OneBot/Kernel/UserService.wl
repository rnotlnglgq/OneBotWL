(* ::Package:: *)

BeginPackage["OneBot`UserService`"]


ClearAll["`*"]


PrivateHandler
WLEvaluate
TeXEvaluate


$Debug = True;


Begin["`Private`"]


ClearAll["`*"]


(* ::Chapter:: *)
(*\:670d\:52a1*)


(* ::Section:: *)
(*\:6838\:5fc3\:670d\:52a1*)


OneBot`$MainHandler = PrivateHandler;


(* ::Section:: *)
(*\:7528\:6237\:670d\:52a1*)


WLEvaluate[message_] := MessageTemplate["text"]@ToString[
	OneBot`Utilities`ConstrainedEvaluate@OneBot`Utilities`AbsorbAbort@OneBot`Utilities`SafeToExpression@
		First@StringCases[message[[-1]]["data", "text"], "wl"~~Whitespace~~e__~~WhitespaceCharacter...~~EndOfString :> e, 1]
, InputForm]


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


PrivateHandler[assoc_] := Module[{receive, messageType, message, messageID, selfID, senderID, response},
	receive = ImportByteArray[
		ByteArray@ImportString[#Data,"HTTPRequest"]@"BodyBytes",
		"RawJSON"
	, CharacterEncoding -> "UTF8"];
	{messageType, message, messageID, selfID, senderID} = receive/@{"message_type", "message", "message_id", "self_id", "user_id"};
	response = Switch[{messageType, message, senderID},
		{"private", {MessagePattern["wl"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> WLEvaluate@message}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		{"private", {MessagePattern["tex"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> TeXEvaluate@message}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["wl"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> {MessageTemplate["reply"]@messageID, WLEvaluate@message}}, "JSON", "Compact" -> True]
			, <|"StatusCode" -> 200|>], "HTTPResponse"],
		{"group", {MessagePattern["at"]@selfID, MessagePattern["tex"]}, _},
			ExportString[GenerateHTTPResponse@HTTPResponse[
				ExportForm[{"reply" -> {MessageTemplate["reply"]@messageID, TeXEvaluate@message}}, "JSON", "Compact" -> True]
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


End[]


End[]
