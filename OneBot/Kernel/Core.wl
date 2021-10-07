(* ::Package:: *)

(* ::Title:: *)
(*OneBot*)


BeginPackage["OneBot`", {"GeneralUtilities`", "ZeroMQLink`", "CURLLink`"}]


ClearAll["`*"]


$Listener
$MainHandler


$DefaultOneBotDomain
$DefaultOneBotPort
$DefaultOneBotWLDomain
$DefaultOneBotWLPort


CallOneBot
SendPrivateMessage
SendGroupMessage


ReverseEscapeUnicode
ReverseEscapeCQCode
MessagePattern
MessageTemplate


StartListen
StopListen


Begin["`Private`"]


ClearAll["`*"]


(* ::Chapter:: *)
(*Request*)


$DefaultOneBotDomain = "127.0.0.1";
$DefaultOneBotPort = 5700;


CallOneBot[api_, params_, HoldPattern@OptionsPattern@{"Domain" -> $DefaultOneBotDomain, "Port" -> $DefaultOneBotPort}] :=
	URLRead@HTTPRequest[<|
		"Domain" -> OptionValue@"Domain",
		"Port" -> OptionValue@"Port",
		"Path" -> {"", api},
		"Query" -> params
	|>, <||>]


(* ::Chapter:: *)
(*Listen*)


$DefaultOneBotWLDomain = "127.0.0.1";
$DefaultOneBotWLPort = 5701;


StartListen[HoldPattern@OptionsPattern@{"Domain" -> $DefaultOneBotWLDomain, "Port" -> $DefaultOneBotWLPort}] := $Listener = SocketListen[
	StringTemplate["``:``"][OptionValue@"Domain", OptionValue@"Port"]
, $MainHandler]


StopListen[] := $Listener/@{"Socket", "SourceSocket"} //(
	DeleteObject@$Listener;
	Close/@#
)&


(* ::Chapter:: *)
(*Tool*)


(* ::Section:: *)
(*API*)


SendPrivateMessage[qid_, msg_] := CallOneBot["send_private_msg", {"user_id" -> qid, "message" -> msg}]


SendGroupMessage[gid_, msg_] := CallOneBot["send_group_msg", {"group_id" -> gid, "message" -> msg}]


(* ::Section:: *)
(*Parse*)


ReverseEscapeUnicode = StringReplace["\\u"~~u:(HexadecimalCharacter~~HexadecimalCharacter) :> Interpreter["HexInteger"]@u];


(* ::Text:: *)
(*Note: CQCode is currently not in use.*)


ReverseEscapeCQCode = StringReplace[{"&#91;"->"[", "&#93;"->"]", "&amp;"->"&", "&#44;" -> ","}];


(* ::Section:: *)
(*Pattern*)


MessagePattern["at"][qq_String] := <|
	"data" -> <|"qq" -> qq|>,
	"type" -> "at"
|>
MessagePattern["at"][qq_Integer] := MessagePattern["at"][IntegerString@qq]


MessagePattern["wl"] = <|
	"data" -> <|"text" -> s_String /; StringMatchQ[WhitespaceCharacter...~~"wl"~~Whitespace~~__]@s|>,
	"type" -> "text"
|>;


MessagePattern["tex"] = <|
	"data" -> <|"text" -> s_String /; StringMatchQ[WhitespaceCharacter...~~"tex"~~Whitespace~~__]@s|>,
	"type" -> "text"
|>;


(* ::Section:: *)
(*Template*)


MessageTemplate["text"][str_String] := <|
	"data" -> <|"text" -> str|>,
	"type" -> "text"
|>

MessageTemplate["text"][expr_] := MessageTemplate["text"]@ToString[expr, InputForm]


MessageTemplate["reply"][id_Integer] := <|
	"data" -> <|"id" -> id|>,
	"type" -> "reply"
|>

(*MessageTemplate["reply"][id_Integer] := MessageTemplate["text"][IntegerString@id]*)


MessageTemplate["img"][img_Image] := <|
	"data" -> <|"file" -> "base64://"<>BaseEncode@ExportByteArray[img, "PNG"]|>,
	"type" -> "image"
|>


(* ::Chapter:: *)
(*End*)


End[]


EndPackage[]
