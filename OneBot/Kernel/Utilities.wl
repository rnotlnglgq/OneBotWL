(* ::Package:: *)

Begin["OneBot`Utilities`"]


$ThreadMemoryLimit = 1*^8;
$ThreadTimeLimit = 10;


$ContextWhiteList = {"Global`"};


$SystemSymbols = First@*StringReplace[{StartOfString~~s__~~";"~~EndOfString :> False -> s, s__ :> True -> s}] /@ StringSplit[
	Import["SystemSymbol.wl", "String"]
, "\r\n"|"\n"] //Merge[Identity];


{$SystemWhiteList, $SystemBlackList} = OneBot`Utilities`$SystemSymbols /@ {True, False};


OneBot`Utilities`$UserSymbolWhiteList = {};


Begin["`Private`"]


OneBot`Utilities`SafeToExpression[str_] := OneBot`Utilities`EvaluateInTemporaryContext@Catch@CheckAbort[
	StackBegin@ToExpression[str, InputForm, OneBot`Utilities`SafeEvaluate]
, $Aborted];


OneBot`Utilities`EvaluateInTemporaryContext[expr_] := Block[{$Context = "Global`", $ContextPath = Prepend["System`"]@OneBot`Utilities`$ContextWhiteList},
	ClearAll/@Names@"Global`*";
	expr
]

SetAttributes[OneBot`Utilities`EvaluateInTemporaryContext, HoldAllComplete]


OneBot`Utilities`SafeEvaluate[expr_] := If[
	FreeQ[HoldComplete@expr, _Symbol?(Not@*OneBot`Utilities`SafeSymbolQ), {1, Infinity}],
	expr,
	Failure["Unsafe", <||>]
];

SetAttributes[OneBot`Utilities`SafeEvaluate, HoldAllComplete]


OneBot`Utilities`SafeSymbolQ[sym_Symbol] := If[OneBot`Utilities`ValueQWithAutoLoad@sym,
	False,
	If[MemberQ[Context@sym]@OneBot`Utilities`$ContextWhiteList || MemberQ[sym]@OneBot`Utilities`$UserSymbolWhiteList,
		True,
		If[Context@sym === "System`",
			MemberQ[SymbolName@Unevaluated@sym]@OneBot`Utilities`$SystemWhiteList,
			False
		]
	]
];

OneBot`Utilities`SafeSymbolQ[str_String] := ToExpression[str, InputForm, OneBot`Utilities`SafeSymbolQ]

(* Should introduce error here. *)
OneBot`Utilities`SafeSymbolQ[_] := False

SetAttributes[OneBot`Utilities`SafeSymbolQ, HoldAllComplete]


OneBot`Utilities`ValueQWithAutoLoad[sym_] := If[ValueQ[sym],
	FirstCase[
		OwnValues[sym],
		HoldPattern[
			Verbatim[RuleDelayed][
				Verbatim[HoldPattern][sym],
				Alternatives[
					Verbatim[Condition][System`Dump`AutoLoad[Verbatim[Hold][_],Verbatim[Hold][_],_String], System`Dump`TestLoad],
					Verbatim[Package`ActivateLoad][sym, {__Symbol}, _String, _]
				]
			]
		] :> (sym;ValueQ@sym),
		True
	],
	False
]

SetAttributes[OneBot`Utilities`ValueQWithAutoLoad, HoldAllComplete]


OneBot`Utilities`ConstrainedEvaluate[expr_] := MemoryConstrained[
	TimeConstrained[
		expr
	, OneBot`Utilities`$ThreadTimeLimit, OneBot`MessageTemplate["text"]@Failure["OutOfTime", <||>]]
, OneBot`Utilities`$ThreadMemoryLimit, OneBot`MessageTemplate["text"]@Failure["OutOfMemory", <||>]]

SetAttributes[OneBot`Utilities`ConstrainedEvaluate, HoldAllComplete]


OneBot`Utilities`CatenateTextMessage[msg_List] := SequenceReplace[msg,
	s:{<|"data" -> <|"text" -> _|>, "type" -> "text"|>..} :> (
		If[$DebugLevel > 1, Print["CatenateTextMessage: Catenating ", msg]];
		<|"data" -> <|"text" -> StringJoin[#[["data", "text"]]&/@s]|>, "type" -> "text"|>
	)
]


OneBot`Utilities`CatenateTextMessage[msg:Except@_List] := (
	If[$DebugLevel > 0, Print@"CatenateTextMessage: Not a list"];
	msg
)


End[]


End[]
