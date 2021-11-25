(* ::Package:: *)

Begin["OneBot`Utilities`"]


$ThreadMemoryLimit = 1*^8;
$ThreadTimeLimit = 10;


Begin["`Private`"]


OneBot`Utilities`SafeEvaluate[expr_] := If[
	FreeQ[HoldComplete@expr, _Symbol?(Not@*OneBot`Utilities`SafeSymbolQ), {1, Infinity}],
	expr,
	Failure["Unsafe", <||>]
];

SetAttributes[OneBot`Utilities`SafeEvaluate, HoldAllComplete]


OneBot`Utilities`$SystemSymbols = First@*StringReplace[{StartOfString~~s__~~";"~~EndOfString :> False -> s, s__ :> True -> s}] /@ StringSplit[
	Import["SystemSymbol.wl", "String"]
, "\r\n"|"\n"] //Merge[Identity];


{OneBot`Utilities`$SystemWhiteList, OneBot`Utilities`$SystemBlackList} = OneBot`Utilities`$SystemSymbols /@ {True, False};


OneBot`Utilities`$ContextWhiteList = {};


OneBot`Utilities`SafeSymbolQ[sym_Symbol] := Catch@StackBegin@If[OneBot`Utilities`ValueQWithAutoLoad@sym,
	False,
	If[MemberQ[OneBot`Utilities`$ContextWhiteList]@Context@sym,
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


OneBot`Utilities`SafeToExpression[expr_] := ToExpression[expr, InputForm, OneBot`Utilities`SafeEvaluate];


OneBot`Utilities`AbsorbAbort[expr_] := CheckAbort[expr, $Aborted]

SetAttributes[OneBot`Utilities`AbsorbAbort, HoldAllComplete]


OneBot`Utilities`ConstrainedEvaluate[expr_] := MemoryConstrained[
	TimeConstrained[
		expr
	, OneBot`Utilities`$ThreadTimeLimit, OneBot`MessageTemplate["text"]@Failure["OutOfTime", <||>]]
, OneBot`Utilities`$ThreadMemoryLimit, OneBot`MessageTemplate["text"]@Failure["OutOfMemory", <||>]]

SetAttributes[OneBot`Utilities`ConstrainedEvaluate, HoldAllComplete]


End[]


End[]
