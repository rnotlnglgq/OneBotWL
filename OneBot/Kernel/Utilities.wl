(* ::Package:: *)

Begin["OneBot`Utilities`"]


$ThreadMemoryLimit = 1*^8;
$ThreadTimeLimit = 10;


Begin["`Private`"]


OneBot`Utilities`SafeEvaluate[expr_] := If[
	FreeQ[HoldComplete@expr, _Symbol?OneBot`Utilities`SideEffectSymbolQ, {1, Infinity}],
	expr,
	Failure["Unsafe", <||>]
];

SetAttributes[OneBot`Utilities`SafeEvaluate, HoldAllComplete]


OneBot`Utilities`SideEffectSymbolQ[sym_Symbol] := If[OneBot`Utilities`ValueQWithAutoLoad@sym,
	True,
	Or[
		MemberQ[GeneralUtilities`$SideEffectfulFunctions, SymbolName@Unevaluated@sym],
		StringMatchQ["OneBot`"~~___]@Context@sym
	]
];

OneBot`Utilities`SideEffectSymbolQ[str_String] := ToExpression[str, InputForm, OneBot`Utilities`SideEffectSymbolQ]

OneBot`Utilities`SideEffectSymbolQ[_] := False

SetAttributes[OneBot`Utilities`SideEffectSymbolQ, HoldAllComplete]


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
	, $ThreadTimeLimit, OneBot`MessageTemplate["text"]@Failure["OutOfTime", <||>]]
, $ThreadMemoryLimit, OneBot`MessageTemplate["text"]@Failure["OutOfMemory", <||>]]

SetAttributes[OneBot`Utilities`ConstrainedEvaluate, HoldAllComplete]


End[]


End[]
