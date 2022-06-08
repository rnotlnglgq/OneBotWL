(* ::Package:: *)

(*
	Select[ToExpression[#, InputForm, OneBot`Utilities`ValueQWithAutoLoad]&]@Names["System`"~~Except["$"]~~Except["`"]..];
	StringTemplate["\"``\""]@StringRiffle[%, "\"\n\""]
*)


"Black"
"Blue"
"Brown"
"CloudExpression";(* BUG introduced by WRI *)
"ComplexInfinity"
"Cyan"
"Dashed"
"DeviceObject";(* just AutoLoad, however no need to use this *)
"DotDashed"
"Dotted"
"Gray"
"Green"
"Here";
"I"
"InfiniteFuture";
"InfinitePast";
"Infinity"
"LightBlue"
"LightBrown"
"LightCyan"
"LightGray"
"LightGreen"
"LightMagenta"
"LightOrange"
"LightPink"
"LightPurple"
"LightRed"
"LightYellow"
"Magenta"
"NotebookInformation";
"Now";
"Orange"
"Pink"
"Purple"
"Red"
"Space"
"StateSpaceModel"(* AutoLoad *)
"Tab"
"Thick"
"Thin"
"Today";
"Tomorrow";
"TransferFunctionModel"(* AutoLoad *)
"Transparent"
"White"
"Yellow"
"Yesterday";
