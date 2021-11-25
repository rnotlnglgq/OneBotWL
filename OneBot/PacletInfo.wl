(* ::Package:: *)

Paclet[
	Name -> "OneBot",
	Version -> "0.0.1",(* Rewrite the safeQ *)
	WolframVersion -> "12.0+",(* Not tested. *)
	Description -> "Wolfram Language interface for a HTTP server in OneBot standard.",
	Root -> ".",
	Loading -> Manual,
	Extensions -> {
		{
			"Kernel",
			Root -> ".",
			Context -> "OneBot`"
		}
	}
]
