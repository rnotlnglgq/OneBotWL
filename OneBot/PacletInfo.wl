(* ::Package:: *)

Paclet[
	Name -> "OneBot",
	Version -> "0.0.3",(* add constant whitelist *)
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
