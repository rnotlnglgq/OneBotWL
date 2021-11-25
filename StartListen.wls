#!/usr/bin/env wolframscript
(* ::Package:: *)

<<UserService.wl
$Administrator = 123456
StartListen[]
OneBot`$Running = True
While[OneBot`$Running,Null]
