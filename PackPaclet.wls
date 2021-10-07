#!/usr/bin/env wolframscript
(* ::Package:: *)

paclet = CreatePacletArchive@"OneBot"

PacletInstall[paclet, ForceVersionInstall -> True] //Print

Pause[.5]