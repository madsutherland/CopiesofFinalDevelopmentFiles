(* ::Package:: *)

(* :Title: DataImportandNormalization.m -- a package template *)

(* :Context: DataImportandNormalization.m` *)

(* :Author: Madeleine Sutherland *)

(* :Summary:
   This package contains commonly-needed functions for getting data imported from Excel into
   an acceptable format for analysis.
 *)

(* :Copyright: (C) <2018> by <MIT> *)

(* :Mathematica Version: 11.3 *)

(* :Keywords: data, normalization, package *)

(* :Warnings:
   <description of global effects, incompatibilities>
*)

(* :Limitations:
   <special cases not handled, known problems>
*)

(* :Discussion:
   <description of algorithm, information for experts>
*)

(* :Requirements:
   ProgrammingInMathematica/Package1.m
   ProgrammingInMathematica/Package2.m
   ProgrammingInMathematica/Package3.m
*)

(* :Examples:
   <sample input that demonstrates the features of this package>
*)


(* set up the package context, including public imports *)

BeginPackage["`DataImportandNormalization`"] 
	(*{"ProgrammingInMathematica`Package1`", "ProgrammingInMathematica`Package2`"}*)
ClearAll[Evaluate[Context[]<>"*"]]

(* usage messages for the exported functions and the context itself *)

DataImportandNormalization::usage = "DataImportandNormalization.m is a package that manages data imported 
from Excel. Functions: deleteZeroRows, deleteZeroColumns, ratioNormalize, logNormalize, randomTrainingTest, 
and makeAssociation."

deleteZeroRows::usage = "deleteZeroRows[matrix] removes rows of zeros and rows of nulls"
deleteZeroColumns::usage = "deleteZeroColumns[matrix] removes columns of zeros and columns of nulls"
ratioNormalize::usage = "ratioNormalize[list,position] takes the ratio of each element of {list} to
the specified element"
logNormalize::usage = "logNormalize[n,c] takes the natural log of n+c. Can be applied to matrices where c
is a positive number of magnitude greater than the lowest negative number in the data set"
randomTrainingTest::usage = "randomTrainingTest[list,n] splits the members of list into two sets of 
random indices, where n is the length of the training set"
makeAssociation::usage = "makeAssociation[list] takes every row of a matrix and converts it to an 
association between the first through second to last entries and the last entry"
Begin["`Private`"]    (* begin the private context (implementation part) *)
ClearAll[Evaluate[Context[]<>"*"]]

deleteZeroRows[m_]:=DeleteCases[m,{0..}|{Null..},1]
deleteZeroColumns[m_]:=Block[{Global`s},
Global`s=DeleteCases[Transpose[m],{0..}|{Null..},1];
Transpose[Global`s]]
ratioNormalize[list_,pos_ :4]:=Map[(#/#[[pos]])&,list,-2]
logNormalize[n_,c_]:=N@Log[n+c]
randomTrainingTest[list_,n_]:=Module[{s,t,training,testing},
s=RandomSample[Range[Length[list]],n];
t=Complement[Range[Length[list]],s];
{training,testing}={list[[s]],
list[[t]]};
{training,
testing}]
makeAssociation[list_]:=Normal[AssociationThread@Thread[list[[All,1;;-2]]->list[[All,-1]],list],Association]
End[ ]         (* end the private context *)

EndPackage[ ]  (* end the package context *)

































