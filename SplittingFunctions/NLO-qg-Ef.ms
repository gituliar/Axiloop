#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-qg-Ef"];

  $LO = AX$Get["LO-qg.mx"];
  TS$Print["$LO"];

  $vertex = Expand[ FV[i2, Line->f2]**FP[l, Line->f2]**FV[i7, Line->f2]**FP[l+k, Line->f2] ];
  $topology = x Expand[ PFi[p]**FV[i1]**FPx[q]**FV[i10] GP[i1,i2,k] $vertex GP[i7,i8,k] PGo[i8,i9] GP[i9,i10,k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  $result >> "NLO-qg-Ef.mx";
  TS$Print["$result"];

TS$End[];
