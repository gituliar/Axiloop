#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-qg-Eg"];

  $LO = AX$Get["LO-qg.mx"];
  TS$Print["$LO"];

  $vertex = Expand[ GV[i2,-k, i3,-l, i4,l+k] GP[i3,i5,l] GP[i4,i6,l+k] GV[i5,l, i7,k, i6, -l-k] ];
  $topology = x Expand[ PFi[p]**FV[i1]**FPx[q]**FV[i10] GP[i1,i2,k] $vertex GP[i7,i8,k] PGo[i8,i9] GP[i9,i10,k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  $result >> "NLO-qg-Eg.mx";
  TS$Print["$result"];

TS$End[];
