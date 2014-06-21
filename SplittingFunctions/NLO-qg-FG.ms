#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-qg-FG"];

  $LO = AX$Get["LO-qg.mx"];
  TS$Print["$LO"];

  $topology = x Expand[ PFi[p]**FV[i1]**FP[q]**FV[i5]**FP[l]**FV[i6]**FPx[q]**FV[i4] GP[i5,i6,l+q] GP[i1,i2,k] PGo[i2,i3] GP[i3,i4,k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  $result >> "NLO-qg-FG.mx";
  TS$Print["$result"];

TS$End[];
