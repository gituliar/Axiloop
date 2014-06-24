#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-qg-C"];

  $LO = AX$Get["LO-qg.mx"];
  TS$Print["$LO"];

  $vertex = Expand[ FV[i2]**FP[l+k]**FV[i3]**FP[-l]**FV[i1] GP[i1,i2,l+p] ];
  TS$Print["$vertex"];
  $topology = x Expand[ PFi[p]**FVc[i6]**FPx[p-k]**$vertex GP[i3,i4,k] PGo[i4,i5] GPc[i5,i6,k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  $result >> "NLO-qg-C.mx";
  TS$Print["$result"];

TS$End[];
