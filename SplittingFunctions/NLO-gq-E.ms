#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-gq-E"];

  $LO = AX$Get["LO-gq.mx"];
  TS$Print["$LO"];

  $topology = x Expand[ PGi[i1,i4,p] FV[i1]**FP[k]**FV[i2]**FP[l]**FV[i3]**FP[k]**PFo[k]**FP[k]**FV[i4]**FPx[p-k] GP[i2,i3,l+k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  $result >> "NLO-gq-E.mx";
  TS$Print["$result"];

TS$End[];
