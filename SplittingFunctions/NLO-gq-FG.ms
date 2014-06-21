#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-gq-FG"];

  $LO = AX$Get["LO-gq.mx"];
  TS$Print["$LO"];

  $topology = x Expand[ PGi[i1,i4,p] FV[i1]**FP[q]**FV[i2]**FP[l]**FV[i3]**FPx[q]**FV[i4]**FP[k]**PFo[k]**FP[k] GP[i2,i3,l+q] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  $result >> "NLO-gq-FG.mx";
  TS$Print["$result"];

TS$End[];
