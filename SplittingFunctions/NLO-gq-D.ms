#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-gq-D"];

  $LO = AX$Get["LO-gq.mx"];
  TS$Print["$LO"];

  $vertex = Expand[ FV[i4]**FP[l+k]**FV[i5] GP[i2,i4,l] GP[i3,i5,l+p] GV[i1,-p, i2,-l, i3,l+p] ];
  TS$Print["$vertex"];
  $topology = x Expand[ PGi[i1,i6,p] FP[k]**$vertex**FPx[p-k]**FV[i6]**FP[k]**PFo[k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  $result >> "NLO-gq-D.mx";
  TS$Print["$result"];

TS$End[];
