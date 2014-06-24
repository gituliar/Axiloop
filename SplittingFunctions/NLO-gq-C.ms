#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-gq-C"];

  $LO = AX$Get["LO-gq.mx"];
  TS$Print["$LO"];

  $vertex = Expand[ FV[i2]**FP[-l]**FV[i1]**FP[l+p]**FV[i3] GP[i2,i3,l+k] ];
  $topology = x Expand[ PGi[i1,i4,p] $vertex**FPx[q]**FV[i4]**FP[k]**PFo[k]**FP[k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  $result >> "NLO-gq-C.mx";
  TS$Print["$result"];

TS$End[];
