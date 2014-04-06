#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];
Needs["UnitTest`"];


TS$Begin["NLO-qg-CD"];
UT$BeginTestCase["NLO-qg-CD"];

  $LO = AX$Get["LO-qg.mx"];
  TS$Print["$LO"];

  $vertex = Expand[ FPx[p-k]**FV[mu]**FP[l+p]**FV[i1]  GP[i1,i2,l] GP[mu,i4,l+k] GV[i2,l, i3,k, i4,-l-k] ];
  TS$Print["$vertex"];
  $topology = x Expand[ PFi[p]**FVc[nu]**$vertex GPc[i3,i5,k] PGo[i5,i6] GP[i6,nu,k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  TS$Print["$result"];

UT$EndTestCase[];
TS$End[];
