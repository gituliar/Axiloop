#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-gg-S1"];

  $LO = AX$Get["LO-gg.mx"];
  TS$Print["$LO"];

  $vertex = Expand[ GV[mu,p-k, i6,-l+k, i5,l-p] GP[i4,i6,l-k] GP[i3,i5,l-p] GV[i1,-p, i2,k, i3,-l+p, i4,l-k] ];
  TS$Print["$vertex"];
  $topology = x Expand[ PGi[i1,i10,p] GV[i10,p, nu,-p+k, i9,-k] GPx[mu,nu,p-k] $vertex GP[i2,i7,k] PGo[i7,i8] GP[i8,i9,k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  TS$Print["$result"];

TS$End[];
