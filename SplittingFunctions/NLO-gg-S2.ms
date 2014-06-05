#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-gg-S2"];

  $LO = AX$Get["LO-gg.mx"];
  TS$Print["$LO"];

  $vertex = Expand[ GV[mu,p-k, i1,-p, i2,-l, i3,l+k] GP[i2,i4,l] GP[i3,i5,l+k] GV[i4,l, i6,k, i5,-l-k] ];
  TS$Print["$vertex"];
  $topology = x Expand[ PGi[i1,i10,p] GV[i10,p, nu,-p+k, i9,-k] GPx[mu,nu,p-k] $vertex GP[i6,i7,k] PGo[i7,i8] GP[i8,i9,k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  TS$Print["$result"];

TS$End[];
