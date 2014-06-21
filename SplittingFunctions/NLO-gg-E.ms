#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["Timestamp`"];


TS$Begin["NLO-gg-E"];

  $LO = AX$Get["LO-gg.mx"];
  TS$Print["$LO"];

  $vertex = Expand[ GV[i8,k, i6,l, i7,-l-k] GP[i6,i4,l] GP[i5,i7,l+k] GV[i4,-l, i3,-k, i5,l+k] ];
  TS$Print["$vertex"];
  $topology = x Expand[ PGi[i1,i12,p] GV[i1,-p, i2,k, mu,p-k] GP[i2,i3,k] $vertex GP[i8,i9,k] PGo[i9,i10] GP[i10,i11,k] GV[i12,p, i11,-k, nu,-p+k] GPx[mu,nu,p-k] ];
  TS$Print["$topology"];

  $result = SplittingFunction[$topology, $LO];
  $result >> "NLO-gg-E.mx";
  TS$Print["$result"];

TS$End[];
