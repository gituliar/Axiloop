#!/usr/bin/math -script

Needs["Axiloop`"];


$LO = AX$Get["LO-gg.mx"];

$propagator = 1/2 Expand[ GP[i7,i8,q] FV[i8]**FP[l]**FV[mu]**FP[l+q] GPx[mu,nu,q] ];
$topology = x Expand[ PGi[i2,i5,p] GV[i5,-p, i6,k, i7,q] GP[i6,i13,k] PGo[i13,i14] GP[i14,i1,k] GV[nu,-q, i1,-k, i2,p] ];


$result = SplittingFunction[$topology $propagator, $LO];
$result >> "NLO-gg-G.mx";
