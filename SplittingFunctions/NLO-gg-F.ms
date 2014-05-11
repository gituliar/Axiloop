#!/usr/bin/math -script

Needs["Axiloop`"];


$LO = AX$Get["LO-gg.mx"];

$propagator = 1/2 Expand[ GP[i7,i8,q] GV[i8,-q, i9,-l, i10,l+q] GP[i9,i11,l] GP[i10,i12,l+q] GV[i11,l, mu,q, i12,-l-q] GPx[mu,nu,q] ];
$topology = x Expand[ PGi[i2,i5,p] GV[i5,-p, i6,k, i7,q] GP[i6,i13,k] PGo[i13,i14] GP[i14,i1,k] GV[nu,-q, i1,-k, i2,p] ];

(*
$real = aspi^2 (Pgg (4 Log[Q2] (11/3 - 8 Log[1-x] - 8 I0) - 8 I1 - 24 I0 Log[1-x] + 8 I0 Log[x] - 8 I0 - 28 Log[1-x]^2 + 8 Log[x] Log[1-x] + 64/3 Log[1-x] - 22/3 Log[x] + 4/3 Pi^2 + 134/9) - (668 x^4 - 1333 x^3 + 2010 x^2 - 1333 x + 668)/(18 (1-x) x));
$full = 0;
*)

$result = SplittingFunction[$topology $propagator, $LO];
$result >> "NLO-gg-F.mx";
