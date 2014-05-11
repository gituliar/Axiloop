#!/usr/bin/math -script

Needs["Axiloop`"];


$LO = AX$Get["LO-qq.mx"];
  
$propagator = 1/2 Expand[ GP[i1,i2,q] GV[i2,-q, i3,-l, i4,l+q] GP[i3,i5,l] GP[i4,i6,l+q] GV[i5,l, mu,q, i6,-l-q] ];
$topology = x PFo[k]**FP[k]**FV[i1]**PFi[p]**FV[nu]**FP[k] $propagator GPx[mu,nu,q];
  
$result = SplittingFunction[$topology, $LO];

$real = aspi^2 ( pqq (103/9 - 4 Li2[1] + 6 Log[1-x]^2 - 4 Log[x] Log[1-x] + 11/3 Log[x] - 34/3 Log[1-x] + 4 I1 - 4 I0 - 4 I0 Log[x] + 4 I0 Log[1-x] + (-22/3 + 8 Log[1-x] + 8 I0) Log[Q2]) + (1-x) (-4 + 4 Log[1-x] + 4 I0));
$full = aspi^2 ( pqq (103/9 - 4 Li2[1] - 2 Log[1-x]^2 - 4 Log[x] Log[1-x] + 11/3 Log[x] - 4 Log[1-x] + 4 I1 - 4 I0 (1 + Log[x] + Log[1-x]) ) + (1-x) (10/3 - 4 Log[1-x] - 4 I0));
