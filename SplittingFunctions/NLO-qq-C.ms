#!/usr/bin/math -script

Needs["Axiloop`"];


$LO = AX$Get["LO-qq.mx"];

$topology = x PFo[k]**FP[k]**FV[i1]**FP[l+k]**FV[mu]**FP[l+p]**FV[i2]**GP[i1,i2,l]**PFi[p]**GPx[mu,nu,p-k]**FVc[nu]**FPc[k];

pqq = (1+x^2)/(1-x);

$real = aspi^2 (pqq (-3/2 Log[x] - Log[x]^2 - 2 Li2[1-x]) + (1-x)(-15/2) + (1+x)(-1/2 - 7/2 Log[x]));
$full = aspi^2 (pqq (-7 + 4 Li2[1] - 3/2 Log[x] - 3 Log[1-x] + Log[x]^2 + 2 Log[x] Log[1-x] - 4 I1 + 4 I0 Log[x] + 4 I0 Log[1-x]) - 7/2 (1+x) Log[x] + (1-x) (-11 + 2 Log[x] + 4 I0));

$result = SplittingFunction[$topology, $LO];
