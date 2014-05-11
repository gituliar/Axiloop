#!/usr/bin/math -script

Needs["Axiloop`"];


$LO = AX$Get["LO-qq.mx"];

$topology = x PFo[k] ** FP[k] ** FV[i1] ** FP[k - l] ** GP[i1, i2, l] ** 
  FV[i2] ** FP[k] ** FV[mu] ** PFi[p] ** GPx[mu, nu, p - k] ** 
  FVc[nu] ** FPc[k];

$result = SplittingFunction[$topology, $LO];

$full = aspi^2 (pqq (7 - 4 Li2[1] - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0));
$real = 0;
$virt = aspi^2 (pqq (7 - 4 Li2[1] - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0));
