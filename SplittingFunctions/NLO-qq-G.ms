#!/usr/bin/math -script

Needs["Axiloop`"];


$LO = AX$Get["LO-qq.mx"];

$topology = - x GammaTrace[PFo[k]**FP[k]**FV[i1]**PFi[p]**FV[nu]**FP[k]] GammaTrace[FV[i2]**FP[l]**FV[mu]**FP[l+q]] GP[i1,i2,q] GPx[mu,nu,q] ;

$result = SplittingFunction[$topology, $LO];

$full = aspi^2 (pqq (-20/9 - 4/3 Log[x]) - 8/3 (1-x));
$real = aspi^2 (pqq (-20/9 - 4/3 Log[x] + 8/3 Log[1-x] + 8/3 Log[Q2]));
$virt = aspi^2 (-8/3) (pqq (Log[1-x] + Log[Q2]) + (1-x));
