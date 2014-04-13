#!/usr/bin/math -script

Needs["Axiloop`"];


$topology = Cf x PFi[p]**FVc[nu]**FPc[k]**PFo[k]**FP[k]**FV[mu]**GPx[mu,nu,p-k];

$result = SplittingFunction[$topology];
