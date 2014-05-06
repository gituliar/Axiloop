#!/usr/bin/math -script

Needs["Axiloop`"];


$topology = 2 Nf Tf x PGi[mu,nu,p] FP[k]**FV[mu]**FPx[p-k]**FVc[nu]**FPc[k]**PFo[k];

$result = SplittingFunction[$topology];
