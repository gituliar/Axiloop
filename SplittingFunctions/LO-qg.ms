#!/usr/bin/math -script

Needs["Axiloop`"];


$topology = 1/2 Cf x PFi[p]**FVc[nu]**FPx[p-k]**FV[mu] GP[mu,i1,k] PGo[i1,i2] GPc[i2,nu,k];

$result = SplittingFunction[$topology];
