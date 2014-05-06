#!/usr/bin/math -script

Needs["Axiloop`"];


$topology = Ca x PGi[i2,i3,p] GV[i2,-p,i1,k,mu,p-k] GP[i1,i6,k] PGo[i6,i5] GPc[i5,i4,k] GVc[i4,-k,i3,p,nu,-p+k] GPx[mu,nu,p-k];

$result = SplittingFunction[$topology];
