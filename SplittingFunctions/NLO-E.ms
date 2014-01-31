#!/usr/bin/math -script

<< Axiloop`


$LO = AX$Get["LO.result"];

$topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[k - l] ** GP[i1, i2, l] ** 
  FV[i2] ** FP[k] ** FV[mu] ** FPx[p] ** GPx[mu, nu, p - k] ** 
  FV[nu] ** FP[k];

$result = SplittingFunction[$topology, $LO];
