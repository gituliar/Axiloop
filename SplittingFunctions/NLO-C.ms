#!/usr/bin/math -script

<< Axiloop`
<< Axiloop`Core`

$LO = AX$Get["LO.result"];

$topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[l+k] ** FV[mu] **
  FP[l+p] ** FV[i2] ** GP[i1, i2, l] ** FPx[p] ** GPx[mu, nu, p-k] **
  FV[nu] ** FP[k];

$result = SplittingFunction[$topology, $LO];
