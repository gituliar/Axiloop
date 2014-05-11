#!/usr/bin/math -script

Needs["Axiloop`"];


$LO = AX$Get["LO-qq.mx"];

$topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[l] ** FV[i2] ** 
  GP[i1, i3, l + k] ** GP[i2, i4, l + p] ** 
  GV[i3, -l - k, i4, l + p, mu, -p + k] ** FPx[p] ** 
  GPx[mu, nu, p - k] ** FV[nu] ** FP[k];

$result = SplittingFunction[$topology, $LO];


$full = aspi^2 (pqq (-11 + 6 Li2[1] + Log[x]^2 + 2 Log[1-x]^2 + 6 Log[x] Log[1-x] - 3/2 Log[x] + 5 Log[1-x] - 8 I1 + 8 I0 + 8 I0 (Log[x] + Log[1-x])) + (1+x) Log[x]/2 + (1-x) (-1 + 8 I0 + 2 (Log[x] + 2 Log[1-x])));
$real = aspi^2 (pqq (-4 - 3/2 Log[x] + (8 - 4 Log[1-x] + 4 Log[x])(I0 + Log[1-x]) + 4 I1 + 2 Log[1-x]^2 + 2 Li2[1-x] - 2 Li2[1] - Log[x]^2) + 3/2 (1-x) + (1+x) (1/2 + 1/2 Log[x]));
$virt = aspi^2 ( pqq (-7 + 8 Li2[1] + 2 Log[x]^2 - 2 Li2[1-x] + 4 I0 Log[x] + 4 I0 Log[1-x] - 12 I1 + (-3 + 2 Log[x] + 4 Log[1-x] + 8 I0) Log[1-x]) + (1-x) (-3 + 2 Log[x] + 4 Log[1-x] + 8 I0) - x)
