#!/usr/bin/math -script

Needs["UnitTest`"];

Needs["Axiloop`"];


UT$BeginTestCase["NLO-E"];


  $LO = AX$Get["LO.result"];

  $topology = x PFo[k] ** FP[k] ** FV[i1] ** FP[k - l] ** GP[i1, i2, l] ** 
    FV[i2] ** FP[k] ** FV[mu] ** PFi[p] ** GPx[mu, nu, p - k] ** 
    FVc[nu] ** FPc[k];

  $result = SplittingFunction[$topology, $LO];


  $full = aspi^2 (pqq (7 - 4 Li2[1] - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0));
  $real = 0;
  $virt = aspi^2 (pqq (7 - 4 Li2[1] - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0));

  pqq = (1+x^2)/(1-x);

  UT$AssertEquivalent[
    AX$Get["NLO-E.ebs.mx"]
    ,
    $Get[$result, "Wbs"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2/(-k.k) (pqq + (1-x) eps) (-6 + 8 Log[x] + 8 I0)
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wr"]
    ,
    as^2/(-k.k) (pqq + eps (1-x)) (((-k.k)^eps - 1) (-6 + 8 I0 + 8 Log[x]) / eps + (-k.k)^eps (14 - 8 Li2[1] - 4 Log[x]^2 - 8 I0 Log[x] + 8 I1 - 4 eps))
  ];


  UT$AssertEquivalent[
    4 $Get[$result, "G1"]
    ,
    $virt
  ];

  UT$AssertEquivalent[Simplify[Expand[$virt + $real - $full]], 0];


  $W = SplittingFunctionFormFactors[ $Get[$result, "Wb"] ];

  UT$AssertEquivalent[
    "W_0^k" /. $W
    ,
    I g^4 ( pqq (14 + 8 I1 - 8 Li2[1] - 8 I0 Log[x] - 4 Log[x]^2) - (1-x) (6 - 8 I0 - 8 Log[x]) )
  ];

  UT$AssertEquivalent[
    "W_uv^k" /. $W
    ,
    I g^4 ( pqq (-6 + 8 I0 + 8 Log[x]) )
  ];

UT$EndTestCase[];
