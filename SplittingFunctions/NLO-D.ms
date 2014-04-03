#!/usr/bin/math -script

Needs["UnitTest`"];

Needs["Axiloop`"];


UT$BeginTestCase["NLO-D"];


  $LO = AX$Get["LO.result"];

  $topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[l] ** FV[i2] ** 
    GP[i1, i3, l + k] ** GP[i2, i4, l + p] ** 
    GV[i3, -l - k, i4, l + p, mu, -p + k] ** FPx[p] ** 
    GPx[mu, nu, p - k] ** FV[nu] ** FP[k];

  $result = SplittingFunction[$topology, $LO];

  pqq = (1+x^2)/(1-x);

  $full = aspi^2 (pqq (-11 + 6 Li2[1] + Log[x]^2 + 2 Log[1-x]^2 + 6 Log[x] Log[1-x] - 3/2 Log[x] + 5 Log[1-x] - 8 I1 + 8 I0 + 8 I0 (Log[x] + Log[1-x])) + (1+x) Log[x]/2 + (1-x) (-1 + 8 I0 + 2 (Log[x] + 2 Log[1-x])));
  $real = aspi^2 (pqq (-4 - 3/2 Log[x] + (8 - 4 Log[1-x] + 4 Log[x])(I0 + Log[1-x]) + 4 I1 + 2 Log[1-x]^2 + 2 Li2[1-x] - 2 Li2[1] - Log[x]^2) + 3/2 (1-x) + (1+x) (1/2 + 1/2 Log[x]));
  $virt = aspi^2 ( pqq (-7 + 8 Li2[1] + 2 Log[x]^2 - 2 Li2[1-x] + 4 I0 Log[x] + 4 I0 Log[1-x] - 12 I1 + (-3 + 2 Log[x] + 4 Log[1-x] + 8 I0) Log[1-x]) + (1-x) (-3 + 2 Log[x] + 4 Log[1-x] + 8 I0) - x)


  UT$AssertEquivalent[
    $Get[$result, "Wbs"]
    ,
    AX$Get["NLO-D.ebs.mx"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2/(-k.k) (pqq + (1-x) eps) (6 - 4 Log[x] - 8 Log[1-x] - 16 I0)
  ];

  UT$AssertEquivalent[
    4 $Get[$result, "G1"]
    ,
    $virt
  ];

  UT$AssertEquivalent[$full - $real - $virt, 0];


  $W = SplittingFunctionFormFactors[ $Get[$result, "Wb"] ];

  UT$AssertEquivalent[
    "W_0^k" /. $W
    ,
    I g^4 ( pqq (-14 - 24 I1 + 16 Li2[1] - 4 Li2[1-x] + 8 I0 Log[x] + 8 I0 Log[1-x] + 4 Log[x]^2) - (1+x) + (1-x) (7 - 16 I0 - 4 Log[x] - 8 Log[1-x]) )
  ];

  UT$AssertEquivalent[
    "W_ir^k" /. $W
    ,
    I g^4 ( pqq (3/2 + 2 Log[x] - 8 Log[1-x] - 10 I0) + 7/2 (1+x) + (1-x) )
  ];

  UT$AssertEquivalent[
    "W_uv^k" /. $W
    ,
    I g^4 ( pqq (9/2 - 6 Log[x] - 6 I0) - 7/2 (1+x) - (1-x) )
  ];

  UT$AssertEquivalent[
    "W_uv^p" /. $W
    ,
    I g^4 ( pqq (3/2 + 2 Log[x] - 2 Log[1-x] - 4 I0) + 3/2 (1+x) )
  ];

  UT$AssertEquivalent[
    "W_uv^q" /. $W
    ,
    I g^4 ( pqq (-6 I0 - 6 Log[1-x]) + 2 (1+x) + (1-x) )
  ];

UT$EndTestCase[];
