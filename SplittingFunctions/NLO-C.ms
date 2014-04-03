#!/usr/bin/math -script

Needs["UnitTest`"];

Needs["Axiloop`"];


UT$BeginTestCase["NLO-C"];

  $LO = AX$Get["LO.result"];

  $topology = x PFo[k]**FP[k]**FV[i1]**FP[l+k]**FV[mu]**FP[l+p]**FV[i2]**GP[i1,i2,l]**PFi[p]**GPx[mu,nu,p-k]**FVc[nu]**FPc[k];

  $result = SplittingFunction[$topology, $LO];

  $real = aspi^2 (pqq (-3/2 Log[x] - Log[x]^2 - 2 Li2[1-x]) + (1-x)(-15/2) + (1+x)(-1/2 - 7/2 Log[x]));
  $full = aspi^2 (pqq (-7 + 4 Li2[1] - 3/2 Log[x] - 3 Log[1-x] + Log[x]^2 + 2 Log[x] Log[1-x] - 4 I1 + 4 I0 Log[x] + 4 I0 Log[1-x]) - 7/2 (1+x) Log[x] + (1-x) (-11 + 2 Log[x] + 4 I0));

  pqq = (1+x^2)/(1-x);


  UT$AssertEquivalent[
    $Get[$result, "Wbs"]
    ,
    AX$Get["NLO-C.ebs.mx"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2/(-k.k) (pqq + (1-x) eps) (6 - 4 Log[x] - 8 I0)
  ];

  $virt = 4 $Get[$result, "G1"];

  UT$AssertEquivalent[
    $virt
    ,
    aspi^2 ( pqq (-7 + 4 Li2[1] + 2 Log[x]^2 + 2 Li2[1-x] + 4 I0 Log[x] + (-3 + 2 Log[x] + 4 I0) Log[1-x] - 4 I1) + (1-x) (-3 + 2 Log[x] + 4 I0) + x)
  ];

  UT$AssertEquivalent[Simplify[Expand[$virt + $real - $full]], 0];


  $W = SplittingFunctionFormFactors[ $Get[$result, "Wb"] ];

  UT$AssertEquivalent[
    "W_0^k" /. $W
    ,
    I g^4 ( pqq (-14 - 8 I1 + 8 Li2[1] + 4 Li2[1-x] + 8 I0 Log[x] + 4 Log[x]^2) + (1+x) + (1-x) (5 - 8 I0 - 4 Log[x]) )
  ];

  UT$AssertEquivalent[
    "W_ir^k" /. $W
    ,
    I g^4 ( pqq (3/2 - 2 I0 + 2 Log[x]) + 2 (1-x) + 5/2 (1+x) )
  ];

  UT$AssertEquivalent[
    "W_uv^k" /. $W
    ,
    I g^4 ( pqq (9/2 - 6 I0 - 6 Log[x]) - 2 (1-x) - 5/2 (1+x) )
  ];

  UT$AssertEquivalent[
    "W_uv^q" /. $W
    ,
    I g^4 ( 2 Log[x] pqq + 2 (1-x) + (1+x) )
  ];

  UT$AssertEquivalent[
    "W_uv^p" /. $W
    ,
    I g^4 ( pqq (3/2 - 2 I0) + 3/2 (1+x) )
  ];

UT$EndTestCase[];
