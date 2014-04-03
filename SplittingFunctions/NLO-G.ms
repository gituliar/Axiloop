#!/usr/bin/math -script

Needs["UnitTest`"];

Needs["Axiloop`"];


UT$BeginTestCase["NLO-G"];

  $LO = AX$Get["LO.result"];

  $topology = - x GammaTrace[PFo[k]**FP[k]**FV[i1]**PFi[p]**FV[nu]**FP[k]] GammaTrace[FV[i2]**FP[l]**FV[mu]**FP[l+q]] GP[i1,i2,q] GPx[mu,nu,q] ;

  $result = SplittingFunction[$topology, $LO];

  pqq = (1+x^2)/(1-x);

  $full = aspi^2 (pqq (-20/9 - 4/3 Log[x]) - 8/3 (1-x));
  $real = aspi^2 (pqq (-20/9 - 4/3 Log[x] + 8/3 Log[1-x] + 8/3 Log[Q^2]));
  $virt = aspi^2 (-8/3) (pqq (Log[1-x] + Log[Q^2]) + (1-x));


  UT$AssertEquivalent[
    $Get[$result, "Wbs"]
    ,
    AX$Get["NLO-G.ebs.mx"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2/(-k.k) (pqq + (1-x) eps) 8/3
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wr"]
    ,
    as^2 / (-k.k) (pqq + (1-x) eps) (-8/3) / eps
  ];

  UT$AssertEquivalent[
    4 $Get[$result, "G1"]
    ,
    $virt
  ];

  UT$AssertEquivalent[Simplify[Expand[$virt + $real - $full]], 0];


  $Wb = SplittingFunctionFormFactors[ $Get[$result, "Wb"] ];

  UT$AssertEquivalent[
    "W_uv^q" /. $Wb
    ,
    I g^4 pqq 8/3
  ];

UT$EndTestCase[];
