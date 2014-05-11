#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["UnitTest`"];

UT$BeginTestCase["NLO-qq-F"];
  
  AX$Get["NLO-qq-F.ms"];


  UT$AssertEquivalent[
    AX$Get["NLO-F.ebs.mx"]
    ,
    $Get[$result, "Wbs"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2/(-k.k) (pqq + (1-x) eps) (-22/3 + 8 Log[1-x] + 8 I0)
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wr"]
    ,
    as^2/(-k.k) (pqq + eps (1-x)) (22/3 - 8 Log[1-x] - 8 I0) / eps
  ];

  $virt = 4 $Get[$result, "G1"];

  UT$AssertEquivalent[
    $virt,
    aspi^2 (22/3 - 8 I0 - 8 Log[1-x]) (pqq (Log[1-x] + Log[Q2]) + 1-x)
  ];

  UT$AssertEquivalent[Simplify[Expand[$virt + $real - $full]], 0];


  $W = SplittingFunctionFormFactors[ $Get[$result, "Wb"] ];

  UT$AssertEquivalent[
    "W_uv^q" /. $W
    ,
    I g^4 ( pqq (-22/3 + 8 I0 + 8 Log[1-x]) )
  ];

UT$EndTestCase[];
