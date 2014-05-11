#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["UnitTest`"];


UT$BeginTestCase["NLO-qq-G"];

  AX$Get["NLO-qq-G.ms"];


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
