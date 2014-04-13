#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["UnitTest`"];


UT$BeginTestCase["NLO-C"];

  AX$Get["NLO-C.ms"];


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

