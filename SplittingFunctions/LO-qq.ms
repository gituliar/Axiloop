#!/usr/bin/math -script

Needs["UnitTest`"];

Needs["Axiloop`"];


UT$BeginTestCase["LO-qq"];

  $topology = Cf x PFi[p]**FVc[nu]**FPc[k]**PFo[k]**FP[k]**FV[mu]**GPx[mu,nu,p-k];

  $result = SplittingFunction[$topology];


  UT$AssertEquivalent[
    $Get[$result, "G1"] /. {eps->0}
    ,
    (g^2/(8 Pi^2)) Cf (1+x^2)/(1-x)
  ];

UT$EndTestCase[];
