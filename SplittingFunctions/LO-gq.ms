#!/usr/bin/math -script

Needs["UnitTest`"];

Needs["Axiloop`"];


UT$BeginTestCase["LO-gq"];

  $topology = 2 Nf Tf x PGi[mu,nu,p] FP[k]**FV[mu]**FPx[p-k]**FVc[nu]**FPc[k]**PFo[k];

  $result = SplittingFunction[$topology];


  UT$AssertEquivalent[
    $Get[$result, "G1"] /. {eps->0}
    ,
    (g^2/(8 Pi^2)) Nf Tf (1-2x+2x^2)
  ];

UT$EndTestCase[];
