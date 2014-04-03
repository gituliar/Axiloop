#!/usr/bin/math -script

Needs["UnitTest`"];

Needs["Axiloop`"];


UT$BeginTestCase["LO-qg"];

  $topology = 1/2 Cf x PFi[p]**FVc[nu]**FPx[p-k]**FV[mu] GP[mu,i1,k] PGo[i1,i2] GPc[i2,nu,k];

  $result = SplittingFunction[$topology];


  UT$AssertEquivalent[
    $Get[$result, "G1"] /. {eps->0}
    ,
    (g^2/(8 Pi^2)) Cf (2-2x+x^2)/x
  ];

UT$EndTestCase[];
