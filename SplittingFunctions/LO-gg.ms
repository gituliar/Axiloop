#!/usr/bin/math -script

Needs["UnitTest`"];

Needs["Axiloop`"];


UT$BeginTestCase["LO-gg"];

  $topology = Ca x PGi[i2,i3,p] GV[i2,-p,i1,k,mu,p-k] GP[i1,i6,k] PGo[i6,i5] GPc[i5,i4,k] GVc[i4,-k,i3,p,nu,-p+k] GPx[mu,nu,p-k];

  $result = SplittingFunction[$topology];


  UT$AssertEquivalent[
    $Get[$result, "G1"] /. {eps->0}
    ,
    (g^2/(8 Pi^2)) 2 Ca (1-x+x^2)^2/(x(1-x))
  ];

UT$EndTestCase[];
