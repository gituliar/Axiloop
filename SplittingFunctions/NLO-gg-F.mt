#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["UnitTest`"];


UT$BeginTestCase["NLO-gg-F"];

  AX$Get["NLO-gg-F.ms"];


  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2 / k.k Pgg 4 (11/3 - 4 Log[1-x] - 4 I0)
  ];

  UT$AssertEquivalent[
    $Get[$result, "G1"]
    ,
    aspi^2 Pgg (Log[Q2] + Log[1-x])  (11/3 - 4 Log[1-x] - 4 I0)
  ];

UT$EndTestCase[];
