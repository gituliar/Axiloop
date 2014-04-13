#!/usr/bin/math -script

Needs["UnitTest`"];

Needs["Axiloop`"];


UT$BeginTestCase["LO-qq"];

  AX$Get["LO-qq.ms"];

  UT$AssertEquivalent[
    $Get[$result, "G1"] /. {eps->0}
    ,
    (g^2/(8 Pi^2)) Cf (1+x^2)/(1-x)
  ];

UT$EndTestCase[];

