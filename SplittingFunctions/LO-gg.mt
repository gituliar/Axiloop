#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["UnitTest`"];


UT$BeginTestCase["LO-gg"];

  AX$Get["LO-gg.ms"];

  UT$AssertEquivalent[
    $Get[$result, "G1"]
    ,
    (g^2/(8 Pi^2)) 2 Ca (1-x+x^2)^2/(x(1-x))
  ];

UT$EndTestCase[];
