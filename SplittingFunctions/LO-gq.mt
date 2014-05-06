#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["UnitTest`"];


UT$BeginTestCase["LO-gq"];

  AX$Get["LO-gq.ms"];


  UT$AssertEquivalent[
    $Get[$result, "G1"] /. {eps->0}
    ,
    (g^2/(8 Pi^2)) Nf Tf (1-2x+2x^2)
  ];

UT$EndTestCase[];
