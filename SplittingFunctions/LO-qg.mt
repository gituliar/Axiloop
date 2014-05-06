#!/usr/bin/math -script

Needs["Axiloop`"];
Needs["UnitTest`"];


UT$BeginTestCase["LO-qg"];

  AX$Get["LO-qg.ms"];


  UT$AssertEquivalent[
    $Get[$result, "G1"]
    ,
    (g^2/(8 Pi^2)) Cf (2-2x+x^2)/x
  ];

UT$EndTestCase[];
