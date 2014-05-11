#!/usr/bin/math -script

Needs["UnitTest`"];

UT$TestSuite[
  "NLO-gg-CD.mt",
  (* :BUG: NLO-gg-F test case fails when run within this test suite,
           however it works fine when run separately; should be fixed. *)
  "NLO-gg-F.mt"
];

