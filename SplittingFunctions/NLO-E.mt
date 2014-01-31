#!/usr/bin/math -script

(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2014 Oleksandr Gituliar.                                    *)
(*                                                                            *)
(*  This file is part of Axiloop.                                             *)
(*                                                                            *)
(*  Axiloop is free software: you can redistribute it and/or modify           *)
(*  it under the terms of the GNU General Public License as published by      *)
(*  the Free Software Foundation, either version 3 of the License, or         *)
(*  (at your option) any later version.                                       *)
(*                                                                            *)
(*  This program is distributed in the hope that it will be useful,           *)
(*  but WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the             *)
(*  GNU General Public License for more details.                              *)
(*                                                                            *)
(*  You should have received a copy of the GNU General Public License         *)
(*  along with this program.  If not, see <http://www.gnu.org/licenses/>.     *)
(*                                                                            *)
(*============================================================================*)

<< UnitTest`

<< Axiloop`


UT$BeginTestCase["NLO-E"];

  AX$Get["NLO-E.ms"];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
    I g^2 Qv (3 - 4 I0 - 4 Log[x])
  ];


  UT$AssertEquivalent[
    $Get[$result, "inclusive"]
    ,
    (g/(4 Pi))^4 ( (1+x^2)/(1-x) (7 - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] - 4 Li2[1] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0) )
  ];


(*
  $$factors = SplittingFunctionFormFactors[$Get[$result, "exclusive-bare"]];
  LG$Output[ $$factors ];

  $$k$0  = "W_0^k"  /. $$factors;
  $$k$ir = "W_ir^k" /. $$factors;
  $$k$uv = "W_uv^k" /. $$factors;
  $$p$uv = "W_uv^p" /. $$factors;
  $$q$uv = "W_uv^q" /. $$factors;

  UT$AssertEquivalent[
    Expand[$Get[$result, "inclusive"]]
    ,
    Expand[1/16 (($$k$uv + $$p$uv + $$q$uv) (Log[1-x]/2 + (1-x)^2/(1+x^2)) + $$k$0/2)]
  ];
*)

UT$EndTestCase[];
