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


UT$BeginTestCase["NLO-C"];

  AX$Get["NLO-C.ms"];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
    g^2 (4 Pi)^-2 (-3 + 4 I0 + 2 Log[x])
  ];


  UT$AssertEquivalent[
    $Get[$result, "inclusive"]
    ,
    (g/(4 Pi))^4 ((1+x^2)/(1-x) (-7 + 2 Log[x]^2 + 2 Log[x] Log[1-x] - 3 Log[1-x] + 2 Li2[1-x] + 4 Li2[1] - 4 I1 + 4 I0 Log[x] + 4 I0 Log[1-x]) - (1-x) (3 - 2 Log[x] - 4 I0) + x)
  ];


  $real = g^4 / Pi^4 ((1+x^2)/(1-x) ((Log[x])^2 + 2 Li2[1-x]) + 7 (1-x) + 2(1+x)Log[x] + 1 + 3/(1-x)Log[x]);
  $virt = Expand[Simplify[-256 $Get[$result, "inclusive"]]];
  $full = g^4 / Pi^4 ((1+x^2)/(1-x) (7 - 4 Li2[1] - (Log[x])^2 - 2 Log[x] Log[1-x] + 3/2 Log[x] + 3 Log[1-x] + 4 I1 - 4 I0 (Log[x] + Log[1-x])) + 7/2 (1+x) Log[x] + (1-x) (11 - 2 Log[x] - 4 I0));

  UT$AssertEquivalent[Simplify[Expand[$virt + $real - $full]], 0];


(*
  $$factors = ExtractFormFactors[$Get[$result, "exclusive-bare"]];

  $$k$0  = $Get[$$factors, "$$k$0"];
  $$k$ir = $Get[$$factors, "$$k$ir"];
  $$k$uv = $Get[$$factors, "$$k$uv"];
  $$p$uv = $Get[$$factors, "$$p$uv"];
  $$q$uv = $Get[$$factors, "$$q$uv"];


  UT$AssertEquivalent[ $$k$0,  4 I ((4 + 5 x + 5 x^2)/(2(1-x)) + (Log[x] + 2 I0) (1-x) + (2 I1 - 2 I0 Log[x] - Log[x]^2 - Li2[1-x] - 2 Li2[1]) (1+x^2)/(1-x)) ];
  UT$AssertEquivalent[ $$k$ir, 4 I ((1-x)^3 + (1+x^3) Log[x])/(1-x)^2 ];
  UT$AssertEquivalent[ $$k$uv, 4 I ((1-x) (1/2 + 2 x + x^2/2 - 2 I0 (1 + x^2)) - (2 - x + x^2) Log[x])/(1-x)^2 ];
  UT$AssertEquivalent[ $$p$uv, 0 ];
  UT$AssertEquivalent[ $$q$uv, 4 I ((1-x)^3 + (1 + x^3) Log[x])/(1-x)^2 ];

  UT$AssertEquivalent[
    Expand[$Get[$result, "inclusive"]]
    ,
    Expand[I (g/(4 Pi))^4 (($$k$uv + $$p$uv + $$q$uv) (Log[1-x]/2 + (1-x)^2/(1+x^2)) + $$k$0/2)]
  ];
*)

UT$EndTestCase[];
