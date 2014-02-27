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


UT$BeginTestCase["NLO-gg-CD"];

  AX$Get["NLO-gg-CD.ms"];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
	I g^2 Qv (-22/3 + 12 I0 + 4 Log[1-x] + 4 Log[x])
  ];


  UT$AssertEquivalent[
    AX$Get["NLO-gg-CD.ebs.mx"]
    ,
    $Get[$result, "exclusive-bare-short"]
  ];

(*
$virt = Expand[Simplify[$Get[$result, "inclusive"]]];

Test[
	$virt
	,
	g^4/(576 Pi^4 (-1 + x) x) (-134 - 144 I1 + 268 x + 288 I1 x - 405 x^2 - 432 I1 x^2 + 271 x^3 + 288 I1 x^3 - 134 x^4 - 144 I1 x^4 + 108 Li2[1] - 216 x Li2[1] + 324 x^2 Li2[1] - 216 x^3 Li2[1] + 108 x^4 Li2[1] + 36 (1 - x + x^2)^2 Log[1 - x]^2 + 72 I0 (1 - x + x^2)^2 Log[x] + 36 (1 - x + x^2)^2 Log[x]^2 + 6 (1 - x + x^2)^2 Log[1 - x] (-11 + 24 I0 + 6 Log[x]))
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-D-20140105-N3M2Z0"
];
*)

UT$EndTestCase[];
