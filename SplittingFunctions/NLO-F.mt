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


UT$BeginTestCase["NLO-F"];

  AX$Get["NLO-F.ms"];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
    I g^2 Qv (22/3 - 8 I0 - 8 Log[1-x])
  ];

(*
  $inclusive$full = (g/(4 Pi))^4 ((1+x^2)/(1-x) (  103/9 - 4 Li2[1] - 2 Log[1-x]^2 - 4 Log[x] Log[1-x] + 11/3 Log[x] - 4 Log[1-x] + 4 I1 - 4 I0 (1 + Log[x] + Log[1-x]) ) + (1-x) (10/3 - 4 Log[1-x] - 4 I0));
  $inclusive$real = (g/(4 Pi))^4 ((1+x^2)/(1-x) ( - 68/9 - 4 Li2[1] + 6 Log[1-x]^2 - 4 Log[x] Log[1-x] + 11/3 Log[x] - 4 Log[1-x] + 4 I1 - 4 I0 (1 + Log[x] - Log[1-x]) ) + 2/3 x/(1-x));
*)

  UT$AssertEquivalent[
    1/4 $Get[$result, "inclusive"]
    ,
    (g/(4 Pi))^4 (11/3 - 4 I0 - 4 Log[1-x]) ((1+x^2)/(1-x) (Log[1-x] + Log[k.k]) + 1 - x)
  ];


  $W = SplittingFunctionFormFactors[ $Get[$result, "exclusive-bare"] ];

  UT$AssertEquivalent[
    "W_uv^q" /. $W
    ,
    I g^4 ( (1+x^2)/(1-x) (44/3 - 16 I0 - 16 Log[1-x]) )
  ];

UT$EndTestCase[];
