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


UT$BeginTestCase["NLO-G"];

  AX$Get["NLO-G.ms"];


  UT$AssertEquivalent[
    AX$Get["NLO-G.ebs.mx"]
    ,
    $Get[$result, "exclusive-bare-short"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
    I g^2 Qv (4/3)
  ];


  UT$AssertEquivalent[
    $Get[$result, "inclusive"]
    ,
    (g/(4 Pi))^4 ((1+x^2)/(1-x) (8/3 Log[1-x] + 8/3 Log[k.k]) + 8/3 (1-x))
  ];


  $W = SplittingFunctionFormFactors[ $Get[$result, "exclusive-bare"] ];
(*
  LG$Output[ Series[Simplify[$Get[$result, "exclusive-bare"] /. euv -> eps], {eps,0,0}] ];
*)

  UT$AssertEquivalent[
    "W_uv^q" /. $W
    ,
    I g^4 ( (1+x^2)/(1-x) (8/3) )
  ];

UT$EndTestCase[];
