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

  pqq = (1+x^2)/(1-x);


  UT$AssertEquivalent[
    AX$Get["NLO-G.ebs.mx"]
    ,
    $Get[$result, "exclusive-bare-short"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
    I g^2 Qv (-4/3)
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2 (pqq + (1-x) eps) (8/3) / k.k
  ];


  $virt = 4 $Get[$result, "G1"];

  UT$AssertEquivalent[
    $virt
    ,
    aspi^2 8/3 ( pqq(Log[1-x] + Log[Q^2]) + (1-x))
  ];

  $real = aspi^2 (pqq (-10/9 - 2/3 Log[x] - 8/3 Log[1-x] - 8/3 Log[Q^2]) - 4 (1-x));
  $full = aspi^2 (pqq (-10/9 - 2/3 Log[x]) - 4/3 (1-x));

  UT$AssertEquivalent[Simplify[Expand[$virt + $real - $full]], 0];


  $W = SplittingFunctionFormFactors[ $Get[$result, "exclusive-bare"] ];

  UT$AssertEquivalent[
    "W_uv^q" /. $W
    ,
    I g^4 ( (1+x^2)/(1-x) (-8/3) )
  ];

UT$EndTestCase[];
