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

  pqq = (1+x^2)/(1-x);

  UT$AssertEquivalent[
    AX$Get["NLO-E.ebs.mx"]
    ,
    $Get[$result, "Wbs"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2/(-k.k) (pqq + (1-x) eps) (-6 + 8 Log[x] + 8 I0)
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wr"]
    ,
    as^2/(-k.k) (pqq + eps (1-x)) (((-k.k)^eps - 1) (-6 + 8 I0 + 8 Log[x]) / eps + (-k.k)^eps (14 - 8 Li2[1] - 4 Log[x]^2 - 8 I0 Log[x] + 8 I1 - 4 eps))
  ];

  $virt = 4 $Get[$result, "G1"];

  UT$AssertEquivalent[
    $virt
    ,
    aspi^2 (pqq (7 - 4 Li2[1] - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0))
  ];

  $real = 0;
  $full = aspi^2 (pqq (7 - 4 Li2[1] - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0));

  UT$AssertEquivalent[Simplify[Expand[$virt + $real - $full]], 0];


  $W = SplittingFunctionFormFactors[ $Get[$result, "Wb"] ];

  UT$AssertEquivalent[
    "W_0^k" /. $W
    ,
    I g^4 ( pqq (14 + 8 I1 - 8 Li2[1] - 8 I0 Log[x] - 4 Log[x]^2) - (1-x) (6 - 8 I0 - 8 Log[x]) )
  ];

  UT$AssertEquivalent[
    "W_uv^k" /. $W
    ,
    I g^4 ( pqq (-6 + 8 I0 + 8 Log[x]) )
  ];

UT$EndTestCase[];
