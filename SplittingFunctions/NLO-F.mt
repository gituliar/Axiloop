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

  pqq = (1+x^2)/(1-x);

  UT$AssertEquivalent[
    AX$Get["NLO-F.ebs.mx"]
    ,
    $Get[$result, "exclusive-bare-short"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
    I g^2 Qv (22/3 - 8 Log[1-x] - 8 I0)
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2 (pqq + (1-x) eps) (-44/3 + 16 Log[1-x] + 16 I0) / k.k
  ];

  $virt = 4 $Get[$result, "G1"];

  UT$AssertEquivalent[
    $virt,
    aspi^2 (-44/3 + 16 I0 + 16 Log[1-x]) (pqq (Log[1-x] + Log[Q^2]) + 1-x)
  ];

  $real = aspi^2 ( pqq (103/9 - 4 Li2[1] - 18 Log[1-x]^2 - 4 Log[x] Log[1-x] + 11/3 Log[x] + 32/3 Log[1-x] + 4 I1 - 4 I0 - 4 I0 Log[x] - 20 I0 Log[1-x] + (44/3 - 16 Log[1-x] - 16 I0) Log[Q^2]) + (1-x) (18 - 20 Log[1-x] - 20 I0));
  $full = aspi^2 ( pqq (103/9 - 4 Li2[1] - 2 Log[1-x]^2 - 4 Log[x] Log[1-x] + 11/3 Log[x] - 4 Log[1-x] + 4 I1 - 4 I0 (1 + Log[x] + Log[1-x]) ) + (1-x) (10/3 - 4 Log[1-x] - 4 I0));

  UT$AssertEquivalent[Simplify[Expand[$virt + $real - $full]], 0];


  $W = SplittingFunctionFormFactors[ $Get[$result, "exclusive-bare"] ];

  UT$AssertEquivalent[
    "W_uv^q" /. $W
    ,
    I g^4 ( pqq (44/3 - 16 I0 - 16 Log[1-x]) )
  ];

UT$EndTestCase[];
