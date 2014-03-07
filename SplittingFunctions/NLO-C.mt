#!/usr/local/bin/math -script

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

  pqq = (1+x^2)/(1-x);


  UT$AssertEquivalent[
    AX$Get["NLO-C.ebs.mx"]
    ,
    $Get[$result, "exclusive-bare-short"]
  ];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
    I g^2 Qv (3 - 4 I0 - 2 Log[x])
  ];

  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2 / k.k (pqq + (1-x) eps) (-6 + 4 Log[x] + 8 I0)
  ];

  $virt = 4 $Get[$result, "G1"];

  UT$AssertEquivalent[
    $virt
    ,
    - aspi^2 ( pqq (7 - 2 Log[x]^2 - 2 Log[x] Log[1-x] + 3 Log[1-x] - 2 Li2[1-x] - 4 Li2[1] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 2 Log[x] - 4 I0) - x)
  ];

  $real = - aspi^2 (pqq ((Log[x])^2 + 2 Li2[1-x]) + 7 (1-x) + 2(1+x)Log[x] + 1 + 3/(1-x)Log[x]);
  $full =   aspi^2 (pqq (-7 + 4 Li2[1] + (Log[x])^2 + 2 Log[x] Log[1-x] - 3/2 Log[x] - 3 Log[1-x] - 4 I1 + 4 I0 (Log[x] + Log[1-x])) - 7/2 (1+x) Log[x] + (1-x) (-11 + 2 Log[x] + 4 I0));

  UT$AssertEquivalent[Simplify[Expand[$virt + $real - $full]], 0];


  $W = SplittingFunctionFormFactors[ $Get[$result, "exclusive-bare"] ];

  UT$AssertEquivalent[
    "W_0^k" /. $W
    ,
    I g^4 ( pqq (-14 - 8 I1 + 8 Li2[1] + 4 Li2[1-x] + 8 I0 Log[x] + 4 Log[x]^2) + (1+x) + (1-x) (5 - 8 I0 - 4 Log[x]) )
  ];

  UT$AssertEquivalent[
    "W_ir^k" /. $W
    ,
    I g^4 ( pqq (3/2 - 2 I0 + 2 Log[x]) + 2 (1-x) + 5/2 (1+x) )
  ];

  UT$AssertEquivalent[
    "W_uv^k" /. $W
    ,
    I g^4 ( pqq (9/2 - 6 I0 - 6 Log[x]) - 2 (1-x) - 5/2 (1+x) )
  ];

  UT$AssertEquivalent[
    "W_uv^q" /. $W
    ,
    I g^4 ( 2 Log[x] pqq + 2 (1-x) + (1+x) )
  ];

  UT$AssertEquivalent[
    "W_uv^p" /. $W
    ,
    I g^4 ( pqq (3/2 - 2 I0) + 3/2 (1+x) )
  ];

UT$EndTestCase[];
