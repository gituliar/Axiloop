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


UT$BeginTestCase["NLO-D"];

  AX$Get["NLO-D.ms"];

  UT$AssertEquivalent[
   Simplify[
    $Get[$result, "exclusive-bare-short"] -
    I g^4/((1-x)k.k) (
      Qv[q] (-6(1+x^2 + (1-x)^2eps) C0[euv] +
        4x(1+x) C1[euv] +
        (3-6x-5x^2 + 3(1-x)^2eps) T0[euv]) +
      Qv[p] (-4(1+x^2 + (1-x)^2eps) B0[euv] +
        2x(x - (1-x)eps) B1[euv] -
        2(1+x^2 + (1-x)^2eps) D0[euv] +
        (3-2x^2 + (3-2x-2x^2)eps) T0[euv]) +
      Qv[k] (-2x^2(x-(1-x)eps) E1[euv] -
        4x^2(x-(1-x)eps) E2[euv] -
        2x^3(x-(1-x)eps) E3[euv] -
        6(1+x^2 + (1-x)^2eps) P0[euv] +
        2(1+x^2 + (1-x)^2eps) R0[eir] +
        (6-2x-x^2 + (4-8x+3x^2)eps) T0[eir] +
        x(2+7x - 3(2-x)eps) T0[euv] +
        2(1+x^2 + (1-x)^2eps) U0[eir]))
   ]
    , 0
  ];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
    - I g^2 Qv (3 - 8 I0 - 4 Log[1-x] - 2 Log[x])
  ];


  $full = aspi^2 ((1+x^2)/(1-x) (-11 + 6 Li2[1] + Log[x]^2 + 2 Log[1-x]^2 + 6 Log[x] Log[1-x] - 3/2 Log[x] + 5 Log[1-x] - 8 I1 + 8 I0 + 8 I0 (Log[x] + Log[1-x])) + (1+x) Log[x]/2 + (1-x) (-1 + 8 I0 + 2 (Log[x] + 2 Log[1-x])));
  $real = aspi^2 ((1+x^2)/(1-x) ((8 - 4 (Log[1-x] - Log[x])) (I0 + Log[1-x]) + 4 (I1 + (Log[1-x])^2/2) + 2 Li2[1-x] - 2 Li2[1] - Log[x]^2) + 1/(1-x) (-Log[x] - 2 x^2 Log[x] - 3 x (1+x) - 2));
  $virt = Expand[Simplify[$Get[$result, "inclusive"]]];

  UT$AssertEquivalent[$full - $real - 4 $virt, 0];


  $W = SplittingFunctionFormFactors[ $Get[$result, "exclusive-bare"] ];

  UT$AssertEquivalent[
    "W_0^k" /. $W
    ,
    I g^4 ( (1+x^2)/(1-x) (14 + 24 I1 - 16 Li2[1] + 4 Li2[1-x] - 8 I0 Log[x] - 8 I0 Log[1-x] - 4 Log[x]^2) + (1+x) + (1-x) (-7 + 16 I0 + 4 Log[x] + 8 Log[1-x]) )
  ];

  UT$AssertEquivalent[
    "W_ir^k" /. $W
    ,
    I g^4 ( (1+x^2)/(1-x) (-3/2 + 10 I0 - 2 Log[x] + 8 Log[1-x]) - 7/2 (1+x) - (1-x) )
  ];

  UT$AssertEquivalent[
    "W_uv^k" /. $W
    ,
    I g^4 ( (1+x^2)/(1-x) (-9/2 + 6 I0 + 6 Log[x]) + 7/2 (1+x) + (1-x) )
  ];

  UT$AssertEquivalent[
    "W_uv^p" /. $W
    ,
    I g^4 ( (1+x^2)/(1-x) (-3/2 + 4 I0 - 2 Log[x] + 2 Log[1-x]) - 3/2 (1+x) )
  ];

  UT$AssertEquivalent[
    "W_uv^q" /. $W
    ,
    I g^4 ( (1+x^2)/(1-x) (6 I0 + 6 Log[1-x]) - 2 (1+x) - (1-x) )
  ];

UT$EndTestCase[];
