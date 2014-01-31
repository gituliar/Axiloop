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


UT$BeginTestCase["NLO-D"];
  $$debug = False;

  Get["NLO-D.ms"];

  UT$AssertEquivalent[
    $Get[$result, "Z"]
    ,
    I g^2 (4 Pi)^-2 (3 - 8 I0 - 4 Log[1-x] - 2 Log[x])
  ];


  $full = I g^4/Pi^4 ((1+x^2)/(1-x) (-11 + 6 Li2[1] + Log[x]^2 + 2 Log[1-x]^2 + 6 Log[x] Log[1-x] - 3/2 Log[x] + 5 Log[1-x] - 8 I1 + 8 I0 + 8 I0 (Log[x] + Log[1-x])) + (1+x) Log[x]/2 + (1-x) (-1 + 8 I0 + 2 (Log[x] + 2 Log[1-x])));
  $real = I g^4/Pi^4 ((1+x^2)/(1-x) ((8 - 4 (Log[1-x] - Log[x])) (I0 + Log[1-x]) + 4 (I1 + (Log[1-x])^2/2) + 2 Li2[1-x] - 2 Li2[1] - Log[x]^2) + 1/(1-x) (-Log[x] - 2 x^2 Log[x] - 3 x (1+x) - 2));
  $virt = Expand[Simplify[$Get[$result, "inclusive"]]];

  UT$AssertEquivalent[$full - $real - (- 256 $virt), 0];

UT$EndTestCase[];