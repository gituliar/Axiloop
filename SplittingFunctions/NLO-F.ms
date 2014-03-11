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

<< Axiloop`


$LO = AX$Get["LO.result"];

$propagator = 1/2 Expand[ GP[i1, i2, q] GV[i2, -q, i3, -l, i4, l+q] GP[i3, i5, l] GP[i4, i6, l+q] * GV[i5, l, mu, q, i6, -l-q] ];
$topology = x (G[n]/(4 k.n))**FP[k]**FV[i1]**FPx[p]**FV[nu]**FP[k] $propagator GPx[mu, nu, q];

$result = SplittingFunction[$topology, $LO];

(*
$ebsf = $Get[$result, "exclusive-bare-short"];
$n = I g^4 / ((1-x) k.k);
$ebsf = $n CollectFormFactors[$ebsf / $n];
$ebsf >> "NLO-F.ebs.mx";
*)
