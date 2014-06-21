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

Needs["Axiloop`"];


$LO = AX$Get["LO-gg.mx"];

$vertex = Expand[ FV[i1]**FP[l]**FV[i2]**FP[l+k]**FV[mu]**FP[l+p] ];

$topology = Expand[ 
  x PGi[i1,i6,p] GP[i2,i3,k] PGo[i3,i4] GP[i4,i5,k] GV[i5,-k, i6,p, nu,-p+k] GPx[mu,nu,p-k]
] /. {n.n -> 0, p.p -> 0, q.q -> 0};

$result = SplittingFunction[$topology $vertex, $LO];
$result >> "NLO-gg-C.mx";
