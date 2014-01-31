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

<< Logging`

<< Axiloop`


$LO = << "LO.result";

d = 4 + 2 eps;

$result = Expand[
  GammaTrace[(G[n]/(4 p.n)) ** FP[k] ** FV[i1] ** FPx[p] ** FV[nu] ** FP[k]]
  GammaTrace[ FV[i2] ** FP[l] ** FV[mu] ** FP[l + q] ] GP[i1, i2, q] GPx[mu, nu, q]
] /. { n.n -> 0, p.q -> -(k.k/2), k.p -> k.k/2, k.q -> -(k.k/2)};
(*
*)

$result = SplittingFunction[$result, $LO, IntegrateLoopPrescription -> "MPV"]
LG$Output[ $Get[$result, {"Z"}] ]
LG$Output[ Simplify[$Get[$result, {"counterterm"}]] ]
LG$Output[ Simplify[$Get[$result, {"exclusive"}]] ]
(*
LG$Output[ Collect[ 256 $Get[$result, {"exclusive-bare"}] /. {euv -> eps}, {Log[k.k], Log[1-x]}, Simplify] ]
LG$Output[ Collect[ 256 $Get[$result, {"inclusive"}], {Log[k.k], Log[1-x]}, Simplify] ]
*)

(*
LG$Output[ $Get[$result, {"simplified"}] ]
*)
