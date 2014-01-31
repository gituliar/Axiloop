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


Get["Tests/core.mt"];


$LO = << "LO-gg.result";

$vertex = Expand[
    GV[i1, -p, i2, p - l, i3, l] GP[i3, i4, l] GV[i4, -l, i6, k, i5, l - k]
    GP[i2, i12, p - l] GV[i12, l - p, i10, k - l, mu, q] GP[i5, i10, l - k]];

$topology = Expand[ 
     x (1 - eps) GPx[i1, i11, p] GP[i6, i7, k] ({i7}.{i8}) GP[i8, i9, k]
     GV[i9, -k, nu, -q, i11, p] GPx[mu, nu, q]];

$result = SplittingFunction[$topology $vertex, $LO];



Test[
	$Get[$result, "Z"]
	,
	g^2/(8 Pi^2) (-11/3 + 6 I0 + 2 Log[1-x] + 2 Log[x])
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-D-gg-20140102-M1X6E0"
];


$virt = Expand[Simplify[$Get[$result, "inclusive"]]];

Test[
	$virt
	,
	g^4/(576 Pi^4 (-1 + x) x) (-134 - 144 I1 + 268 x + 288 I1 x - 405 x^2 - 432 I1 x^2 + 271 x^3 + 288 I1 x^3 - 134 x^4 - 144 I1 x^4 + 108 Li2[1] - 216 x Li2[1] + 324 x^2 Li2[1] - 216 x^3 Li2[1] + 108 x^4 Li2[1] + 36 (1 - x + x^2)^2 Log[1 - x]^2 + 72 I0 (1 - x + x^2)^2 Log[x] + 36 (1 - x + x^2)^2 Log[x]^2 + 6 (1 - x + x^2)^2 Log[1 - x] (-11 + 24 I0 + 6 Log[x]))
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-D-20140105-N3M2Z0"
];