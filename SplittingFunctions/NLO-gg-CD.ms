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

$vertex = Block[
  {n,p,q},

  Unprotect[S];
    S[n,n] = 0;
    S /: g^n_Integer S[p,p] := 0;
    S /: g^n_Integer S[q,q] := 0;
  Protect[S];

  Expand[
    GV[i1, -p, i2, p - l, i3, l] GP[i3, i4, l] GV[i4, -l, i6, k, i5, l - k]
    GP[i2, i12, p - l] GV[i12, l - p, i10, k - l, mu, q] GP[i5, i10, l - k]
  ]
];

$topology = Expand[ 
  x (1 - eps) GPx[i1, i11, p] GP[i6, i7, k] ({i7}.{i8}) GP[i8, i9, k]
  GV[i9, -k, nu, -q, i11, p] GPx[mu, nu, q]
] /. {n.n -> 0, p.p -> 0, q.q -> 0};

$result = SplittingFunction[$topology $vertex, $LO];
$result >> "NLO-gg-CD.mx";
