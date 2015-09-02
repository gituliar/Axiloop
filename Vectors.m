(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2015 Oleksandr Gituliar.                                    *)
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


BeginPackage["AX$Vectors`"];

  AX$indices;
  AX$vectors;

  AX$ContractIndices;
  AX$DeclareIndex;
  AX$DeclareVector;
  AX$IndexQ;
  AX$S::usage = "A scalar product of indices and/or vectors.";
  AX$VectorQ;

  Begin["`Private`"]

    AX$ContractIndices[expr_, indices___] := Module[
      {$i, $index, $indices, $result, $rules},

      $indices = {indices};
      If[$indices == {}, $indices = AX$indices];
      $result = expr;

      For[$i=1, $i<=Length[$indices], $i++,
        $index = $indices[[$i]];
        $rules = {
          AX$S[a_,$index] AX$S[b_,$index] :> AX$S[a,b]
          ,
          AX$S[a_,$index]^2 :> AX$S[a,a]
        };
        $result = $result /. $rules;
      ];

      $result
    ];

    AX$DeclareIndex[indices__] := Module[{},
      If[
        Head[AX$indices] =!= List
        ,
        AX$indices = {};
      ];

      AX$indices = Join[AX$indices, {indices}] // DeleteDuplicates;
    ];

    AX$DeclareVector[vectors__] := Module[{},
      If[
        Head[AX$vectors] =!= List
        ,
        AX$vectors = {};
      ];

      AX$vectors = Join[AX$vectors, {vectors}] // DeleteDuplicates;
    ];

    AX$IndexQ[index_] := MemberQ[AX$indices, index];

    SetAttribute[AX$S, Orderless];
    AX$S[x_] := AX$S[x,x];

    AX$VectorQ[vector_] := MemberQ[AX$vectors, vector];

  End[];

EndPackage[];
