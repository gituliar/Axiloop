(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2012-2015 Oleksandr Gituliar.                               *)
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

Needs["AX$Vectors`", "Axiloop`Vectors`"];

BeginPackage["AX$IntegrateLoop`", {
  "AX$Vectors`"
  }];

  AX$FormFactor;

  AX$Loop::usage = "Loop integral";
  AX$LoopCollect;
  AX$LoopLorentzBasis;
  AX$LoopReduceLorentz::usage = "Reduce Lorentz structure in terms of Lorentz vector invariants and scalar form factors.";
  AX$LoopToExpression;

  Begin["`Private`"] 

    AX$LoopCollect::unevaluated = "`1`";

    AX$LoopCollect[expr_, l_, n1_:Null, n2_:Null] := Module[
      {$rules, $result},

      $rules = {
        (* Numerator *)
        AX$Loop[def__,{a___},{b___},{c___},{d___}] AX$S[l, aa_] :> AX$Loop[def,{a,aa},{b},{c},{d}]
        ,
        AX$Loop[def__,{a___},{b___},{c___},{d___}] AX$S[l, aa_]^n_ :> AX$Loop[def,Flatten[{a,x&/@Range[n]}],{b},{c},{d}] /; n>0
        ,

        (* Feynman denominator *)
        AX$Loop[def__,{a___},{b___},{c___},{d___}] / AX$S[l, l] :> AX$Loop[def,{a},{b,0},{c},{d}]
        ,
        AX$Loop[def__,{a___},{b___},{c___},{d___}] / AX$S[l+bb_Symbol, l+bb_Symbol] :> AX$Loop[def,{a},{b,bb},{c},{d}]
        ,
        AX$Loop[def__,{a___},{b___},{c___},{d___}] / AX$S[l-bb_Symbol, l-bb_Symbol] :> AX$Loop[def,{a},{b,-bb},{c},{d}]
        ,

        (* Axial denominators *)
        AX$Loop[def__,{a___},{b___},{c___},{d___}] AX$S[l, n1]^-1 :> AX$Loop[def,{a},{b},{c,0},{d}]
        ,
        AX$Loop[def__,{a___},{b___},{c___},{d___}] AX$S[l, n2]^-1 :> AX$Loop[def,{a},{b},{c},{d,0}]
        ,
        (*
        AX$Loop[{a___},{b___},{c___}] S[l+d_, n]^-1 :> AX$Loop[{a},{b},{c,d}]
        ,
        AX$Loop[{a___},{b___},{c___}] S[l-d_, n]^-1 :> AX$Loop[{a},{b},{c,-d}]
        ,
        AX$Loop[{a___},{b___},{c___}] S[-l+d_, n]^-1 :> - AX$Loop[{a},{b},{c,-d}]
        ,
        *)

        AX$Loop[def__,{},{},{},{}] -> 1
      };

      $result = Expand[expr AX$Loop[l,n1,n2,{},{},{},{}]] //. $rules;
      If[
        !FreeQ[result, AX$S[l,_]]
        ,
        Message[
            AX$LoopCollect::unevaluated,
            result
        ];
        Return[Null];
      ];

      $result = $result /. AX$Loop[def__,{a___},{b___},{c___},{d___}] :> AX$Loop[def,Sort[{a}],Sort[{b}],Sort[{c}],Sort[{d}]];

      $result
    ];


    AX$LoopIndices[integral:AX$Loop[l_,n1_,n2_,{a___},{b___},{c___},{d___}]] := Module[
      {$indices},

      $indices = Select[{a}, AX$IndexQ];

      $indices
    ];


    AX$LoopLorentzBasis[integral:AX$Loop[l_,n1_,n2_,{a___},{b___},{c___},{d___}]] := Module[
      {$basis, $indices, $vectors},

      $indices = AX$LoopIndices[integral];
      If[$indices == {}, Return[$indices];];

      $vectors = AX$LoopVectors[integral];

      $basis = Tuples[$vectors, {Length[$indices]}];
      $basis = Inner[AX$S, #, $indices, Times]& /@ $basis;

      $basis
    ];


    $AX$LoopReduceLorentz[integral:AX$Loop[l_,n1_,n2_,{a___},{b___},{c___},{d___}]] := Module[
      {$basis, $i, $result},

      $basis = AX$LoopLorentzBasis[integral];
      If[$basis == {}, Return[integral];];

      $result = 0;
      For[ $i=1, $i<=Length[$basis], $i++,
        $b = $basis[[$i]];
        $vs = Cases[$b, AX$S[k_?AX$VectorQ,mu_]:>k, {0,Infinity}];
        $result += $b AX$FormFactor[Sort[$vs], integral];
      ];

      $result
    ];

    AX$LoopReduceLorentz[expr_] := Module[
      {$integrals, $result, $rules},

      $integrals = Cases[expr, _AX$Loop, {0,Infinity}];

      $rules = (# -> $AX$LoopReduceLorentz[#])& /@ $integrals;
      $result = expr /. $rules;

      $result
    ];


    AX$LoopToExpression[expr_] := Module[
      {$result, $rules},

      $rules = {
        AX$Loop[l_,n1_,n2_,{a1_,a2___},{b___},{c___},{d___}] :> AX$Loop[l,n1,n2,{a2},{b},{c},{d}] AX$S[l,a1]
        ,
        AX$Loop[l_,n1_,n2_,{a___},{b1_,b2___},{c___},{d___}] :> AX$Loop[l,n1,n2,{a},{b2},{c},{d}] / AX$S[l+b1,l+b1]
        ,
        AX$Loop[l_,n1_,n2_,{a___},{b___},{c1_,c2___},{d___}] :> AX$Loop[l,n1,n2,{a},{b},{c2},{d}] / AX$S[l+c1,n1]
        ,
        AX$Loop[l_,n1_,n2_,{a___},{b___},{c___},{d1_,d2___}] :> AX$Loop[l,n1,n2,{a},{b},{c},{d2}] / AX$S[l+d1,n2]
        ,
        AX$Loop[def__,{},{},{},{}] :> 1
      };

      $result = expr //. $rules;

      $result
    ];


    AX$LoopVectors[integral:AX$Loop[l_,n1_,n2_,{a___},{b___},{c___},{d___}]] := Module[
      {$vectors},

      $vectors = Select[Cases[{b,If[{c}=!={},{n1,c}],If[{d}=!={},{n2,d}]}, _Symbol, Infinity], AX$VectorQ];

      $vectors
    ];

  End[];

EndPackage[];
