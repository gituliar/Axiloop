(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2013-2014 Oleksandr Gituliar.                               *)
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


BeginPackage["Axiloop`PaVeReduce`", {
  "Axiloop`Core`",
  "Axiloop`Integrate`"}];

  PaVeReduce::usage = ""


  Begin["`Private`"] ;

    PaVeReduce[lhs_, rhs_, basis_, vars_] := Block[
      {$lhs, $result},

      $PutOnShell[expr_] := Replace[
        Expand[expr] /. {
          S[x_,x_]^(eir) :> 0 /; (x == p || x == q)
          ,
          S[x_,x_]^(n_Integer+eir) :> 0 /; n > 0 && (x == p || x == q)
          ,
          S[x_,x_]^n_Integer :> 0 /; n > 1 && (x == p || x == q)
        }
        ,
        {p.p -> 0, q.q -> 0, n.n -> 0, (n.n)^2 -> 0, k.p -> (k.k)/2, (k.p)^n_ :> ((k.k)/2)^n}
        ,
        2
      ];


      $equations = {};
      For[i = 1, i <= Length[basis], i++,
        ff = basis[[i]];    
        $lhs = $$CollectLoopIntegrals[lhs  ff, l];
        $lhs = $$SimplifyNumeratorAndDenominator[$lhs];
        $lhs = IntegrateLoopGeneral[$lhs, l];
        $lhs = $PutOnShell[$lhs] /. {n.p -> 1, k.n -> x, n.q -> 1-x};
        (*
        $lhs = $lhs /. {eir -> 0};
        *)

        $rhs = Simplify[rhs ff];
        $rhs = $PutOnShell[$rhs];
        $rhs = $rhs /. {Global`d -> 4 - 2 eps, n.p -> 1, k.n -> x, n.q -> 1-x};
    
        AppendTo[$equations, $lhs == $rhs];
      ];

      $result = Simplify[Expand[First[Solve[$equations, vars]]]];
      (*
      Print[$result];
      *)
      $result = $result /. {Rule[e1_,e2_] :> Rule[e1, Simplify[$PutOnShell[$$ExpandMPV[e2]] /. {eir -> eps, euv -> eps}]]};
      $result
    ];

  End[];

EndPackage[];
