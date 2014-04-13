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
  "Axiloop`FeynmanRules`",
  "Axiloop`IntegrateLoop`"}];

  PaVeReduce::usage = ""


  Begin["`Private`"] ;

    PaVeReduce[lhs_, rhs_, basis_, vars_] := Block[
      {$lhs, $result},

      $PutOnShell[expr_] := Block[
        {},

        expr /. {n.n -> 0, k.p -> (k.k)/2}
             /. {p.p -> 0, Qv[p] -> 0}
             /. {q.q -> 0, Qv[q] -> 0}
      ];

      $equations = {};
      For[i = 1, i <= Length[basis], i++,
        ff = basis[[i]];    
        $lhs = $$CollectLoopIntegrals[lhs  ff, l];
        $lhs = $$SimplifyNumeratorAndDenominator[$lhs];
        $lhs = $$SimplifyTranslate[$lhs];
        $lhs = IntegrateLoopGeneral[$lhs, l];
        $lhs = $PutOnShell[ $lhs ];
        $lhs = $lhs /. {p.n -> 1, k.n -> x, q.n -> 1-x};

        $rhs = Simplify[rhs ff];
        $rhs = $PutOnShell[$rhs];
        $rhs = $rhs /. {p.n -> 1, k.n -> x, q.n -> 1-x};
    
        AppendTo[$equations, $lhs == $rhs];
      ];

      $result = Simplify[Expand[First[Solve[$equations, vars]]]] /. {Rule[e1_,e2_] :> Rule[e1, CollectFormFactors[$$ExpandPaVe[$PutOnShell[e2]] /. {eir -> eps, euv -> eps}] ]};
      $result = $result /. {Rule[e1_,e2_] :> Rule[e1, Collect[Normal[Series[$PutOnShell[$$ExpandMPV[e2]], {eps,0,0}]], 1/eps, Simplify]]};
      $result
    ];

  End[];

EndPackage[];
