(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2012-2014 Oleksandr Gituliar.                               *)
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


BeginPackage["Axiloop`Integrate`", {
  "Axiloop`Core`",
  "Axiloop`Exception`",
  "Axiloop`FeynmanRules`",
  "Axiloop`Tracer`"}]

  $$::usage = ""

  eir::usage =
  "Infra-red (IR) eps pole."

  eps::usage =
  "Dimensional regulator in 4 + 2 eps dimensions."

  euv::usage =
  "Ultra-violet (UV) eps pole."

  CollectFormFactors::usage = ""

  ExtractFormFactors::usage = ""

  IntegrateLoop::usage =
  "Integrate over loop momenta."

  IntegrateLoopGeneral::usage = ""

  I0::usage =
  "Principal Value regulated integral; I0 = - Log[delta] + O[delta]."

  I1::usage =
  "Principal Value regulated integral; I1 = - (Log[delta]^2)/2 - Li2[1]/4 + O[delta]."

  Li2::usage =
  "Dilogarythm function; Li2[x] = - Integrate[Log[1-t]/t, {t, 0, x}]."

  Qv::usage = ""

  X0;Y0;B0;B1;B3;C0;C1;C3;D0;E0;E1;E2;E3;H1;H2;H3;H4;K0;P0;P1;P3;
  R0;R1;R2;R3;R4;R5;R6;
  H0;H1;H2;H3;H4;H5;H6;
  S0;S1;S2;S3;S4;S5;S6;S7;S8;S9;S10;
  U0;U1;U2;U3;U4;U5;U6;U7;U8;U9;U10;
  W0;W1;W2;W3;W4;W5;W6;W7;W8;W9;W10;
  T0;T1;T2;T3;V0;V1;V2;V3;U0;




  $$CollectLoopIntegrals::usage="CollectLoopIntegrals"
  $$ExpandLoopIntegrals::usage="ExpandLoopIntegrals"
  $$ExpandCommon::usage"ExpandCommon"
  $$ExpandMPV::usage"ExpandMPV"
  $$ExpandPV::usage"ExpandPV"
  $$ExpandPaVe::usage = ""
  $$SimplifyAlgebraic::usage="SimplifyAlgebraic"
  $$SimplifyNumeratorAndDenominator::usage="SimplifyNumeratorAndDenominator"
  $$SimplifyOnePoint::usage="SimplifyOnePoint"
  $$SimplifyTranslate::usage="SimplifyTranslate"


  Begin["`Private`"] 

    CollectFormFactors[expr_] := Block[
      {$result},

      $result = Collect[expr,
        {Qv[_], B0[_],B1[_],C0[_],C1[_],D0[_],E0[_],E1[_],E2[_],E3[_],K0[_],P0[_],P1[_],P3[_],R0[_],R1[_],R2[_],R3[_],R4[_],R5[_],S0[_],T0[_],T1[_],T2[_],U0[_],V1[_],V2[_]},
        Simplify
      ];

      $result
    ];

    ExtractFormFactors[expr_] := Block[{},
      DeleteDuplicates[Cases[expr, B0[_]|B1[_]|C0[_]|C1[_]|D0[_]|K0[_]|P0[_]|P1[_]|P2[_]|P3[_]|R0[_]|R1[_]|R2[_]|R3[_]|R4[_]|R5[_]|R6[_]|S0[_]|S1[_]|S2[_]|S3[_]|T2[_]|U0[_]|V1[_]|V2[_], Infinity]]
    ];

    $$CollectLoopIntegrals::unevaluated = "`1`";

    $$CollectLoopIntegrals[expr_, l_] := Module[
      {collectRules, result},

      collectRules = {
        $$[{a___},{b___},{c___}] S[l, x_] :> $$[{a,x},{b},{c}]
        ,
        $$[{a___},{b___},{c___}] S[l, x_]^n_ :>
            $$[Flatten[{a,x&/@Range[n]}],{b},{c}] /; n>0
        ,

        $$[{a___},{b___},{c___}] / S[l, l] :> $$[{a},{b,0},{c}]
        ,
        $$[{a___},{b___},{c___}] / S[l+x_Symbol, l+x_Symbol] :>
            $$[{a},{b,x},{c}]
        ,
        $$[{a___},{b___},{c___}] / S[l-x_Symbol, l-x_Symbol] :>
            $$[{a},{b,-x},{c}]
        ,

        $$[{a___},{b___},{c___}] S[l, n]^-1 :> $$[{a},{b},{c,0}]
        ,
        $$[{a___},{b___},{c___}] S[l+d_, n]^-1 :> $$[{a},{b},{c,d}]
        ,
        $$[{a___},{b___},{c___}] S[l-d_, n]^-1 :> $$[{a},{b},{c,-d}]
        ,
        $$[{a___},{b___},{c___}] S[-l+d_, n]^-1 :> - $$[{a},{b},{c,-d}]
        ,

        $$[{},{},{}] -> 1
      };

      result = Expand[expr $$[{},{},{}]]
        //. collectRules
        /. $$[{a___},{b___},{c___}] :> $$[Sort[{a}], Sort[{b}], Sort[{c}]
      ];


      If[
        !(FreeQ[result, Dot[l,_]] && FreeQ[result, Dot[_,l]])
        ,
        Message[
            $$CollectLoopIntegrals::unevaluated,
            result
        ];
        Raise[$UnevaluatedError];
      ];

      DEBUG[
        "$$CollectLoopIntegrals"
        ,
        ToString[#] &/@ Union[Cases[{result}, $$[__], Infinity]]
      ];

      result
    ];


    $$ExpandLoopIntegrals[expr_, l_] := Module[
      {expandRules},

      expandRules = {
        $$[{x_,a___},{b___},{c___}] :> $$[{a},{b},{c}] l.x,
        $$[{a___},{x_,b___},{c___}] :> $$[{a},{b},{c}] / (l+x).(l+x),
        $$[{a___},{b___},{x_,c___}] :> $$[{a},{b},{c}] / (l+x).n,
        $$[{},{},{}] -> 1
      };

      Expand[expr //. expandRules]
    ];


    $$SimplifyAlgebraic[expr_] := Module[
      {result, signRules, simplifyRules},

      signRules = {
          $$[{a___}, {b___}, {c___}] :>
              (-1)^(Length[Select[{a}, !MatchQ[#,l] &]]+Length[{c}]) $$[{a}, -# &/@ {b}, -# &/@ {c}] /;
                  {b} == Select[{b}, Or[# == 0, MatchQ[#, Times[-1, _Symbol]]] &]
      };

      simplifyRules = {
        $$[{a1___,q,a2___}, {b__}, {c___}] :> $$[Sort[{a1,p,a2}], {b}, {c}] - $$[Sort[{a1,k,a2}], {b}, {c}]
        ,

        (* 1/(l.n-x.n) 1/(l.n-y.n) *)
        $$[{a___}, {b___}, {x_,y_,c___}] :>
            ($$[{a}, {b}, {y,c}] - $$[{a}, {b}, {x,c}]) / (x.n-y.n)
        ,
        $$[x__, {k,p}] :> ($$[x, {k}] - $$[x, {p}]) / (p.n-k.n)
        ,

        (* l.n terms in the numerator *)
        $$[{n}, b_, {x_}] :>
            $$[{}, b, {}] - x.n $$[{}, b, {x}]
        ,
        $$[{a1___,n,a2___}, b_, {x_}] :>
            $$[{a1,a2}, b, {}] - x.n $$[{a1,a2}, b, {x}]
        ,

        (* l.l terms in the numerator *)
        $$[{l}, {k,p}, {c_}] :>
            $$[{}, {p}, {c}] - 2 $$[{k}, {k,p}, {c}] - k.k $$[{}, {k,p}, {c}]
      };

      result = expr /. signRules //. simplifyRules /. $$[{a___},{b___},{c___}] :> $$[Sort[{a}], Sort[{b}], Sort[{c}]];

      DEBUG[ "$$SimplifyAlgebraic" , ToString[#] &/@ Union[Cases[{result}, $$[__], Infinity]] ];

      Expand[result]
    ];


    $$SimplifyNumeratorAndDenominator[expr_] := Block[
      {result, signRules, simplifyRules},

      expr //. {
        $$[{a1___,q,a2___}, {b__}, {c___}] :>
          $$[Sort[{a1,p,a2}], {b}, {c}] - $$[Sort[{a1,k,a2}], {b}, {c}],
        $$[{a1___,n,a2___}, b_, {x_}] :>
          $$[{a1,a2}, b, {}] - x.n $$[{a1,a2}, b, {x}],
        $$[{a1___,p,a2___}, {0,k,p}, {c___}] :>
          1/2 ($$[{a1,a2}, {0,k}, {c}] - $$[{a1,a2}, {k,p}, {c}] - p.p $$[{a1,a2}, {0,k,p}, {c}]),
        $$[{a1___,k,a2___}, {0,k,p}, {c___}] ->
          1/2 ($$[{a1,a2}, {0,p}, {c}] - $$[{a1,a2}, {k,p}, {c}] - k.k $$[{a1,a2}, {0,k,p}, {c}])        
      } /. $$[{a___},{b___},{c___}] :> $$[Sort[{a}], Sort[{b}], Sort[{c}]]
    ];


    $$SimplifyOnePoint[expr_] := Module[
      {simplifyRules},

      simplifyRules = {
        $$[{}, {k}, {0}] -> $$[{}, {0}, {0}] - 2 $$[{k}, {0,k}, {0}] - k.k $$[{}, {0,k}, {0}]
        ,
        $$[{}, {q}, {0}] -> $$[{}, {0}, {0}] - 2 $$[{q}, {0,q}, {0}] - q.q $$[{}, {0,q}, {0}]
        ,
        $$[{},{0},{}] -> 0
      };

      Expand[expr /. simplifyRules]
    ];


    $$SimplifyTranslate[expr_] := Module[
      {result, simplifyRules},

      simplifyRules = {
        $$[{}, {k}, {}] -> $$[{}, {0}, {}]
        ,
        $$[{}, {q}, {}] -> $$[{}, {0}, {}]
        ,

        $$[{}, {p,q}, {}] -> $$[{}, {0,k}, {}]
        ,

        $$[{x_,y_},{k,p},{}] :> $$[{x,y},{0,q},{}] - k.y $$[{x},{0,q},{}] - k.x $$[{y},{0,q},{}] + k.x k.y $$[{},{0,q},{}]
        ,


        $$[{}, {p}, {p}] -> $$[{}, {0}, {0}]
        ,
        $$[{}, {q}, {q}] -> $$[{}, {0}, {0}]
        ,
        $$[{}, {p}, {k}] -> $$[{}, {q}, {0}]
        ,

        $$[{ },{0,k},{k}] -> - $$[{ },{0, k},{0}]
        ,
        $$[{ },{0,k},{p}] -> - $$[{ },{p, q},{0}]
        ,
        $$[{ },{0,p},{k}] -> - $$[{ },{k,-q},{0}]
        ,
        $$[{ },{0,p},{p}] -> - $$[{ },{0, p},{0}]
        ,
        $$[{ },{0,q},{q}] -> - $$[{ },{0, q},{0}]
        ,
        $$[{ },{k,p},{k}] ->   $$[{ },{0, q},{0}],
        $$[{ },{k,p},{p}] -> - $$[{ },{0, q},{0}],

        $$[{k},{0,k},{k}] ->   $$[{k},{0, k},{0}] + k.k $$[{},{0, k},{0}],
        $$[{k},{0,k},{p}] ->   $$[{k},{p, q},{0}] + k.p $$[{},{p, q},{0}],
        $$[{k},{0,p},{k}] ->   $$[{k},{k,-q},{0}] + k.k $$[{},{k,-q},{0}],
        $$[{k},{0,p},{p}] ->   $$[{k},{0, p},{0}] + k.p $$[{},{0, p},{0}],
        $$[{k},{k,p},{k}] ->   $$[{k},{0, q},{0}] - k.k $$[{},{0, q},{0}],
        $$[{k},{k,p},{p}] ->   $$[{k},{0, q},{0}] + k.p $$[{},{0, q},{0}],
        $$[{p},{0,k},{k}] ->   $$[{p},{0, k},{0}] + p.k $$[{},{0, k},{0}],
        $$[{p},{0,k},{p}] ->   $$[{p},{p, q},{0}] + p.p $$[{},{p, q},{0}],
        $$[{p},{0,p},{k}] ->   $$[{p},{k,-q},{0}] + p.k $$[{},{k,-q},{0}],
        $$[{p},{0,p},{p}] ->   $$[{p},{0, p},{0}] + p.p $$[{},{0, p},{0}],
        $$[{p},{k,p},{k}] ->   $$[{p},{0, q},{0}] - p.k $$[{},{0, q},{0}],
        $$[{p},{k,p},{p}] ->   $$[{p},{0, q},{0}] + p.p $$[{},{0, q},{0}],

        $$[{k},{0,q},{q}] ->   $$[{k},{0, q},{0}] + k.q $$[{},{0, q},{0}],
        $$[{p},{0,q},{q}] ->   $$[{p},{0, q},{0}] + p.q $$[{},{0, q},{0}]
      };

      result = Expand[expr /. simplifyRules /. {k.n -> x, n.p -> 1, n.q -> 1-x}];

      DEBUG[ "$$SimplifyTranslate" , ToString[#] &/@ Union[Cases[{result}, $$[__], Infinity]] ];

      result
    ];


    IntegrateLoopGeneral::unevaluated = "`1`"

    IntegrateLoopGeneral[expr_, l_] := Module[
      {integrateRules, result, unevaluated},

      integrateRules = {
        $$[{},{0},{ }] -> 0,
        $$[{},{0},{0}] -> 0,
        $$[{},{p},{0}] -> 0,

        $$[{},{0,k},{ }] ->   Qv[k] T0[euv],
        $$[{},{0,p},{ }] ->   Qv[p] T0[euv],
        $$[{},{0,q},{ }] ->   Qv[q] T0[euv],
        $$[{},{k,p},{ }] ->   Qv[q] T0[euv],

        $$[{x_},{0,k},{}] :> - Qv[k] k.x T1[euv],
        $$[{x_},{0,p},{}] :> - Qv[p] p.x T1[euv],
        $$[{x_},{0,q},{}] -> - Qv[q] q.x T1[euv],
        $$[{x_},{k,p},{}] :> - Qv[q] (p.x + k.x) T1[euv],

        $$[{x_,y_},{0,z_},{}] :> Qv[z] (x.z y.z T2[euv] + x.y z.z T3[euv]),

        $$[{},{0,k,p},{}]       -> Qv[k] (k.k)^-1 R0[eir],
        $$[{x_},{0,k,p},{}]     :> Qv[k] (k.k)^-1 (p.x R1[eir] + k.x R2[eir]),
        $$[{x_, y_},{0,k,p},{}] :> Qv[k] (k.k)^-1 (p.x p.y R3[eir] + k.x k.y R4[eir] + (k.x p.y + p.x k.y) R5[eir] + k.k x.y R6[euv]),

        $$[{x_,y_,z_},{0,k,p},{}] :> Qv[k] (k.k)^-1 (
                 p.x p.y p.z H1[eir]
               + (p.x p.y k.z + p.x k.y p.z + k.x p.y p.z) H2[eir]
               + (p.x k.y k.z + k.x p.y k.z + k.x k.y p.z) H3[eir]
               + k.x k.y k.z H4[eir]
               + k.k (p.x y.z + p.y x.z + p.z x.y) H5[euv]
               + k.k (k.x y.z + k.y x.z + k.z x.y) H6[euv]
           ),

        $$[{},{0, k},{0}] -> - Qv[k] P0[euv] / k.n,
        $$[{},{0, p},{0}] -> - Qv[p] B0[euv] / n.p,
        $$[{},{0, q},{0}] -> - Qv[q] C0[euv] / n.q,
        $$[{},{k, p},{0}] -> - Qv[q] K0[euv] / n.q,
        $$[{},{k,-q},{0}] ->   Qv[p] D0[euv] / n.p, (* sign ??? *)
        $$[{},{p, q},{0}] -> - Qv[k] E0[euv] / n.p,

        $$[{x_},{0,k},{0}] :> Qv[k] 1/k.n (k.x P1[euv] + n.x k.k/(2 k.n) P3[euv]),
        $$[{x_},{0,p},{0}] :> Qv[p] 1/p.n (p.x B1[euv] + n.x p.p/(2 p.n) B3[euv]),
        $$[{x_},{0,q},{0}] :> Qv[q] 1/q.n (q.x C1[euv] + n.x q.q/(2 q.n) C3[euv]),
        (* tego nie ma u MS: wydaje sie ze brakuje ogolnego 1/qn oraz 1/qn przy czlonie V3 -- ale to chyba i tak nie jest uzywane bo q^2=0 *)
        $$[{x_},{k,p},{0}] :> Qv[q] (p.x V1[euv] + k.x V2[euv] + n.x q.q/2 V3[euv]),
         (* tego nie ma u MS: wydaje sie ze brakuje 1/kn przy czlonie E3 (k^2\[NotEqual]0 !!!) *)
        $$[{x_},{p,q},{0}] :> Qv[k] 1/p.n (p.x E1[euv] + k.x E2[euv] + n.x k.k/2 E3[euv]),
        (* tego nie ma u MS: co z wymiarem czlonu przy F3 (p^2=0) *)
        $$[{x_},{k,-q},{0}] :> Qv[p] 1/p.n (p.x F1 + k.x F2 + n.x p.p/2 F3),

        (* tego nie ma u MS: czy to jest wogole uzywane??  *)
        $$[{p,p}, {0,p}, {k}] -> Qv[p] X0
        ,
        (* tego nie ma u MS: czy to jest wogole uzywane?? *)
        $$[{p,p}, {k,p}, {k}] -> Qv[q] Y0
        ,

        $$[{},{0,k, p},{0}] -> - Qv[k] (k.k)^-1 / p.n S0[eir],

        (* calka S3 jest inna niz u MS ale blad jest u MS !!! (same UV part) *)
        $$[{x_},{0,k,p},{0}] :> Qv[k] (k.k)^-1 / p.n (p.x S1[eir] + k.x S2[eir] + n.x k.k/(2 k.n) S3[euv]),

        (* tego nie ma u MS *)
        $$[{x_, y_},{0,k,p},{0}] :> Qv[k] (k.k)^-1 / q.n (
              S4[eir] p.x p.y
            + S5[eir] (p.x k.y + k.x p.y)
            + S6[eir] k.x k.y
            + S7[euv]  (p.x n.y + n.x p.y) k.k/(2 p.n)
            + S8[euv]  (k.x n.y + n.x k.y) k.k/(2 k.n)
            + S9[euv]  n.x n.y (k.k/(2 k.n))^2
            + S10[euv] x.y k.k
        ),

        (* tego nie ma u MS: jest to samo po podstawieniu: l \[Rule] l+k; ie jestem tylko pewny zanku  *)
        $$[{},{0,k,p},{k}] -> - Qv[k] (k.k)^-1 / q.n U0[eir],

        (* tego nie ma u MS *)
        $$[{x_},{0,k,p},{k}] :> Qv[k] (k.k)^-1 / q.n (p.x U1[eir] + k.x U2[eir] + n.x k.k/(2 k.n) U3[euv]),

        (* tego nie ma u MS *)    
        $$[{x_, y_},{0,k,p},{k}] :> Qv[k] (k.k)^-1 / q.n (
              U4[eir] p.x p.y
            + U5[eir] (p.x k.y + k.x p.y)
            + U6[eir] k.x k.y
            + U7[euv]  (p.x n.y + n.x p.y) k.k/(2 p.n)
            + U8[euv]  (k.x n.y + n.x k.y) k.k/(2 k.n)
            + U9[euv]  n.x n.y (k.k/(2 k.n))^2
            + U10[euv] x.y k.k
        ),

        (* tego nie ma u MS *)
        $$[{},{0,k,p},{p}] -> Qv[k] (k.k)^-1 k.n/q.n W0[eir],

        (* tego nie ma u MS *)
        $$[{x_},{0,k,p},{p}] :> Qv[k] (k.k)^-1 / q.n (
            p.x W1[eir] + k.x W2[eir] + n.x k.k/(2 k.n) W3[euv]
        ),

        (* tego nie ma u MS *)
        $$[{x_, y_},{0,k,p},{p}] :> Qv[k] (k.k)^-1 / q.n (
              W4[eir] p.x p.y
            + W5[eir] (p.x k.y + k.x p.y)
            + W6[eir] k.x k.y
            + W7[euv] (p.x n.y + n.x p.y) k.k/(2 p.n)
            + W8[euv] (k.x n.y + n.x k.y) k.k/(2 k.n)
            + W9[euv] n.x n.y (k.k/(2 k.n))^2
            + W10[euv] x.y k.k
        )
      };

      result = Expand[expr /. integrateRules];

      unevaluated = Union[Cases[result, $$[__], {0, Infinity}]];
      If[
        unevaluated != {}
        ,
        Message[ IntegrateLoopGeneral::unevaluated, ToString[#] &/@ unevaluated ];
        Raise[$UnevaluatedError];
      ];

      result
    ];


    $$ExpandPaVe[expr_] := Block[
      {B0,B1,B3,C0,C1,C3,D0,E0,E1,E2,E3,H1,H2,H3,H4,H5,H6,K0,P0,P1,P3,R1,R2,R3,R4,R5,R6,
       S1,S2,S3,T0,T1,T2,T3,U1,U2,U3,V1,V2,W1,W2,W3},

      H1[e_] := (-(1 + eps) R0[e] - 8 (1 + eps) T2[e] + 12 T3[e])/(1 + eps);
      H2[e_] := -((2 (T2[e] + 2 (3 + eps) T3[e]))/(1 + eps));
      H3[e_] := (2 (T2[e] + (3 + eps) T3[e]))/(1 + eps);
      H4[e_] := T2[e];
      H5[e_] := T3[e]/(1 + eps);
      H6[e_] := - T2[e]/(4 + 2 eps);

      P1[e_] := T0[e];
      P3[e_] := P0[e] - 2 T0[e];

      R1[e_] := - R0[e] - 2 T0[e];
      R2[e_] :=   T0[e];
      R3[e_] :=   (R0[e] + eps R0[e] + 6 T1[e] + 4 eps T1[e])/(1 + eps);
      R4[e_] := - T1[e]; 
      R5[e_] := - (T1[e]/(1 + eps));
      R6[e_] :=   T1[e]/(2 + 2 eps);

      T1[e_] :=   T0[e] / 2;
      T3[e_] := - T2[e]/(4 + 2 eps);

      Expand[expr]
    ];

    $$ExpandCommon[expr_] := Block[
      {B0,B1,B3,C0,C1,C3,D0,E0,E1,E2,E3,H1,H2,H3,H4,H5,H6,K0,P0,P1,P3,R1,R2,R3,R4,R5,R6,
       S1,S2,S3,T0,T1,T2,T3,U1,U2,U3,V1,V2,W1,W2,W3},

      B0[eps_] := - I0/eps - I1 + Li2[1];
      B1[eps_] := - 1/eps + 2;
      B3[eps_] := - (I0 - 2)/eps - I1 - 4 + Li2[1];

      C0[eps_] := - (I0 + Log[1-x])/eps - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 + Li2[1];
      C1[eps_] := - 1/eps + 2;
      C3[eps_] := - (I0 + Log[1-x] - 2)/eps - I1 + I0 Log[1-x] + (Log[1-x]^2)/2 - 4 + Li2[1];

      D0[eps_] := - (Log[1-x] - Log[x])/eps + (Log[x]^2)/2 - (Log[1-x]^2)/2 + Li2[1] - 2 Li2[1-x] - Log[x]Log[1-x];

      E0[eps_] :=   Log[1-x]/(x eps);

      E1[eps_] :=   1/eps Log[1-x]/x - (2 Li2[x] + (Log[1-x]^2)/2)/x;
      E2[eps_] := - 1/eps (x + Log[1-x])/x^2 + (2 Li2[x] + (Log[1-x]^2)/2 - 2x)/x^2;
      E3[eps_] := - 1/eps ((x-2)Log[1-x] - 2x)/x^3 + (4x + (x-2)(2 Li2[x] + (Log[1-x]^2)/2))/x^3;

      K0[eps_] :=   Log[x]/euv + 2 Li2[1-x] + Log[x]^2/2;

      P0[eps_] := - (I0 + Log[x])/eps - I1 + I0 Log[x] + (Log[x]^2)/2 + Li2[1];

      S1[eps_] := (R0[eps] (-2 + x) + (P0[eps] + S0[eps]) x)/(2 (-1 + x));
      S2[eps_] := (P0[eps] - R0[eps] + S0[eps])/(2 - 2 x);
      S3[eps_] := (-P0[eps] (-2 + x) + (-R0[eps] + S0[eps]) x)/(2 (-1 + x));

      T0[eps_] := - 1/eps + 2;
      T2[eps_] := - 1/(3 eps) + 13/18;

      U1[eps_] := 1/2 (x P0[eps] - (-2 + x) R0[eps] + x U0[eps]);
      U2[eps_] := 1/2 (- P0[eps] - R0[eps] + U0[eps]);
      U3[eps_] := 1/2 (-(-2 + x) P0[eps] + x (R0[eps] - U0[eps]));

      V1[eps_] := - (1-x + x Log[x])/(1-x)^2 / eps;
      V2[eps_] :=   (1-x + Log[x])/(1-x)^2 / eps;

      W1[eps_] := 1/2 (-(-2 + x) R0[eps] + x (E0[eps] x - 2 W0[eps]));
      W2[eps_] := 1/2 (-E0[eps] x - R0[eps]);
      W3[eps_] := 1/2 x (-E0[eps] (-2 + x) + R0[eps]);

      Expand[ expr ]
    ];


    $$ExpandMPV[expr_] := Block[
      {R0,S0,S4,S5,S6,S7,S8,S9,S10,U0,U4,U5,U6,U7,U8,U9,U10,W0,W4,W5,W6,W7,W8,W9,W10,$expr},

      $expr = $$ExpandCommon[ $$ExpandPaVe[expr] ];

      R0[eps_] := - (- (2 I0 + Log[1-x])/eps - 4 I1 + 2 I0 Log[1-x] + (Log[1-x]^2)/2);

      S0[eps_] := - ( - (3 I0 + Log[1-x] - Log[x]) / eps - 5 I1 + 2 I0 Log[1-x] + I0 Log[x] + (Log[x]^2)/2 + (Log[1-x]^2)/2 + 2 Li2[1-x] + Li2[1]);


      S4[eps_] := ((2 - 2 I0 (1-x) - x) (1-x) - (1-x)^2 Log[1-x] + x^2 Log[x])/(eps (1-x)) + (1/(2 (1-x)))(2 x^2 Li2[1-x] + 4 I0 (1-x)^2 Log[1-x] + (1-x)^2 Log[1-x]^2 - 2 (2 (1-x) (2 + 2 I1 (1-x) - x) + x^2 Log[x]));
      S5[eps_] := (-1 + x - x Log[x])/(eps (1-x)) + (2 - 2 x - x Li2[1 - x] + x Log[x])/(1-x);
      S6[eps_] := (-2 + 2 x + Li2[1 - x] - Log[x])/(1-x) + (1 - x + Log[x])/(eps (1-x)); 
      S7[eps_] := (1 - x + Log[x])/(eps (1-x)) + (2 (-1 + x) + Li2[1 - x] + (-2 + x) Log[x])/(1-x); 
      S8[eps_] := (-1 + x - x Log[x])/(eps (1-x)) + (2 - 2 x - x Li2[1 - x] + x Log[x])/(1-x); 
      S9[eps_] := ((2 - I0(1-x) - x)(1-x) - (1-2 x)Log[x])/(eps(1-x)) + (2 (1-x)(-4 - I1(1-x) + 2 x + (1-x)Li2[1]) + 2 x^2 Li2[1-x] + 2 (I0(1-x)^2 - x^2)Log[x] + (1-x)^2 Log[x]^2)/(2 (1-x));
      S10[eps_] := -(Li2[1 - x] - 2 Log[x])/2 - Log[x]/(2 eps);

      U0[eps_] := - (- (3 I0 + 3 Log[1-x] - Log[x])/eps - 5 I1 + 2 I0 Log[1-x] + I0 Log[x] + (Log[x]^2)/2 - (Log[1-x]^2)/2 - 2 Li2[1-x] + 5 Li2[1]);

      U4[eps_] := 1/2 (-8 - 8 I1 + 4 x + 4 x^2 Li2[1] - 2 x^2 Li2[1-x] + 2 (2 I0 + x^2) Log[1-x] - (-1 + x^2) Log[1-x]^2 - 2 x^2 Log[x]) + (2 - 2 I0 - x - (1 + x^2) Log[1-x] + x^2 Log[x])/eps;
      U5[eps_] := 2 - 1/eps - x Log[1 - x] + x Log[x];
      U6[eps_] := -2 - I1 + 3 Li2[1] - Li2[1 - x] + (1 - I0 - Log[1 - x])/eps + Log[1 - x] - 1/2 Log[1 - x]^2 - Log[x] + I0 Log[x] + Log[x]^2/2;
      U7[eps_] := (-1 + x) Li2[1 - x] + 1/2 (-4 + 4 Li2[1] - 4 x Li2[1] - 2 (-2 + x) Log[1 - x] + (-1 + x) Log[1 - x]^2 + 2 (-2 + x) Log[x]) + (1 + (-1 + x) Log[1 - x] + Log[x] - x Log[x])/eps; 
      U8[eps_] := 2 + I1 - Li2[1] - 2 x Li2[1] + x Li2[1 - x] - x Log[1 - x] + 1/2 x Log[1 - x]^2 - I0 Log[x] + x Log[x] - Log[x]^2/2 + (-1 + I0 + x Log[1 - x] + Log[x] - x Log[x])/eps; 
      U9[eps_] := -4 - I1 + 2 x + Li2[1] + 2 x^2 Li2[1] - x^2 Li2[1-x] + x^2 Log[1-x] - x^2 Log[1-x]^2/2 + I0 Log[x] - x^2 Log[x] + Log[x]^2/2 + (2 - I0 - x - x^2 Log[1-x] - (1-x^2) Log[x])/eps; 
      U10[eps_] := 1/4 (-1 + x) (4 Li2[1] - 2 Li2[1 - x] + 4 Log[1 - x] - Log[1 - x]^2 - 4 Log[x]) - ((-1 + x) (Log[1 - x] - Log[x]))/(2 eps);


      W0[eps_] := - (- (3 I0 + 3 Log[1-x] - 2 Log[x])/eps - 5 I1 + 2 I0 Log[1-x] - 3 I0 Log[x] - (Log[1-x]^2)/2 + Log[x] Log[1-x] + 2 Li2[1-x] + Log[x]^2 + 5 Li2[1]);

      W4[eps_] := (2 - 4 I0 - x + 5 I0 x + (-2 + 3 x) Log[1 - x] - 2 x Log[x])/eps - (1/(2 (-1 + x)))(-8 - 16 I1 + 12 x + 34 I1 x + 4 x^2 - 18 I1 x^2 - 4 x^3 - 10 x Li2[1] + 10 x^2 Li2[1] + 4 (-1 + x) x Li2[1 - x] + 4 (-1 + x) x Li2[x] + 8 I0 Log[1 - x] + 2 x Log[1 - x] - 16 I0 x Log[1 - x] - 2 x^2 Log[1 - x] + 8 I0 x^2 Log[1 - x] + 2 Log[1 - x]^2 - 3 x Log[1 - x]^2 + x^2 Log[1 - x]^2 + 6 I0 x Log[x] - 6 I0 x^2 Log[x] - 2 x Log[1 - x] Log[x] + 2 x^2 Log[1 - x] Log[x] - 2 x Log[x]^2 + 2 x^2 Log[x]^2);
      W5[eps_] := (-1 + I0 + Log[1 - x])/eps + (8 (-1 + I1 (-1 + x) + 3 x - x^2) + 8 (-1 + x) Li2[x] - 4 (1 + I0) (-1 + x) Log[1 - x] + (-1 + x) Log[1 - x]^2)/(4 (-1 + x));
      W6[eps_] := 1/eps - (4 x + 4 (-1 + x) Li2[x] - 2 (-1 + x) Log[1 - x] + (-1 + x) Log[1 - x]^2)/(2 (-1 + x) x);
      W7[eps_] := (1 - I0 - Log[1-x]/x)/eps + (-8 x (3 - I1 (1-x) - 3 x + x^2) + 8 (2 - 3 x + x^2) Li2[x] + 4 (-1 + x) (2 + (-1 + I0) x) Log[1-x] + (4 - 7 x + 3 x^2) Log[1-x]^2)/(4 (-1 + x) x);
      W8[eps_] := (-x + (-1 + x) Log[1 - x])/(eps x) + (4 x + 4 (-1 + x) Li2[x] - 2 (-1 + x) x Log[1 - x] + (-1 + x) Log[1 - x]^2)/(2 (-1 + x) x);
      W9[eps_] := ((2-x) x + 2 (1-x) Log[1-x])/(eps x) - (4 x (-2 + 3 x - 3 x^2 + x^3) + 4 (2 - 4 x + 3 x^2 - x^3) Li2[x] - 2 (1-x) x^2 Log[1-x] + (2 - 4 x + 3 x^2 - x^3) Log[1-x]^2)/(2 (1-x) x);
      W10[eps_] := -(((-1 + x) Log[1 - x])/(2 eps x)) + (4 (-2 + x) x - 4 (-1 + x) Li2[x] + 4 (-1 + x) Log[1 - x] - (-1 + x) Log[1 - x]^2)/(4 x);

      Expand[ $expr ] /. {n.p->1, k.n->x, q.n->1-x}
    ];


    $$ExpandPV[expr_] := Module[
      {},

      Expand[$$ExpandCommon[expr] //. {
        R0 ->  1/eir^2 - Li2[1],
        R1 ->  1/eir^2 + 2/eir + 4 - Li2[1],
        R2 -> -1/eir - 2,
        R3 ->  1/eir^2 + 3/eir + 7 - Li2[1],
        R4 -> -1/(2 eir) - 1,
        R5 -> -1/(2 eir) - 3/2,
        R6 -> - 1/(4 euv) + 3/4,

        U0 -> 1/eir^2 + (-I0 + Log[x] - 2 Log[1-x])/eir + I1 -I0 Log[x] + 2 Li2[1-x] - (Log[x]^2)/2 + Log[1-x]^2 - 6 Li2[1],

        S0 -> 1/eir^2 - (I0 - Log[x])/eir + I1 - I0 Log[x] - 2 Li2[1] - 2 Li2[1-x] - (Log[x]^2)/2,

        S1 -> 1/eir^2 - x Log[x]/((1-x) eir)  + x/(1-x) Li2[1-x] - Li2[1],
        S2 -> Log[x]/((1-x) eir) - Li2[1-x]/(1-x)
      }]
    ];


    Options[IntegrateLoop] = {Prescription -> "MPV", SimplifyNumeratorAndDenominator -> True};
    IntegrateLoop[expr_, l_, OptionsPattern[]] := Module[
      {collected, integrated, integratedPV, simplified},

      collected = $$CollectLoopIntegrals[expr, l];

      simplified = collected;
      simplified = $$SimplifyAlgebraic[simplified];
      If[
        OptionValue[SimplifyNumeratorAndDenominator] == True
        , 
        simplified = $$SimplifyNumeratorAndDenominator[simplified];
      ];
      simplified = $$SimplifyTranslate[simplified];
      simplified = $$SimplifyOnePoint[simplified];

      integrated = Expand[ IntegrateLoopGeneral[simplified, l] /. $kinematicRules ];

      integratedPV =  If[
        OptionValue[Prescription] == "MPV"
        ,
        $$ExpandMPV[integrated]
        ,
        If[OptionValue[Prescription] == "PV"
        ,
        $$ExpandPV[integrated]
      ]];

      {
          {"collected", collected},
          {"simplified", simplified},
          {"integrated", {
              {"short", integrated},
              {"long", integratedPV}}}
      }
    ];

  End[];

EndPackage[];
