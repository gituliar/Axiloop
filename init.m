(*
;; Copyright (c) 2012-2014 Oleksandr Gituliar <oleksandr@gituliar.org>.
;;
;; This file is part of Axiloop package.
*)

(*
;; This directory contains sources for next-to-leading (\Alpha_s^2) order
;; one-loop splitting functions (which are also known as evolution kernels
;; in the context of DGLAP evolution equations).
;;
;; Source files are designed to serve as:
;;  * scripts;
;;  * tests;
;;  * sessions.
;;
;; for actually performing calculations and printing output
;; for developing new features and checking consistency of the code chacnges
*)

BeginPackage["Axiloop`", {
  "Axiloop`Core`",
  "Axiloop`FeynmanRules`",
  "Axiloop`GammaTrace`",
  "Axiloop`IntegrateLeg`",
  "Axiloop`IntegrateLoop`",
  "Axiloop`SplittingFunction`"
  }];

  AX$Author = "Oleksandr Gituliar <oleksandr@gituliar.org>";
  AX$Version = "Axiloop 2.3 (Mar 2014)";



  AX$Get::usage = ""

  AX$PolynomialReduce::usage = ""

ExtractFormFactors::usage = ""

SplittingFunctionFormFactors::usage = ""

$Cases::usage = "";

Begin["`Private`"]


(*------------------- MISCELLANEOUS ROUTINES and HELPERS --------------------*)

  AX$Get[filename_] := Get[filename, Path -> DirectoryName[$InputFileName]];


AX$PolynomialReduce[expr_] := Block[
  {$A, $A0, $B, $B0, $C, $C0, $common, $solution},

  $common = I g^4;

  {$A0, $B0, $C0} = CoefficientList[Simplify[expr (1-x) / $common], {x}];

  $solution = Solve[$A + $B + $C == $A0 && -2 $B == $B0 && - $A + $B + $C == $C0, {$A,$B,$C}];

  $common ((($A (1+x) + $B (1-x) + $C (1+x^2)/(1-x)) /. $solution)[[1]])
];

AX$CoefficientList$Basis = {Li2[1], Log[x], Log[1-x], I0, Li2[1-x], I1};

AX$CoefficientList[expr_, basis_:AX$CoefficientList$Basis] := Block[
  {$ff, $expr, $i, $result, $rule, $rules, $temp},

  $expr = Expand[expr];
  $result = {};

  For[ $i=1, $i <= Length[basis], $i++,
    $ff = Plus @@ (Cases[expr, Times[__, basis[[$i]]]]) / basis[[$i]];
    If[
      $ff =!= 0
      ,
      $rule = basis[[$i]] -> AX$PolynomialReduce[ $ff ];
      AppendTo[ $result, $rule];
      $expr = Expand[$expr - basis[[$i]] (basis[[$i]] /. $rule)];
    ];
  ];
  AppendTo[ $result, "tail" -> AX$PolynomialReduce[$expr /. (# -> 0 &/@ basis)] ];

  $result
];


Unprotect[Dot];
    (-x_).y_ := -x.y;
Protect[Dot];


CollectExclusiveShort[expr_] := Module[{},
	Collect[
		expr /. {eps -> 0, (k.k)^(-1-eir) -> (k.k)^-1, (p.p)^(-eir) -> 1, (q.q)^(-eir) -> 1}
		(*
			/. {eps->0, (k.k)^(n_Integer-eir):>(k.k)^n, p.p->0, q.q->0}
			/. {0^(-eir)->1, 0^(1-eir)->0, 0^(2-eir)->0}
		*)
		,
		{B0,B1,B3,C0,C1,D0,K0,P0,P1,P3,R0,R1,R2,R3,R4,R5,R6,S0,S1,S2,T0,T1,V0,V1,V2,U0}
		,
		Simplify
	]
];


(*---------------------------------------------------------------------------*)
(*--------------------- FINAL-STATE MOMENTA INTEGRATION ---------------------*)
(*---------------------------------------------------------------------------*)


$Cases[expr_, pattern_] := Plus @@ Cases[expr, e_ pattern -> e, 1];

SplittingFunctionFormFactors[ebsf_] := Block[
  {$CancelFormFactors, $ebsf, $result = {}},

  $ebsf = Expand[k.k ebsf];

  AppendTo[ $result, "W_0^k"  -> AX$PolynomialReduce[ PolePart[$Cases[$ebsf, Qv[k]] /. {eir -> eps, euv -> eps}, eps, 0]]];
  AppendTo[ $result, "W_ir^k" -> AX$PolynomialReduce[ PolePart[$Cases[$ebsf, Qv[k]], eir, -1] /. {eps -> 0}]];
  AppendTo[ $result, "W_uv^k" -> AX$PolynomialReduce[ PolePart[$Cases[$ebsf, Qv[k]], euv, -1] /. {eps -> 0, eir -> 0}]];
  AppendTo[ $result, "W_uv^p" -> AX$PolynomialReduce[ PolePart[$Cases[$ebsf, Qv[p]], euv, -1] /. {eps -> 0, eir -> 0}]];
  AppendTo[ $result, "W_uv^q" -> AX$PolynomialReduce[ PolePart[$Cases[$ebsf, Qv[q]], euv, -1] /. {eps -> 0, eir -> 0}]];

  $result
];


End[];

EndPackage[];
