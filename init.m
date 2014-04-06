BeginPackage["Axiloop`", {
  "Logging`",
  "Timestamp`",
  "Axiloop`Core`",
  "Axiloop`FeynmanRules`",
  "Axiloop`GammaTrace`",
  "Axiloop`Integrate`"
  }];

  AX$Author = "Oleksandr Gituliar <oleksandr@gituliar.org>";
  AX$Version = "Axiloop 2.3 (Mar 2014)";



  AX$Get::usage = ""

  AX$PolynomialReduce::usage = ""

ExtractFormFactors::usage = ""

IntegrateFinal::usage = "Integrate over final-state momenta."


PFi::usage = "";
PFo::usage = "";

PGi::usage = "";
PGo::usage = "";


SplittingFunction::usage = ""
SplittingFunctionFormFactors::usage = ""

$Cases::usage = "";

ExpandPhaseSpace::usage = "";
Qr::usage = "";

Q2::usage = "Hard process scale";

Begin["`Private`"]


(*------------------- MISCELLANEOUS ROUTINES and HELPERS --------------------*)

AX$Get[filename_] := Get[filename, Path -> DirectoryName[$InputFileName]];



  Options[PFi] = {Line -> f1};
  PFi[p_, OptionsPattern[]] := G[p, Line -> OptionValue[Line]];
  Options[PFo] = {Line -> f1};
  PFo[p_, OptionsPattern[]] := G[n, Line -> OptionValue[Line]]/(4 p.n);

  PGi[mu_,nu_,p_] := 1/(2 (1+eps)) (-{mu}.{nu} + (p.{mu} n.{nu} + n.{mu} p.{nu}) / p.n);
  PGo[mu_,nu_] := - {mu}.{nu};



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


(* Useful modifications to standard functions *)

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

IntegrateFinal[Wr_, ndim_] := Block[
	{eps},

	eps = Simplify[ndim/2 - 2];
	- Q2^eps / eps Qr (1 + eps Log[1-x]) (Expand[Wr k.k] /. {Qv[k] -> Qv Q2^eps / 2})
];

ExpandPhaseSpace[expr_] := Block[{},
  expr //. {
    Qr ->   (4 Pi)^(-2+eps)/Gamma[1-eps],
    Qv -> I (4 Pi)^(-2-eps) Gamma[1-eps],
    Qv[r_] :> Qv (-r.r)^eps
  }
];

(*---------------------------------------------------------------------------*)
(*--------------------------- SPLITTING FUNCTION ----------------------------*)
(*---------------------------------------------------------------------------*)

Options[SplittingFunction] = {IntegrateLoopPrescription -> "MPV"};
SplittingFunction[$topology_, $LO_:Null, OptionsPattern[]] := Block[
  {$G, $G1, $Wb, $Wbs, $Wn, $Wr, $Wz, $Z, $result, integrated},

  TS$Start["SplittingFunction"];

  $Wn = GammaTrace[$topology, NumberOfDimensions -> 4 + 2 eps];
  $Wn = $Wn /. {n.n -> 0};
  $Wn = Expand[$Wn /. $kinematicRules /. {g^_Integer (p.p | q.q) :> 0}];
  TS$Print["$Wn"];

  integrated = IntegrateLoop[ $Wn, l,
    Prescription -> OptionValue[IntegrateLoopPrescription]
  ];
  $Wbs = If[$LO =!= Null, $$ExpandPaVe[$Get[integrated, {"integrated", "short"}]], Null];
  $Wbs = $Wbs /. {k.n -> x, n.p -> 1, n.q -> 1-x};
  $Wbs = $Wbs /. {p.p -> 0, q.q -> 0};
  TS$Print["$Wbs"];

  $Wb = $Get[integrated, {"integrated", "long"}];
  $Wb = $Wb /. {k.n -> x, n.p -> 1, n.q -> 1-x};
  $Wb = $Wb /. {2^(2 eps) -> 4^eps};
  $Wb = $Wb /. {p.p -> 0, q.q -> 0};
  TS$Print["$Wb"];

  $Z = If[ $LO =!= Null, Simplify[ PolePart[$Wb, euv] / $Get[$LO, "exclusive"] /. {Qv[_] :> Qv, eps -> 0} ], 0];
  TS$Print["$Z"];

  $Wz = If[ $LO =!= Null, $Z $Get[$LO, "exclusive"], 0];
  TS$Print["$Wz"];

  $Wr = ($Wb - $Wz / euv) /. {eir -> eps, euv -> eps};
  $Wr = $Wr /. {p.p -> 0, Qv[p] -> 0, q.q -> 0, Qv[q] -> 0};
  TS$Print["$Wr"];

  $G = IntegrateFinal[$Wr, 4 + 2 eps];
  TS$Print["$G"];

  $G1 = ExpandPhaseSpace[ PolePart[$G, eps] ];
  TS$Print["$G1"];
	
  $result = {
    {"Wn", $Wn},
    {"Wr", $Wr //. {Qv[r_] :> Qv (-r.r)^eps, Qv -> I (4 Pi)^(-2)}},
    {"G1", $G1}
  };

  If[ $LO =!= Null, AppendTo[$result,#]& /@ {
    {"integrated", integrated},
    {"Wb",  $Wb},
    {"Wbs", $Wbs},
    {"Wz",  $Wz /. {Qv -> I (4 Pi)^(-2)}}
  } ];

  $result
];

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
