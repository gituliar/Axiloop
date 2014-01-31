BeginPackage["Axiloop`", {
  "Axiloop`Core`",
  "Axiloop`FeynmanRules`",
  "Axiloop`GammaTrace`",
  "Axiloop`Integrate`"}];


  Axiloop`$Author = "Oleksandr Gituliar <oleksandr@gituliar.org>";
  Axiloop`$Version = "2.1 (Jan 2014)";

Clear[ "Axiloop`*" , "Axiloop`Private`*"];


(*---------------------------------------------------------------------------*)
(*---------------------- FEYNMAN RULES and GAMMA TRACE ----------------------*)
(*---------------------------------------------------------------------------*)

FP::usage =
"FP[momentum, Line -> f1] -- a fermion propagator in the light-cone gauge."

FPx::usage =
"FPx[momentum, Line -> f1] -- a crossed (final-state, on-shell) fermion
propagator in the light-cone gauge."

FV::usage =
"FV[index, Line -> f1] -- a fermion vertex in the light-cone gauge."

G::usage =
"G[<vector or index>, Line -> f1] -- a gamma matrix.

Usage:
    G[{mu}]     a gamma matrix with vector index `mu`;
    G[p]        a gamma matrix convoluted with a vector,
                the same as `G[{mu}] p.{mu}`;"

GP::usage =
"GP[mu, nu, p] -- a gluon propagator in the light-cone gauge."

GPx::usage =
"GPx[mu, nu, p] -- a crossed (final-state, on-shell) gluon propagator in
the light-cone gauge."


ExtractFormFactors::usage = ""

ExtractFormFactors::usage = ""

ExtractFormFactors::usage = ""

IntegrateFinal::usage =
"Integrate over final-state momenta."

SplittingFunction::usage = ""


PartonDensity::usage =
	"Kernel constructor; define and integrate a kernel."

SplittingFunction::usage = ""


Begin["`Private`"]


(*------------------- MISCELLANEOUS ROUTINES and HELPERS --------------------*)


(* Useful modifications to standard functions *)

Unprotect[Dot];
    (-x_).y_ := -x.y;
Protect[Dot];

(*
Unprotect[S];
    S[n,n] = 0;
Protect[S];
*)

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

IntegrateFinal[kernel_, ndim_] := Module[
	{eps},

	eps = Simplify[ndim/2 - 2];
	(4 Pi)^(-2+eps)/Gamma[1-eps] (1 + eps Log[1-x])	Integrate[Expand[(k.k)^(eps) kernel], k.k]
];


(*---------------------------------------------------------------------------*)
(*------------------------ LOOP MOMENTA INTEGRATION -------------------------*)
(*---------------------------------------------------------------------------*)

$onShellRules = {
	p.p -> 0,
	q.q -> 0
}


(*---------------------------------------------------------------------------*)
(*--------------------------- SPLITTING FUNCTION ----------------------------*)
(*---------------------------------------------------------------------------*)

Options[SplittingFunction] = {IntegrateLoopPrescription -> "MPV"};
SplittingFunction[$topology_, $LO_:Null, OptionsPattern[]] := Module[
	{counterterm, exclusive, exclusiveBare, exclusiveBareShort, inclusive,
	 integrated, trace, Z, $PutOnShell},
	
	$PutOnShell[expr_] := Replace[
		Expand[expr] /. {
			S[x_,x_]^(n_Integer-eir) :> 0 /; n > 0 && (x == p || x == q)
			,
			S[x_,x_]^n_Integer :> 0 /; n > 1 && (x == p || x == q)
		}
		,
		{p.p -> 0, q.q -> 0}
		,
		2
	];
	
	trace = Expand[
		GammaTrace[Expand[$topology], NumberOfDimensions -> 4 + 2 eps]
			/. $kinematicRules
	];

	integrated = IntegrateLoop[
		trace,
		l,
		Prescription -> OptionValue[IntegrateLoopPrescription]
	];
	
	exclusiveBareShort = If[
		SameQ[$LO, Null]
		,
		Null
		,
		$PutOnShell[$Get[integrated, {"integrated", "short"}]]
	] /. {k.n -> x, n.p -> 1, n.q -> 1-x};
	
	exclusiveBare = $PutOnShell[$Get[integrated, {"integrated", "long"}]]
		/. {eps^2 -> 0}
		/. {k.n -> x, n.p -> 1, n.q -> 1-x};
	
	Z = Simplify[If[
		SameQ[$LO, Null]
		,
		0
		,
		PolePart[
			exclusiveBare
				(* /. {(k.k)^(n_Integer-eir) :> (k.k)^n} *)
				/. {S[_,_]^(-eir) :> 1}
			,
			euv
		] / $Get[$LO, "exclusive"]
	] /. {eir -> 0, eps -> 0}] /. $onShellRules;

	counterterm = If[
		SameQ[$LO, Null]
		,
		0
		,
		2^(2 eir) Pi^(eir) Gamma[1+eir] Z $Get[$LO, "exclusive"]
	];
	
	exclusive = exclusiveBare - counterterm / euv
		/. {eir -> - eps, euv -> -eps}
		/. {p.p -> 0, q.q -> 0}
		/. {0^eps -> 0, 0^(1+eps) -> 0}
	;

	inclusive = Expand[
		PolePart[
			IntegrateFinal[Expand[exclusive], 4 + 2 eps]
			,
			eps
		]
	];
	
	{
		{"trace", trace},
		{"integrated", integrated},
		{"Z", Z},
		{"counterterm", counterterm},
		{"exclusive-bare", exclusiveBare},
		{"exclusive-bare-short", exclusiveBareShort},
		{"exclusive", exclusive},
		{"inclusive", inclusive}
	}
];


ExtractFormFactors[bare_] := Module[
	{$$k$uv, $$p$uv, $$q$uv, $$k$ir, $$k$ir2, $$k$0, $$bare},
	
	$$bare = Expand[
		bare / (I g^4 (4 Pi)^(-2+eir) Gamma[1+eir] / k.k)
	];
	
	$$k$ir2 = PolePart[
		$$bare
			/. {eps -> -eir}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		eir
		,
		-2
	] /. {p.p -> 0, q.q -> 0};
	DEBUG[
		"ExtractFormFactors::$$k$ir2"
		,
		Simplify[$$k$ir2]
	];
	
	$$k$ir = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 1, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 0, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 0, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eps -> -eir}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		eir
	];
	DEBUG[
		"ExtractFormFactors::$$k$ir"
		,
		$$k$ir
	];
	
	$$k$uv = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 1, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 0, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 0, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eps -> -euv}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		euv
	];
	DEBUG[
		"SplittingFunction::$$k$uv"
		,
		$$k$uv
	];

	$$p$uv = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 0, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 1, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 0, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eps -> -euv}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		euv
	];
	DEBUG[
		"SplittingFunction::$$p$uv"
		,
		$$p$uv
	];


	$$q$uv = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 0, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 0, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 1, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eps -> -euv}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		euv
	];
	DEBUG[
		"SplittingFunction::$$q$uv"
		,
		$$q$uv
	];
	
	$$k$0 = PolePart[
		$$bare
			/. {(k.k)^(-eir) -> 1, (k.k)^(n_Integer-eir) :> (k.k)^n}
			/. {(p.p)^(-eir) -> 0, (p.p)^(n_Integer-eir) :> (p.p)^n}
			/. {(q.q)^(-eir) -> 0, (q.q)^(n_Integer-eir) :> (q.q)^n}
			/. {q.q -> 0, p.p -> 0}
			/. {eir -> -eps, euv -> -eps}
			/. {k.n -> x, p.n -> 1, q.n -> 1-x}
		,
		eps
		,
		0
	];
	DEBUG[
		"SplittingFunction::$$k$0"
		,
		Collect[
			$$k$0
			,
			{I0, Log[x], Log[1-x], I1, Li2[1], Li2[1-x]}
			,
			Simplify
		]
	];
	
	
	DEBUG[
		"SplittingFunction:: $$k$uv + $$p$uv + $$q$uv"
		,
		Collect[
			Expand[($$k$uv + $$p$uv + $$q$uv)]
			,
			{I0, Log[x], Log[1-x]}
			,
			Simplify
		]
	];
	
	DEBUG[
		"SplittingFunction:: $$p$uv + $$q$uv - $$k$ir"
		,
		Collect[
			Expand[$$p$uv + $$q$uv - $$k$ir]
			,
			{I0, Log[x], Log[1-x]}
			,
			Simplify
		]
	];

    {
        {"$$k$0", $$k$0},
        {"$$k$ir", $$k$ir},
        {"$$k$ir2", $$k$ir2},
        {"$$k$uv", $$k$uv},
        {"$$p$uv", $$p$uv},
        {"$$q$uv", $$q$uv}
    }
];

End[]


EndPackage[]
