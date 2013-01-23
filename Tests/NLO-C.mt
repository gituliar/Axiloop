(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2012-2013 Oleksandr Gituliar.                               *)
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


Get["Tests/utils.mt"];


$topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[l-k] ** FV[mu] **
	FP[l-p] ** FV[i2] ** GP[i1, i2, l] ** FPx[p] ** GPx[mu, nu, p-k] **
	FV[nu] ** FP[k];

$result = SplittingFunction[$topology];


Test[
	$Get[$result, "counterterm"]
	,
	Gamma[1+eir] 2^(2 eir) g^4 Pi^(-2+eir) (1+x^2) / (8 (1-x) k.k) *
		(-3 + 4 I0 + 2 Log[x])
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLOc-20130121-R0Q3G6"
];

	
(*
NLOc = PartonDensity[
	x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[l - k] ** FV[mu] ** FP[l - p] **
	    FV[i2] ** GP[i1, i2, l] ** FPx[p] ** GPx[mu, nu, p - k] ** FV[nu] **
	    FP[k],
	LO
]

Tc = (k.k)^(-1-eir) (
    4/x ((R1 - R0 + (2-eps)R2 - (1-eps)(R4+R5)) (x + eps (1-x)) +
         2 R6 (1-eps)^2 (1+x^2)/(1-x)) +
    4/x ((P0-P1)(1+x^2)/(1-x) - P2 (1+x)/(1-x)) +
    2/x ((S1 + (2-x) S2 - 2 S0 + R0)(1+x^2)/(1-x) -
         2 (R1 + R2 - R0)(1+x)/(1-x))
);

kernel = GetValue[NLOc, "kernel"];
Test[
	Axiloop`Private`ExpandIntegral[
		Axiloop`Private`ReduceIntegral[
			Axiloop`Private`CollectIntegral[kernel, l],
			l
		]
	],
	kernel,
	TestID->"Test 01",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[NLOc, "exclusive"] //. {p.p->0, q.q->0, 0^(2-eta)->0, 0^(1-eta)->0, 0^(-eta)->1},
	2 g^4 (4 Pi)^(-2+eta) Gamma[1+eta] (k.k)^(-1-eta) / (1-x) (
		  Axiloop`Private`P0 3 ((1+x^2) - eps (1-x)^2)
		+ Axiloop`Private`P1 (-1 + eps (1-x))(2-x)
		+ Axiloop`Private`R0 (3+x^2 - eps (2-3x+x^2) - eps^2 (1-x))
		+ Axiloop`Private`R1 (-3 + (2-x) eps + (1-x) eps^2)
		+ Axiloop`Private`R2 x (-3 + (2-x) eps + (1-x) eps^2)
		+ Axiloop`Private`R3 (1-eps)
		+ Axiloop`Private`R4 (1-eps) x^2
		+ Axiloop`Private`R5 (1-eps) 2 x
		+ Axiloop`Private`S0 (-1-x^2 + eps (1-x)^2)
		+ Axiloop`Private`T0 (2(1-x-2x^2) - eps(2-x) - eps^2 (1-x)x)
		+ Axiloop`Private`T1 (1-eps) (3x-2)x
	),
	TestID->"Kernel NLOc exclusive compact",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[NLOc, "inclusive"],
	- (g/(4 Pi))^4 ( (1+x^2)/(1-x) (-7 + 2 Log[x]^2 + 2 Log[x] Log[1-x] - 3 Log[1-x] + 2 Li2[1-x] + 4 Li2[1] - 4 I1 + 4 I0 Log[x] + 4 I0 Log[1-x]) - (1-x) (3 - 2 Log[x] - 4 I0) + x ),
	TestID->"Kernel NLOc inclusive",
	EquivalenceFunction->EqualSimplify
]

Test[
	GetValue[NLOc, "Z"],
	- (g/(4 Pi))^2 (3 - 4 I0 - 2 Log[x]),
	TestID->"Kernel NLOc Z",
	EquivalenceFunction->EqualSimplify
]
*)