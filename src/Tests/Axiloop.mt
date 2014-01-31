(* Unit Tests for Axiloop Package
 *
 * Author:   Oleksandr Gituliar <gituliar@gmail.com>
 * Created:  04 May 2012
 *
 * Copyright (c) 2012 Oleksandr Gituliar
 *)

Needs["Axiloop`"]

(* EquivalenceFunction for Test. It turns out that Equal doesn't do the job. *)
EqualSimplify[x_, y_] := SameQ[0, Simplify[x-y]];

Test[
	Axiloop`Tracer`TracerVersion,
	"1.1.1",
	TestID->"TracerVersion"
]

Test[
	Axiloop`Tracer`Private`d,
	4-2*epsilon,
	TestID->"VectorDimension"
]

Test[
	FP[k, f1],
	I * G[f1, k] / k.k,
	TestID->"FermionPropagator"
]

Test[
	FV[mu, f1],
	- I g G[f1, {mu}],
	TestID->"FermionVertex"
]

Test[
	GP[mu, mu, p],
	- I 2 (1 - epsilon) / p.p,
	TestID->"GluonPropagator",
	EquivalenceFunction->EqualSimplify
]

Test[
	GV[mu,p, mu,k, nu,p],
	g (-3 + 2 epsilon) (k.{nu} - p.{nu}),
	TestID->"GluonVertex",
	EquivalenceFunction->EqualSimplify
]

LOdefinition = DefineKernel[FP[k] ** FV[mu], {FPx[p], GPx[mu, nu, q]}, FV[nu] ** FP[k]];

Test[
	LOdefinition,
	2 g^2 (k.k (1 - epsilon) - x (p.p + k.k) (1 - epsilon) + 2 x (k.k - x p.p)/(1 - x)) / (k.k)^2,
	TestID->"LO-Kernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateFinal[1/k.k],
	1/(4 Pi)^2 (1-x)^(-epsilon) (k.k)^(-epsilon) / (-epsilon),
	TestID->"IntegrateKernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateFinal[LOdefinition] //. p.p -> 0,
	2 g^2/(4 Pi)^2 (1-x)^(-1-epsilon) (1 + x^2 - epsilon (1-x)^2) (k.k)^(-epsilon) / (-epsilon),
	TestID->"IntegrateLOKernel",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`CollectIntegral[l.k1 l.k2 / (l.l (l-p1).(l-p1) (l-p2).(l-p2)) 1/(l.n (l-q1).n (l-q2).n), l],
	Axiloop`Private`KK[l, {k2,k1}, {p2,p1,0}, {q2,q1,0}],
	TestID->"CollectIntegral-1",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`CollectIntegral[x, l],
	x,
	TestID->"CollectIntegral-2",
	EquivalenceFunction->EqualSimplify
]

Test[
	Axiloop`Private`ReduceIntegral[Axiloop`Private`KK[l, {l}, {0}, {k1}], l],
	Axiloop`Private`KK[l, {},{},{0}],
	TestID->"ReduceIntegral-1"
]

Test[
	Axiloop`Private`ReduceIntegral[Axiloop`Private`KK[l, {k1}, {0}, {k1}], l],
	Axiloop`Private`KK[l, {k1}, {-k1}, {0}] + Axiloop`Private`KK[l, {}, {-k1}, {0}] k1.k1,
	TestID->"ReduceIntegral-2"
]

Test[
	Axiloop`Private`ReduceIntegral[Axiloop`Private`KK[l, {}, {0}, {k1, k2}], l],
	(Axiloop`Private`KK[l, {}, {-k1}, {0}] - Axiloop`Private`KK[l, {}, {-k2}, {0}]) / (k1.n - k2.n),
	TestID->"ReduceIntegral-3"
]

Test[
	IntegrateLoop[1/(l.l l.n), l] + IntegrateLoop[1/(l-p).(l-p), l] - IntegrateLoop[1/(l-k).(l-k), l],
	0,
	TestID->"IntegrateLoop K1(0;0)+I1(p)-I1(k)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/(l.l (l-k).(l-k) l.n), l],
	Axiloop`Private`Q ((I0 + Log[x]) / eta - I1 + Li2[1] + I0 Log[x] + (Log[x]^2)/2) / (x (k.k)^eta),
	TestID->"IntegrateLoop K2(k,0;0) Expand",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/(l.l (l-k).(l-k) l.n), l, False],
	Axiloop`Private`Q Axiloop`Private`P0[k] / (x (k.k)^eta),
	TestID->"IntegrateLoop K2(k,0;0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[l.xx/(l.l (l-k).(l-k) l.n), l],
	Axiloop`Private`Q (2 x Beta[1-eta, 1-eta] k.xx + (2 - I0 - Log[x] + eta (4 + I1 - Li2[1] - I0 Log[x] - (Log[x]^2)/2)) k.k n.xx) / (2 eta x^2  (k.k)^eta),
	TestID->"IntegrateLoop K2x(k,0;0) Expand",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[l.xx/(l.l (l-k).(l-k) l.n), l, False],
	Axiloop`Private`Q (2 x Axiloop`Private`T0 k.xx + Axiloop`Private`P3[k] k.k n.xx) / (2 x^2  (k.k)^eta),
	TestID->"IntegrateLoop K2x(k,0;0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/(l.l (l-k).(l-k)), l, False],
	Axiloop`Private`Q Axiloop`Private`T0 / (k.k)^eta,
	TestID->"IntegrateLoop I2(y,0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[1/(l.l (l-k).(l-k)), l],
	Axiloop`Private`Q Beta[1-eta, 1-eta] / (eta (k.k)^eta),
	TestID->"IntegrateLoop I2(y,0) Expand",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[l.xx/(l.l (l-k).(l-k)), l, False],
	Axiloop`Private`Q Axiloop`Private`T0 k.xx / (2 (k.k)^eta),
	TestID->"IntegrateLoop I2x(y,0)",
	EquivalenceFunction->EqualSimplify
]

Test[
	IntegrateLoop[l.xx/(l.l (l-k).(l-k)), l],
	Axiloop`Private`Q Beta[1-eta, 1-eta] k.xx / (2 eta (k.k)^eta),
	TestID->"IntegrateLoop I2x(y,0) Expand",
	EquivalenceFunction->EqualSimplify
]

Test[
	ExtractPole[Beta[1+eta, eta], eta],
	1,
	TestID->"ExtractPole-1"
]

Test[
	ExtractPole[Beta[1+eta, 1+eta] / eta^2, eta],
	-2,
	TestID->"ExtractPole-2"
]


LO = Kernel[FP[k] ** FV[mu], {FPx[p], GPx[mu, nu, p - k]}, FV[nu] ** FP[k]];
 
Test[
	KernelGet[LO, "exclusive"],
	2 g^2 ((1 - x) (1 - epsilon) + 2 x / (1 - x)) (k.k)^(-1),
	TestID->"Kernel LO exclusive",
	EquivalenceFunction->EqualSimplify
]

Test[
	KernelGet[LO, "inclusive"],
	- g^2 (1 + x^2) / (8 Pi^2 (1 - x) ),
	TestID->"Kernel LO inclusive",
	EquivalenceFunction->EqualSimplify
]

Test[
	KernelGet[LO, "Z"],
	0,
	TestID->"Kernel LO Z"
]


NLOe = Kernel[
	FP[k] ** FV[i1] ** FP[k-l] ** GP[i1, i2, l] ** FV[i2] ** FP[k] ** FV[mu],
	{FPx[p], GPx[mu, nu, p-k]},
	FV[nu] ** FP[k],
	LO
]

Test[
	KernelGet[NLOe, "inclusive"],
	I g^4/(16 Pi^2) Axiloop`Private`Q ( (1+x^2)/(1-x) (7 - 2 Log[x]^2 - 4 Log[x] Log[1-x] + 3 Log[1-x] - 4 Li2[1] + 4 I1 - 4 I0 Log[x] - 4 I0 Log[1-x]) + (1-x) (3 - 4 Log[x] - 4 I0) ),
	TestID->"Kernel NLOe inclusive",
	EquivalenceFunction->EqualSimplify
]

Test[
	KernelGet[NLOe, "Z"],
	-I g^2 Axiloop`Private`Q (3 - 4 I0 - 4 Log[x]),
	TestID->"Kernel NLOe Z"
]