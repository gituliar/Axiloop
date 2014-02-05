#!/usr/bin/math -script

<< Axiloop`


$LO = AX$Get["LO.result"];

$topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[l+k] ** FV[mu] **
  FP[l+p] ** FV[i2] ** GP[i1, i2, l] ** FPx[p] ** GPx[mu, nu, p-k] **
  FV[nu] ** FP[k];

$result = SplittingFunction[$topology, $LO];

(*
$ebsf = Expand[$Get[$result, {"integrated", "integrated", "short"}]] /. {p.p->0, q.q->0, k.n->x, p.n->1, q.n->1-x};

LG$Output[ CollectFormFactors[ $ebsf ] ];
LG$Output[ ExtractFormFactors[ $ebsf ] ];

$ebsf = $Get[$result, "exclusive-bare-short"];
$n = I g^4 / ((1-x) k.k);
$ebsf = $n CollectFormFactors[$ebsf / $n];
LG$Output[ $ebsf ];
*)
