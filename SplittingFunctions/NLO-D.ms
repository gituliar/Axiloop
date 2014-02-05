#!/usr/bin/math -script

<< Axiloop`


$LO = AX$Get["LO.result"];

$topology = x (G[n]/(4 k.n)) ** FP[k] ** FV[i1] ** FP[l] ** FV[i2] ** 
  GP[i1, i3, l + k] ** GP[i2, i4, l + p] ** 
  GV[i3, -l - k, i4, l + p, mu, -p + k] ** FPx[p] ** 
  GPx[mu, nu, p - k] ** FV[nu] ** FP[k];

$result = SplittingFunction[$topology, $LO];

(*
<< Logging`
LG$Output[ SplittingFunctionFormFactors[ $Get[$result, "exclusive-bare"] ] ];

$ebsf = $Get[$result, "exclusive-bare-short"];
$n = I g^4 / ((1-x) k.k);
$ebsf = $n CollectFormFactors[$ebsf / $n];
$ebsf >> "NLO-D.mx";
*)
