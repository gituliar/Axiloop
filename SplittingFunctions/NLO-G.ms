#!/usr/bin/math -script

<< Axiloop`


$LO = AX$Get["LO.result"];

$topology = - x GammaTrace[PFo[k]**FP[k]**FV[i1]**PFi[p]**FV[nu]**FP[k]] GammaTrace[FV[i2]**FP[l]**FV[mu]**FP[l+q]] GP[i1,i2,q] GPx[mu,nu,q] ;

$result = SplittingFunction[$topology, $LO];
