#!/usr/bin/math -script

(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2012-2015 Oleksandr Gituliar.                               *)
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


Needs["UnitTest`"];

Needs["Axiloop`"];

AX$DeclareIndex[mu,nu];
AX$DeclareVector[k,n,p,q];

$loops = {
  AX$LoopFromExpression[AX$S[l,k]/(AX$S[l+k] AX$S[l+p]), l]
  ,
  AX$LoopFromExpression[AX$S[l,mu]/(AX$S[l+k] AX$S[l+p]), l]
  ,
  AX$LoopFromExpression[AX$S[l,mu] AX$S[l,k]/(AX$S[l+k] AX$S[l+p] AX$S[l,n]), l, n]
  ,
  AX$LoopFromExpression[AX$S[l,mu] AX$S[l,nu]/(AX$S[l+k] AX$S[l+p]), l]
  ,
  AX$LoopFromExpression[AX$S[l,mu] AX$S[l,nu]/(AX$S[l+k]), l]
};

UT$BeginTestCase["AX$LoopFromExpression"];
  UT$AssertEquivalent[
    $loops[[1]]
    ,
    AX$Loop[l,Null,Null, {k},{k,p},{},{}]
  ];

  UT$AssertEquivalent[
    $loops[[2]]
    ,
    AX$Loop[l,Null,Null, {mu},{k,p},{},{}]
  ];

  UT$AssertEquivalent[
    $loops[[3]]
    ,
    AX$Loop[l,n,Null, {k,mu},{k,p},{0},{}]
  ];
UT$EndTestCase[];


UT$BeginTestCase["AX$LoopLorentzBasis"];
  UT$AssertSame[
    AX$LoopLorentzBasis[$loops[[1]]]
    ,
    {}
  ];

  UT$AssertSame[
    AX$LoopLorentzBasis[$loops[[2]]]
    ,
    {AX$S[k,mu], AX$S[p,mu]}
  ];

  UT$AssertSame[
    AX$LoopLorentzBasis[$loops[[3]]]
    ,
    {AX$S[k,mu], AX$S[p,mu], AX$S[n,mu]}
  ];

  UT$AssertSame[
    AX$LoopLorentzBasis[$loops[[4]]]
    ,
    {AX$S[k,mu] AX$S[k,nu], AX$S[k,mu] AX$S[p,nu], AX$S[p,mu] AX$S[k,nu], AX$S[p,mu] AX$S[p,nu]}
  ];

  UT$AssertSame[
    AX$LoopLorentzBasis[$loops[[5]]]
    ,
    {AX$S[k,mu] AX$S[k,nu], AX$S[mu,nu]}
  ];
UT$EndTestCase[];
