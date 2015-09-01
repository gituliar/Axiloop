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
AX$DeclareVector[k,p,q];

UT$BeginTestCase["AX$IntegrateLoop"];
  UT$AssertEquivalent[
    AX$LoopCollect[AX$S[l,k]/(AX$S[l+k] AX$S[l+p]), l]
    ,
    AX$Loop[l,Null,Null, {k},{k,p},{},{}]
  ];

  UT$AssertEquivalent[
    AX$LoopCollect[AX$S[l,mu] AX$S[l,k]/(AX$S[l+k] AX$S[l+p]), l]
    ,
    AX$Loop[l,Null,Null, {k,mu},{k,p},{0},{}]
  ];
UT$EndTestCase[];
