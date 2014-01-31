#!/usr/bin/math -script

(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2012-2014 Oleksandr Gituliar.                               *)
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


<< UnitTest`

<< Axiloop`Integrate`


UT$BeginTestCase["FeynmanRules"];

(*
  UT$AssertEquivalent[
    Try[IntegrateLoop[1/(l.l (l+k).(l+k) (l+p).(l+p) (l+q).(l+q)), l]]
    ,
	$UnevaluatedError
	,
	Axiloop`Integrate`Private`IntegrateLoopGeneral::unevaluated
  ];
*)


  UT$AssertEquivalent[
    $Get[IntegrateLoop[l.k/((l+k).(l+k) (l+p).(l+p)), l], {"integrated", "short"}]
    ,
    I 2^(-5+2 eir) Pi^(-2+eir) T1 Gamma[1+eir] (-3 k.k - p.p + q.q)/(q.q)^eir
  ];

UT$EndTestCase[];
