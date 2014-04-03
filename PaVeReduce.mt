#!/usr/bin/math -script

(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2013-2014 Oleksandr Gituliar.                               *)
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
Needs["Axiloop`PaVeReduce`"];


UT$BeginTestCase["PaVeReduce"];

  $result = PaVeReduce[
    l.{mu}/(l.l (l+k).(l+k) (l+p).(l+p)) / l.n
    , 
    Qv[k] (k.k)^-1 1/p.n (S1 p.{mu} + S2 k.{mu} + S3 n.{mu} k.k/(2 k.n))
    ,
    {p.{mu}, k.{mu}, n.{mu}}
    ,
    {S1, S2, S3}
  ] /. {n.n -> 0, k.n -> x, p.n -> 1, q.n -> 1-x};

  UT$AssertEquivalent[
	S1 /. $result
    ,
	$$ExpandMPV[S1[eps]]
  ];

UT$EndTestCase[];
