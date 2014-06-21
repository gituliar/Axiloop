#!/usr/bin/math -script

(*============================================================================*)
(*                                                                            *)
(*  Copyright (C) 2014 Oleksandr Gituliar.                                    *)
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


Needs["Axiloop`"];
Needs["UnitTest`"];


UT$BeginTestCase["NLO-gg-D"];

  AX$Get["NLO-gg-D.ms"];


  UT$AssertEquivalent[
    $Get[$result, "Wz"]
    ,
    as^2 / k.k Pgg 16 (11/3 - 2 Log[x] - 2 Log[1-x] - 6 I0)
  ];


  UT$AssertEquivalent[
    $Get[$result, "Wbs"]
    ,
    AX$Get["NLO-gg-D.ebs.mx"]
  ];


  $Wb = $Get[$result, "Wb"];

  $W0k = PolePart[$Cases[$Wb, Qv[k]] /. {eir -> eps, euv -> eps}, eps, 0];

  UT$AssertEquivalent[
    $W0k
    ,
    I g^4 / k.k ( 32 Pgg ( 4 I1 - 3 Li2[1] - I0 Log[1 - x] - 2 I0 Log[x] - Log[x]^2 ) + ( 1072/9 Pgg + 8/3 x ))
  ];

  $Wirk =  PolePart[$Cases[$Wb, Qv[k]], eir, -1] /. {eps -> 0};

  UT$AssertEquivalent[
    $Wirk
    ,
    I g^4 / k.k ( 16 Pgg ( 3 I0 + 2 Log[1 - x] - Log[x]) + ( -44/x - 44/3 Pgg + 44 - 48 x ) )
  ];

  $Wuvk =  PolePart[$Cases[$Wb, Qv[k]], euv, -1] /. {eps -> 0, eir -> 0};

  UT$AssertEquivalent[
    $Wuvk
    ,
    I g^4 / k.k ( 16 Pgg ( 3 I0 + 3 Log[x]) + ( 44/x - 44 Pgg - 44 + 48 x ) )
  ];

  $Wuvp =  PolePart[$Cases[$Wb, Qv[p]], euv, -1] /. {eps -> 0, eir -> 0};

  UT$AssertEquivalent[
    $Wuvp
    ,
    I g^4 / k.k ( 8 Pgg ( 3 I0 + Log[1 - x] - Log[x]) + 1/3 ( -44/x - 44 Pgg - 4 - 4 x - 44 x^2 ) )
  ];

  $Wuvq =  PolePart[$Cases[$Wb, Qv[q]], euv, -1] /. {eps -> 0, eir -> 0};

  UT$AssertEquivalent[
    $Wuvq
    ,
    I g^4 / k.k ( 8 Pgg ( 3 I0 + 3 Log[1 - x] - Log[x]) + 1/3 ( -88/x + 136 - 140 x + 44 x^2 ))
  ];


(*
$virt = Expand[Simplify[$Get[$result, "inclusive"]]];

Test[
	$virt
	,
	g^4/(576 Pi^4 (-1 + x) x) (-134 - 144 I1 + 268 x + 288 I1 x - 405 x^2 - 432 I1 x^2 + 271 x^3 + 288 I1 x^3 - 134 x^4 - 144 I1 x^4 + 108 Li2[1] - 216 x Li2[1] + 324 x^2 Li2[1] - 216 x^3 Li2[1] + 108 x^4 Li2[1] + 36 (1 - x + x^2)^2 Log[1 - x]^2 + 72 I0 (1 - x + x^2)^2 Log[x] + 36 (1 - x + x^2)^2 Log[x]^2 + 6 (1 - x + x^2)^2 Log[1 - x] (-11 + 24 I0 + 6 Log[x]))
	,
	EquivalenceFunction -> EquivalentQ
	,
	TestID -> "NLO-D-20140105-N3M2Z0"
];
*)

UT$EndTestCase[];
