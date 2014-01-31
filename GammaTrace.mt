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


<< UnitTest`

<< Axiloop`GammaTrace`


UT$BeginTestCase["GammaTrace"];

  UT$AssertEquivalent[
    GammaTrace[G[{mu}]**G[{mu}], NumberOfDimensions -> 4 + eps]
    ,
    4 (4 + eps)
  ];

  UT$AssertEquivalent[
    GammaTrace[G[{mu}]**G[{nu}]**G[{mu}]**G[{nu}], NumberOfDimensions -> 4 - eps]
    ,
    - 4 ((4 - eps))^2 + 8 (4 - eps)
  ];

  UT$AssertEquivalent[
    GammaTrace[G[{mu}]**G[{nu}], NumberOfDimensions -> 4 + eps]
    ,
    4 {mu}.{nu}
  ];

  UT$AssertEquivalent[
    GammaTrace[
      G[{mu}]**G[{nu}]**G[{mu}]**G[{xi}],
      NumberOfDimensions -> 4 + eps
    ]
    ,
    - 4 (2 + eps) {nu}.{xi}
  ];

  UT$AssertEquivalent[
    GammaTrace[
      G[{mu}, Line->f2]**G[{nu}]**G[{mu}, Line->f2]**G[{xi}],
      NumberOfDimensions -> 4 + eps
    ]
    ,
    4 (4 + eps) 4 {nu}.{xi}
  ];

UT$EndTestCase[];
