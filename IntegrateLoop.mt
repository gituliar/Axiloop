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


UT$BeginTestCase["IntegrateLoop"];

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
    Qv[q] T1 (-3 k.k - p.p + q.q) / 2
  ];

UT$EndTestCase[];


UT$BeginTestCase["$$CollectLoopIntegrals"];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.k l.p / l.n + X, l]
    ,
    $$[{k,p},{},{0}] + X
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[(l.k)^2 l.p / l.n, l]
    ,
    $$[{k,k,p},{},{0}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[(l.k)^2 (l.p)^3 / l.n, l]
    ,
    $$[{k,k,p,p,p},{},{0}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[(l.k)^2 (l.p)^3 l.q / l.n, l]
    ,
    $$[{k,k,p,p,p,q},{},{0}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.k l.p / l.n + l.q l.p / (k.n l.n), l]
    ,
    $$[{k,p},{},{0}] + $$[{p,q},{},{0}] / k.n
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[1 / ((l+a).(l+a) l.n), l]
    ,
    $$[{},{a},{0}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.a / ((l-b).(l-b) (l+c).(l+c) l.n), l]
    ,
    $$[{a},{-b,c},{0}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.a / ((l-b).(l-b) l.n), l]
    ,
    $$[{a},{-b},{0}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.a / ((l+b).n l.n), l]
    ,
    $$[{a},{},{0,b}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.x l.p / ((l-y).(l-y) (l-z).n), l]
    ,
    $$[{p,x},{-y},{-z}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[(l.p)^3 l.x / ((l-y).(l-y) (l-z).n), l]
    ,
    $$[{p,p,p,x},{-y},{-z}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.x / (l.l (l-y).(l-y) (l-z).n), l]
    ,
    $$[{x},{0,-y},{-z}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.x / ((l-p).(l-p) (l-y).(l-y) (l-z).n), l]
    ,
    $$[{x},{-p,-y},{-z}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.x / ((l+p).(l+p) (l-y).(l-y) (l-z).n), l]
    ,
    $$[{x},{p,-y},{-z}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.x / ((l-y).(l-y) l.n (l-z).n), l]
    ,
    $$[{x},{-y},{0,-z}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.x / ((l-y).(l-y) (l-z).n (l-p).n), l]
    ,
    $$[{x},{-y},{-p,-z}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.x / ((l-y).(l-y) (l-z).n (-l+p).n), l]
    ,
    - $$[{x},{-y},{-p,-z}]
  ];

  UT$AssertEquivalent[
    $$CollectLoopIntegrals[l.x / ((l-y).(l-y) (l-z).n (l+p).n), l]
    ,
    $$[{x},{-y},{p,-z}]
  ];

(*
  UT$AssertEquivalent[
    Try[CollectLoopIntegrals[1 / (l.l (l+x).(l+x) (l+y).(l+y) (l+p).(l+n)), l]]
    ,
    $UnevaluatedError
    ,
    {Axiloop`Integrate`$$CollectLoopIntegrals::unevaluated}
    ,
    TestID->"CollectLoopIntegrals-20130328-N4T8G4"
  ];
*)

UT$EndTestCase[];


UT$BeginTestCase["SimplifyAlgebraic"];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{-k,-p},{}]]
    ,
    $$[{},{k,p},{}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{-k,-p},{0}]]
    ,
    - $$[{},{k,p},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k},{-k,-p},{}]]
    ,
    - $$[{k},{k,p},{}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k},{-k,-p},{0}]]
    ,
    $$[{k},{k,p},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{0,-k,-p},{}]]
    ,
    $$[{},{0,k,p},{}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{0,-k,-p},{0}]]
    ,
    - $$[{},{0,k,p},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k},{0,-k,-p},{}]]
    ,
    - $$[{k},{0,k,p},{}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k},{0,-k,-p},{0}]]
    ,
    1/2 ($$[{}, {0,p}, {0}] - $$[{}, {k,p}, {0}] - k.k $$[{}, {0,k,p}, {0}])
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p},{0,-k,-p},{}]]
    ,
    - $$[{p},{0,k,p},{}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p},{0,-k,-p},{0}]]
    ,
    1/2 ($$[{}, {0,k}, {0}] - $$[{}, {k,p}, {0}] - p.p $$[{}, {0,k,p}, {0}])
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k,k},{0,-k,-p},{}]]
    ,
    $$[{k,k},{0,k,p},{}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k,k},{0,-k,-p},{0}]]
    ,
    - 1/2 (
        $$[{k},{0,p},{0}]
        - k.k/2 (
            $$[{}, {0,p}, {0}]
            - $$[{}, {k,p}, {0}]
            - k.k $$[{}, {0,k,p}, {0}])
        - $$[{k},{k,p},{0}]
    )
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k,p},{0,-k,-p},{}]]
    ,
    $$[{k,p},{0,k,p},{}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k,p},{0,-k,-p},{0}]]
    ,
    - 1/2 (
        $$[{k},{0,k},{0}]
        - $$[{k},{k,p},{0}]
        - p.p 1/2 (
            $$[{}, {0,p}, {0}]
            - $$[{}, {k,p}, {0}]
            - k.k $$[{}, {0,k,p}, {0}]
        )
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k,k},{0,-k,-p},{0}]]
    ,
    - 1/2 (
        $$[{k},{0,p},{0}]
        - k.k/2 (
            $$[{}, {0,p}, {0}]
            - $$[{}, {k,p}, {0}]
            - k.k $$[{}, {0,k,p}, {0}]
        )
        - $$[{k},{k,p},{0}]
    )
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{k,p},{0,k}]]
    ,
    ($$[{}, {k,p}, {0}] - $$[{}, {k,p}, {k}]) / k.n
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{k,p},{k,p}]]
    ,
    ($$[{}, {k,p}, {p}] - $$[{}, {k,p}, {k}]) / (k.n-n.p)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{k,p},{0,k,p}]]
    ,
    (
          ($$[{},{k,p},{0}] - $$[{},{k,p},{p}]) / n.p
        - ($$[{},{k,p},{p}] - $$[{},{k,p},{k}]) / (k.n - n.p)
    ) / k.n
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{-k,-p},{}]]
    ,
    $$[{},{k,p},{}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{-k,-p},{-k}]]
    ,
    - $$[{},{k,p},{k}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{-k,-p},{-p}]]
    ,
    - $$[{},{k,p},{p}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{},{-k,-p},{-k,-p}]]
    ,
    ($$[{},{k,p},{k}] - $$[{},{k,p},{p}]) / (p.n - k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k},{-k,-p},{-k}]]
    ,
    $$[{k},{k,p},{k}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k},{-k,-p},{-p}]]
    ,
    $$[{k},{k,p},{p}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k}, {-k,-p}, {-k,-p}]]
    ,
    - ($$[{k},{k,p},{k}] - $$[{k},{k,p},{p}]) / (p.n-k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k}, {0,-k,-p}, {}]]
    ,
    - $$[{k}, {0,k,p}, {}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k}, {0,-k,-p}, {-k}]]
    ,
    1/2 (
        $$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}]
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k}, {0,-k,-p}, {-p}]]
    ,
    1/2 (
        $$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}]
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k}, {0,-k,-p}, {-k,-p}]]
    ,
    - 1/2 (
         $$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}]
        - 
        ($$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}])
    ) / (p.n-k.n)
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{l}, {-k, -p}, {-k, -p}]]
    ,
    (
        $$[{}, {p}, {k}] - 2 $$[{k}, {k,p}, {k}] - k.k $$[{}, {k,p}, {k}]
        - ($$[{}, {p}, {p}] - 2 $$[{k}, {k,p}, {p}] - k.k $$[{}, {k,p}, {p}])
    ) / (p.n-k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n}, {-k, -p}, {-k}]]
    ,
    $$[{}, {k,p}, {}] - k.n $$[{}, {k,p}, {k}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n}, {-k, -p}, {-p}]]
    ,
    $$[{}, {k,p}, {}] - p.n $$[{}, {k,p}, {p}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n}, {-k, -p}, {-k, -p}]]
    ,
    - ($$[{}, {k,p}, {}] - k.n $$[{}, {k,p}, {k}]
        - ($$[{}, {k,p}, {}] - p.n $$[{}, {k,p}, {p}])
    ) / (p.n-k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n}, {0, -k, -p}, {}]]
    ,
    - $$[{n}, {0,k,p}, {}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n}, {0, -k, -p}, {-k}]]
    ,
    $$[{}, {0,k,p}, {}] - k.n $$[{}, {0,k,p}, {k}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n}, {0, -k, -p}, {-p}]]
    ,
    $$[{}, {0,k,p}, {}] - p.n $$[{}, {0,k,p}, {p}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n}, {0, -k, -p}, {-k, -p}]]
    ,
    - ($$[{}, {0,k,p}, {}] - k.n $$[{}, {0,k,p}, {k}]
        - ($$[{}, {0,k,p}, {}] - p.n $$[{}, {0,k,p}, {p}])
    ) / (p.n-k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p}, {-k, -p}, {-k}]]
    ,
    $$[{p}, {k,p}, {k}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p}, {-k, -p}, {-p}]]
    ,
    $$[{p}, {k,p}, {p}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p}, {-k, -p}, {-k, -p}]]
    ,
    - ($$[{p}, {k,p}, {k}] - $$[{p}, {k,p}, {p}]) / (p.n-k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p}, {0, -k, -p}, {}]]
    ,
    - $$[{p}, {0,k,p}, {}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p}, {0, -k, -p}, {-k}]]
    ,
    1/2 (
        $$[{}, {0,k}, {k}] - $$[{}, {k,p}, {k}] - p.p $$[{}, {0,k,p}, {k}]
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p}, {0, -k, -p}, {-p}]]
    ,
    1/2 (
        $$[{}, {0,k}, {p}] - $$[{}, {k,p}, {p}] - p.p $$[{}, {0,k,p}, {p}]
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p}, {0, -k, -p}, {-k, -p}]]
    ,
    - 1/2 (
        $$[{}, {0,k}, {k}] - $$[{}, {k,p}, {k}] - p.p $$[{}, {0,k,p}, {k}]
            - ($$[{}, {0,k}, {p}] - $$[{}, {k,p}, {p}] - p.p $$[{}, {0,k,p}, {p}])
    ) / (p.n-k.n)
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, k}, {0, -k, -p}, {}]]
    ,
    $$[{k,k}, {0,k,p}, {}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, k}, {0, -k, -p}, {-p}]]
    ,
    - 1/2 (
        $$[{k}, {0,p}, {p}] - $$[{k}, {k,p}, {p}]
            - k.k 1/2 ($$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}] )
    )
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, n}, {-k, -p}, {-k, -p}]]
    ,
    (- k.n $$[{k}, {k,p}, {k}] + p.n $$[{k}, {k,p}, {p}]) / (p.n-k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, n}, {0, -k, -p}, {}]]
    ,
    $$[{k,n}, {0,k,p}, {}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, n}, {0, -k, -p}, {-k}]]
    ,
    - ($$[{k}, {0,k,p}, {}]
        - k.n 1/2 ($$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}])
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, n}, {0, -k, -p}, {-p}]]
    ,
    - ($$[{k}, {0,k,p}, {}]
        - p.n 1/2 ($$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}])
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, n}, {0, -k, -p}, {-k, -p}]]
    ,
    (
        ($$[{k}, {0,k,p}, {}]
            - k.n 1/2 ($$[{}, {0,p}, {k}] - $$[{}, {k,p}, {k}] - k.k $$[{}, {0,k,p}, {k}])
        )
        -
        ($$[{k}, {0,k,p}, {}]
            - p.n 1/2 ($$[{}, {0,p}, {p}] - $$[{}, {k,p}, {p}] - k.k $$[{}, {0,k,p}, {p}])
        )
    ) / (p.n-k.n)
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, p}, {0, -k, -p}, {}]]
    ,
    $$[{k,p}, {0,k,p}, {}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, p}, {0, -k, -p}, {-k}]]
    ,
    - 1/2 (
        $$[{k}, {0,k}, {k}]
        - $$[{k}, {k,p}, {k}]
        - p.p 1/2 (
             $$[{}, {0,p}, {k}]
             - $$[{}, {k,p}, {k}]
             - k.k $$[{}, {0,k,p}, {k}]
        )
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, p}, {0, -k, -p}, {-p}]]
    ,
    - 1/2 (
        $$[{k}, {0,k}, {p}]
        - $$[{k}, {k,p}, {p}]
        - p.p 1/2 (
             $$[{}, {0,p}, {p}]
             - $$[{}, {k,p}, {p}]
             - k.k $$[{}, {0,k,p}, {p}]
        )
    )
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, n}, {-k, -p}, {-k, -p}]]
    ,
    (
        p.n ($$[{}, {k,p}, {}] - p.n $$[{}, {k,p}, {p}])
        -
        k.n ($$[{}, {k,p}, {}] - k.n $$[{}, {k,p}, {k}])
    ) / (p.n-k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, n}, {0, -k, -p}, {}]]
    ,
    $$[{n,n}, {0,k,p}, {}]
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, n}, {0, -k, -p}, {-k}]]
    ,
    - (
        $$[{n}, {0,k,p}, {}]
        -
        k.n (
            $$[{}, {0,k,p}, {}]
            -
            k.n $$[{}, {0,k,p}, {k}]
        )
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, n}, {0, -k, -p}, {-p}]]
    ,
    - (
        $$[{n}, {0,k,p}, {}]
        -
        p.n (
            $$[{}, {0,k,p}, {}]
            -
            p.n $$[{}, {0,k,p}, {p}]
        )
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, n}, {0, -k, -p}, {-k, -p}]]
    ,
    (
        (
            $$[{n}, {0,k,p}, {}]
            -
            k.n (
                $$[{}, {0,k,p}, {}]
                -
                k.n $$[{}, {0,k,p}, {k}]
            )
        )
        -
        (
            $$[{n}, {0,k,p}, {}]
            -
            p.n (
                $$[{}, {0,k,p}, {}]
                -
                p.n $$[{}, {0,k,p}, {p}]
            )
        )
    ) / (p.n-k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, p}, {-k, -p}, {-k, -p}]]
    ,
    (
        (
            $$[{p}, {k,p}, {}]
            -
            k.n $$[{p}, {k,p}, {k}]
        )
        -
        (
            $$[{p}, {k,p}, {}]
            -
            p.n $$[{p}, {k,p}, {p}]
        )
    ) / (p.n-k.n)
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, p}, {0, -k, -p}, {}]]
    ,
    $$[{n,p}, {0,k,p}, {}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, p}, {0, -k, -p}, {-k}]]
    ,
    - (
        $$[{p}, {0,k,p}, {}]
        -
        k.n 1/2 (
            $$[{}, {0,k}, {k}]
            -
            $$[{}, {k,p}, {k}]
            -
            p.p $$[{}, {0,k,p}, {k}]
        )
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, p}, {0, -k, -p}, {-p}]]
    ,
    - (
        $$[{p}, {0,k,p}, {}]
        -
        p.n 1/2 (
            $$[{}, {0,k}, {p}]
            -
            $$[{}, {k,p}, {p}]
            -
            p.p $$[{}, {0,k,p}, {p}]
        )
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, p}, {0, -k, -p}, {-k, -p}]]
    ,
    (
        (
            $$[{p}, {0,k,p}, {}]
            -
            k.n 1/2 (
                $$[{}, {0,k}, {k}]
                -
                $$[{}, {k,p}, {k}]
                -
                p.p $$[{}, {0,k,p}, {k}]
            )
        )
        -
        (
            $$[{p}, {0,k,p}, {}]
            -
            p.n 1/2 (
                $$[{}, {0,k}, {p}]
                -
                $$[{}, {k,p}, {p}]
                -
                p.p $$[{}, {0,k,p}, {p}]
            )
        )
    ) / (p.n-k.n)
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p, p}, {0, -k, -p}, {}]]
    ,
    $$[{p,p}, {0,k,p}, {}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{p, p}, {0, -k, -p}, {-k}]]
    ,
    - 1/2 (
        $$[{p}, {0,k}, {k}]
        -
        $$[{p}, {k,p}, {k}]
        -
        p.p 1/2 (
            $$[{}, {0,k}, {k}]
            -
            $$[{}, {k,p}, {k}]
            -
            p.p $$[{}, {0,k,p}, {k}]
        )
    )
  ];

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{k, n, n}, {0, -k, -p}, {-k, -p}]]
    ,
    - (
        (
            $$[{k,n}, {0,k,p}, {}]
            -
            k.n (
                $$[{k}, {0,k,p}, {}]
                -
                k.n 1/2 (
                    $$[{}, {0,p}, {k}]
                    -
                    $$[{}, {k,p}, {k}]
                    -
                    k.k $$[{}, {0,k,p}, {k}]
                )
            )
        )
        -
        (
            $$[{k,n}, {0,k,p}, {}]
            -
            p.n (
                $$[{k}, {0,k,p}, {}]
                -
                p.n 1/2 (
                    $$[{}, {0,p}, {p}]
                    -
                    $$[{}, {k,p}, {p}]
                    -
                    k.k $$[{}, {0,k,p}, {p}]
                )
            )
        )
    ) / (p.n-k.n)
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, n, n}, {0, -k, -p}, {-k, -p}]]
    ,
    - (
        (
            $$[{n,n}, {0,k,p}, {}]
            -
            k.n (
                $$[{n}, {0,k,p}, {}]
                -
                k.n (
                    $$[{}, {0,k,p}, {}]
                    -
                    k.n $$[{}, {0,k,p}, {k}]
                )
            )
        )
        -
        (
            $$[{n,n}, {0,k,p}, {}]
            -
            p.n (
                $$[{n}, {0,k,p}, {}]
                -
                p.n (
                    $$[{}, {0,k,p}, {}]
                    -
                    p.n $$[{}, {0,k,p}, {p}]
                )
            )
        )
    ) / (p.n-k.n)
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyAlgebraic[$$[{n, n, p}, {0, -k, -p}, {-k, -p}]]
    ,
    - (
        (
            $$[{n,p}, {0,k,p}, {}]
            -
            k.n (
                $$[{p}, {0,k,p}, {}]
                -
                k.n 1/2 (
                    $$[{}, {0,k}, {k}]
                    -
                    $$[{}, {k,p}, {k}]
                    -
                    p.p $$[{}, {0,k,p}, {k}]
                )
            )
        )
        -
        (
            $$[{n,p}, {0,k,p}, {}]
            -
            p.n (
                $$[{p}, {0,k,p}, {}]
                -
                p.n 1/2 (
                    $$[{}, {0,k}, {p}]
                    -
                    $$[{}, {k,p}, {p}]
                    -
                    p.p $$[{}, {0,k,p}, {p}]
                )
            )
        )
    ) / (p.n-k.n)
  ];
*)

UT$EndTestCase[];


UT$BeginTestCase["$$SimplifyTranslate"];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{},{p},{k}]]
    ,
    $$[{},{q},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{},{p},{p}]]
    ,
    $$[{},{0},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{},{0,k},{k}]]
    ,
    - $$[{},{0,k},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{},{0,k},{p}]]
    ,
    - $$[{},{p,q},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{},{0,p},{k}]]
    ,
    - $$[{},{k,-q},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{},{0,p},{p}]]
    ,
    - $$[{},{0,p},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{}, {k, p}, {k}]]
    ,
    $$[{},{0,q},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{}, {k, p}, {p}]]
    ,
    - $$[{},{0,q},{0}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{}, {0, k, p}, {k}]]
    ,
    - $$[{},{0,k,-q},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{}, {0, k, p}, {p}]]
    ,
    - $$[{},{0,p,q},{0}]
  ];
*)

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{k}, {0, k}, {k}]]
    ,
    $$[{k},{0,k},{0}] + k.k $$[{},{0,k},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{k}, {0, k}, {p}]]
    ,
    $$[{k},{p,q},{0}] + k.p $$[{},{p,q},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{k}, {0, p}, {p}]]
    ,
    $$[{k},{0,p},{0}] + k.p $$[{},{0,p},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{k}, {k, p}, {k}]]
    ,
    $$[{k},{0,q},{0}] - k.k $$[{},{0,q},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{k}, {k, p}, {p}]]
    ,
    $$[{k},{0,q},{0}] + k.p $$[{},{0,q},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{p}, {0, k}, {k}]]
    ,
    $$[{p},{0,k},{0}] + k.p $$[{},{0,k},{0}]
  ];

  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{p}, {k, p}, {k}]]
    ,
    $$[{p},{0,q},{0}] - k.p $$[{},{0,q},{0}]
  ];

(*
  UT$AssertEquivalent[
    $$SimplifyTranslate[$$[{p}, {k, p}, {p}]]
    ,
    $$[{p},{0,q},{0}] + p.p $$[{},{0,q},{0}]
  ];
*)

UT$EndTestCase[];



UT$BeginTestCase["PaVeExpand"];

  PaVeExpand = $$ExpandMPV;


  UT$AssertEquivalent[
    Simplify[ PaVeExpand[H1] - ( (2 I0 + Log[1-x] - 11/3) / eir - 85/9) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[H2] - ( 1/(3 eir) + 11/9 ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[H3] - ( 1/(6 eir) + 4/9 ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[H4] - ( 1/(3 eir) + 13/18 ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[H5] - ( -1/(12 euv) - 11/36 ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[H6] - ( -1/(12 euv) - 2/9 ) ]
    ,
    0
  ];


  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S1] - (- (2 I0 + Log[1-x] + x Log[x]/(1-x)) / eir + 4 I1 - 2 I0 Log[1-x] + x Li2[1-x]/(1-x) - Log[1-x]^2/2) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S2] - ( Log[x]/((1-x) eir) - Li2[1-x]/(1-x) ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S3] - ( - (I0 + Log[x]/(1-x)) / euv + I1 - I0 Log[x] + x Li2[1-x]/(1-x) - Log[x]^2/2 - Li2[1] ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S4] - 1/(1-x) ( (2 (1-x) I0 - x^2 Log[x]/(1-x) + (1-x) Log[1-x] + x - 2) / eir - 4 (1-x) I1 + 2 (1-x) I0 Log[1-x] + x^2 Li2[1-x]/(1-x) + (1-x) Log[1-x]^2/2 - x^2 Log[x]/(1-x) + 2 x - 4 ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S5] - 1/(1-x)^2 ( (x Log[x] - x + 1) / eir - x Li2[1-x] + x Log[x] - 2 x + 2 ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S6] - 1/(1-x)^2 ( - (Log[x] - x + 1) / eir + Li2[1-x] - Log[x] + 2 x - 2) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S7] - 1/(1-x)^2 ( - (Log[x] - x + 1) / euv + Li2[1-x] - (2-x) Log[x] + 2 x - 2) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S8] - 1/(1-x)^2 ( (x Log[x] - x + 1) / euv - x Li2[1-x] + x Log[x] - 2 x + 2 ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S9] - 1/(1-x) ( ((1-x) I0 + (1-2x) Log[x]/(1-x) + x - 2) / euv - (1-x) I1 + (1-x) I0 Log[x] + x^2 Li2[1-x]/(1-x) + (1-x) Log[x]^2/2 - x^2 Log[x]/(1-x) + (1-x) Li2[1] + 2 x - 4) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[S10] - 1/(2 (1-x)) ( Log[x] / euv - Li2[1-x] + 2 Log[x]) ]
    ,
    0
  ];




  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U1] - ( - (2 I0 - x Log[x] + (1+x) Log[1-x]) / eir + 4 I1 - 2 I0 Log[1-x] + x Li2[1-x] - (1-x) Log[1-x]^2/2 - 2 x Li2[1]) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U2] - ( - (I0 + Log[1-x]) / eir + I1 - I0 Log[x] + Li2[1-x] - Log[x]^2/2 + Log[1-x]^2/2 - 3 Li2[1]) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U3] - ( (I0 + (1-x) Log[x] + x Log[1-x]) / euv - I1 + I0 Log[x] - x Li2[1-x] + Log[x]^2/2 - x Log[1-x]^2/2 + (1+2x) Li2[1]) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U4] - ( (2 I0 - x^2 Log[x] + (1+x^2) Log[1-x] + x - 2) / eir - 4 I1 + 2 I0 Log[1-x] - x^2 Li2[1-x] + (1-x^2) Log[1-x]^2/2 - x^2 Log[x] + x^2 Log[1-x] + 2 x^2 Li2[1] + 2 x - 4 ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U5] - ( 1 / eir + x Log[x] - x Log[1-x] + 2) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U6] - ( (I0 + Log[1-x] - 1) / eir - I1 + I0 Log[x] - Li2[1-x] + Log[x]^2/2 - Log[1-x]^2/2 - Log[x] + Log[1-x] + 3 Li2[1] - 2 ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U7] - ( (- (1-x) Log[x] + (1-x) Log[1-x] - 1) / euv - (1-x) Li2[1-x] - (1-x) Log[1-x]^2/2 - (2-x) Log[x] + (2-x) Log[1-x] + 2 (1-x) Li2[1] - 2) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U8] - ( - (I0 + (1-x) Log[x] + x Log[1-x] - 1) / euv + I1 - I0 Log[x] + x Li2[1-x] - Log[x]^2/2 + x Log[1-x]^2/2 + x Log[x] - x Log[1-x] - (1+2x) Li2[1] + 2) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U9] - ( (I0 + (1-x^2) Log[x] + x^2 Log[1-x] + x - 2) / euv - I1 + I0 Log[x] - x^2 Li2[1-x] + Log[x]^2/2 - x^2 Log[1-x]^2/2 - x^2 Log[x] + x^2 Log[1-x] + (1+2x^2) Li2[1] + 2 x - 4) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[U10] - (1-x)/2 ( (Log[x] - Log[1-x]) / euv + Li2[1-x] + Log[1-x]^2/2 + 2 Log[x] - 2 Log[1-x] - 2 Li2[1]) ]
    ,
    0
  ];


  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W1] - ( ((-2+4x) I0 - 2 x Log[x] - (1-3x) Log[1-x]) / eir + (4-7x) I1 - 3 x I0 Log[x] + (-2+3x) I0 Log[1-x] + 2 x Li2[1-x] + x Log[x]^2 - (2+x) Log[1-x]^2/4 + x Log[x] Log[1-x] + 5 x Li2[1] ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W2] - ( (I0 + Log[1-x]) / eir - 2 I1 + I0 Log[1-x] + Log[1-x]^2/4) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W3] - ( - (x I0 + Log[1-x]) / euv + 2 x I1 - x I0 Log[1-x] - x Log[1-x]^2/4) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W4] - ( ((4-5x) I0 + 2 x Log[x] + (2-3x) Log[1-x] + x - 2) / eir - (8-9x) I1 + 3 x I0 Log[x] + 4 (1-x) I0 Log[1-x] - 2 x Li2[x] - 2 x Li2[1-x] - x Log[x]^2 + (2-x) Log[1-x]^2/2 - x Log[x] Log[1-x] + x Log[1-x] - 5 x Li2[1] + 2 (2-x) (x^2 + x - 1)/(1-x) ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W5] - ( - (I0 + Log[1-x] - 1) / eir + 2 I1 - I0 Log[1-x] + 2 Li2[x] + Log[1-x]^2/4 - Log[1-x] + 2 (1-3x+x^2)/(1-x)) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W6] - ( - 1 / eir - 2 Li2[x]/x - Log[1-x]^2/(2x) + Log[1-x]/x + 2/(1-x) ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W7] - ( (I0 + Log[1-x]/x - 1) / euv - 2 I1 + I0 Log[1-x] - 2 (2-x) Li2[x]/x - (4-3x) Log[1-x]^2/(4x) + (2 - x) Log[1-x]/x + 2 (3-3x+x^2)/(1-x) ) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W8] - ( ((1-x) Log[1-x] + x) / (x euv) + 2 Li2[x]/x + Log[1-x]^2/(2x) - Log[1-x] - 2/(1-x)) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W9] - ( - (2 (1-x) Log[1-x] - x^2 + 2x) / (x euv) - 2 (2-2x+x^2) Li2[x]/x - (2-2x+x^2) Log[1-x]^2/(2x) + x Log[1-x] + 2 (2-x) (1-x+x^2)/(1-x)) ]
    ,
    0
  ];

  UT$AssertEquivalent[
    Simplify[ PaVeExpand[W10] - (1-x)/x ( - Log[1-x] / (2 euv) + Li2[x] + Log[1-x]^2/4 - Log[1-x] + (x^2-2x)/(1-x) ) ]
    ,
    0
  ];

UT$EndTestCase[];
