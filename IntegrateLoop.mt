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
    I 2^(-5+2 eir) Pi^(-2+eir) T1 Gamma[1+eir] (-3 k.k - p.p + q.q)/(q.q)^eir
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
