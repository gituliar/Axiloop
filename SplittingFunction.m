(*
;; Copyright (c) 2012-2014 Oleksandr Gituliar <oleksandr@gituliar.org>.
;;
;; This file is part of Axiloop package.
*)

BeginPackage["Axiloop`SplittingFunction`", {
  "Logging`",
  "Timestamp`",

  "Axiloop`Core`",
  "Axiloop`FeynmanRules`",
  "Axiloop`GammaTrace`",
  "Axiloop`IntegrateLeg`",
  "Axiloop`IntegrateLoop`"
  }];

  SplittingFunction;
  AX$CollectG1;
  $Pgg; $Pgq; $Pqg; $Pqq;

  Begin["`Private`"];

    Options[SplittingFunction] = {IntegrateLoopPrescription -> "MPV"};
    SplittingFunction[$topology_, $LO_:Null, OptionsPattern[]] := Block[
      {$G, $G1, $Wb, $Wbs, $Wn, $Wr, $Wz, $Z, $result, integrated},
    
      TS$Begin["SplittingFunction"];
    
      $Wn = GammaTrace[$topology, NumberOfDimensions -> 4 + 2 eps];
      $Wn = $Wn /. {n.n -> 0};
      $Wn = Expand[$Wn /. $kinematicRules /. {g^_Integer (p.p | q.q) :> 0}];
      TS$Print["$Wn"];
    
      integrated = IntegrateLoop[ $Wn, l,
        Prescription -> OptionValue[IntegrateLoopPrescription]
      ];
      $Wbs = If[$LO =!= Null, $$ExpandPaVe[$Get[integrated, {"integrated", "short"}]], Null];
      $Wbs = Expand[$Wbs /. {k.n -> x, n.p -> 1, n.q -> 1-x}];
      $Wbs = $Wbs /. {p.p -> 0, q.q -> 0};
      TS$Print["$Wbs"];
    
      $Wb = $Get[integrated, {"integrated", "long"}];
      $Wb = Expand[$Wb /. {k.n -> x, n.p -> 1, n.q -> 1-x}];
      $Wb = $Wb /. {2^(2 eps) -> 4^eps};
      $Wb = $Wb /. {p.p -> 0, q.q -> 0};
      TS$Print["$Wb"];
    
      $Z = If[ $LO =!= Null, Simplify[ PolePart[$Wb, euv] / $Get[$LO, "Wr"] /. {Qv[_] :> Qv, eps -> 0} ], 0];
      TS$Print["$Z"];
    
      $Wz = If[ $LO =!= Null, $Z $Get[$LO, "Wr"], 0];
      TS$Print["$Wz"];
    
      $Wr = ($Wb - $Wz / euv) /. {eir -> eps, euv -> eps};
      $Wr = $Wr /. {p.p -> 0, Qv[p] -> 0, q.q -> 0, Qv[q] -> 0};
      TS$Print["$Wr"];
    
      $G = IntegrateLeg[$Wr, 4 + 2 eps];
      TS$Print["$G"];
    
      $G1 = PolePart[$G, eps];
      $G1 = ExpandPhaseSpaceLeg[$G1] /. eps -> 0;
      $G1 = ExpandPhaseSpaceLoop[$G1] /. eps -> 0;
      TS$Print["$G1"];
    	
      $result = {
        {"Wn", $Wn},
        {"Wr", $Wr //. {Qv[r_] :> Qv (-r.r)^eps, Qv -> I (4 Pi)^(-2)}},
        {"G1", $G1}
      };
    
      If[ $LO =!= Null, AppendTo[$result,#]& /@ {
        {"integrated", integrated},
        {"Wb",  $Wb},
        {"Wbs", $Wbs},
        {"Wz",  $Wz /. {Qv -> I (4 Pi)^(-2)}}
      } ];
    
      $result
    ];

    AX$CollectG1[expr_, case_] := Block[
      {$Pgq, $bases, $basis, $expr, $i, $result, $term},

      Switch[
        case
        ,
        "gq"
        ,
        $bases = {
          $Pgq Log[1-x]^2,
          $Pgq Log[x]^2,
          $Pgq Log[x] Log[1-x],
          $Pgq I0 Log[x],
          $Pgq I0 Log[1-x],
          $Pgq Log[x],
          $Pgq Log[1-x],
          $Pgq Li2[1-x],
          $Pgq Li2[1],
          $Pgq I0,
          $Pgq I1,
          Log[x]^2,
          Log[x],
          Log[1-x],
          I0
        };
        $rule = $Pgq -> 1-2x+2x^2;
        ,
        "qg"
        ,
        $bases = {
          $Pqg Log[1-x]^2,
          $Pqg Log[x]^2,
          $Pqg Log[x] Log[1-x],
          $Pqg I0 Log[x],
          $Pqg I0 Log[1-x],
(*
          $Pqg Log[x],
*)
          $Pqg Log[1-x],
          $Pqg Li2[1-x],
          $Pqg Li2[1],
(*
          $Pqg I0,
*)
          $Pqg I1,
          x Log[x]^2,
          Log[x]^2,
          x Log[x],
          1/x Log[x],
          Log[x],
          x Log[1-x],
          Log[1-x],
          x I0,
          1/x I0,
          I0,
          x,
          1/x
        };
        $rule = $Pqg -> (2-2x+x^2)/x;
      ];
      $result = {};

      $expr = Expand[expr];
      For[$i=1, $i<=Length[$bases], $i++,
        $basis = $bases[[$i]];
        $term = Simplify[Coefficient[$expr, $basis /. $rule]];
        $expr = Expand[$expr - ($term $basis /. $rule)];

        AppendTo[$result, {$basis, $term}];
      ];

      AppendTo[$result, {"rest", $expr}];

      $result
    ];

  End[];

EndPackage[];
