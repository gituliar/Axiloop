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

    AX$CollectG1[expr_, basis_] := Block[
      {$result},

      $result = Collect[expr, {I0 Log[x], I0 Log[1-x], I1, I0, Log[x]^2, Log[x] Log[1-x], Log[1-x]^2, Li2[1], Log[x], Log[1-x]}, Simplify[# /. basis]&];

      $result
    ];

  End[];

EndPackage[];
