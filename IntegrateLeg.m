(*
;; Copyright (c) 2012-2014 Oleksandr Gituliar <oleksandr@gituliar.org>.
;;
;; This file is part of Axiloop package.
*)

BeginPackage["Axiloop`IntegrateLeg`", {
  "Axiloop`Core`",
  "Axiloop`IntegrateLoop`"
  }];

  ExpandPhaseSpaceLeg;

  IntegrateLeg;

  Q2;

  Qr;

  Begin["`Private`"]

    ExpandPhaseSpaceLeg[expr_] := Block[{Qr},
      Qr = (4 Pi)^(-2+eps)/Gamma[1-eps];
      expr
    ];

    IntegrateLeg[Wr_, ndim_] := Block[
      {eps, $result},
    
      eps = Simplify[ndim/2 - 2];
    
      $result = - Q2^eps / eps Qr (1 + eps Log[1-x]) (Expand[Wr k.k] /. {Qv[k] -> Qv Q2^eps / 2});
    
      $result
    ];

  End[];

EndPackage[];
