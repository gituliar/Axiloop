BeginPackage["Axiloop`FeynmanRules`", {
  "Axiloop`Core`",
  "Axiloop`GammaTrace`"
  }];

  FP::usage =
  "FP[momentum, Line -> f1] -- a fermion propagator in the light-cone gauge."
  FPc::usage =
  "The complex conjugated Fermion Propagator."

  FPx::usage =
  "FPx[momentum, Line -> f1] -- a crossed (final-state, on-shell) fermion
  propagator in the light-cone gauge."

  FV::usage =
  "FV[index, Line -> f1] -- a fermion vertex in the light-cone gauge."
  FVc::usage =
  "The complex conjugated Fermion Vertex."

  GP::usage =
  "GP[mu, nu, p] -- a gluon propagator in the light-cone gauge."
  GPc::usage =
  "The complex conjugated Gluon Propagator."

  GPx::usage =
  "GPx[mu, nu, p] -- a crossed (final-state, on-shell) gluon propagator in
  the light-cone gauge."

  GV::usage =
  "GV[i1,p1, i2,p2, i3,p3] -- a gluon vertex in the light-cone gauge."
  GVc::usage =
  "The complex conjugated Gluon Vertex."

  PFi::usage = "";
  PFo::usage = "";

  PGi::usage = "";
  PGo::usage = "";

  Ca::usage = "Color factor."
  Cf::usage = "Color factor."
  Nf::usage = "Color factor; number of fermion flawours."
  Tf::usage = "Color factor."

  as::usage = "g^2 / (4 Pi)";

  aspi::usage = "Alpha_s / (2 Pi)";

  g::usage =
  "Quark-gluon coupling constant."

  n::usage =
  "Light-cone gauge vector; n.n = 0."

  Begin["`Private`"]

    as = g^2 / (4 Pi);
    aspi = as / (2 Pi);

    Options[FP] = {Line -> f1};
    FP[p_, OptionsPattern[]] := I/p.p G[p, Line -> OptionValue[Line]];
    FPc[args__] := - FP[args];

    Options[FPx] = {Line -> f1};
    FPx[p_, OptionsPattern[]] := G[p, Line -> OptionValue[Line]];

    Options[FV] = {Line -> f1};
    FV[mu_, OptionsPattern[]] := - I g G[{mu}, Line -> OptionValue[Line]];
    FVc[argv__] := - FV[argv];

    GP[mu_, nu_, p_] := I/p.p (- {mu}.{nu} + (p.{mu} n.{nu} + n.{mu} p.{nu}) / p.n);
    GPc[argv__] := - GP[argv];

    GPx[mu_, nu_, p_] := (- {mu}.{nu} + (p.{mu} n.{nu} + n.{mu} p.{nu}) / p.n);

    GV[i1_,p1_, i2_,p2_, i3_,p3_] := I g ({i1}.{i2} (p1.{i3}-p2.{i3}) + {i2}.{i3} (p2.{i1}-p3.{i1}) + {i3}.{i1} (p3.{i2}-p1.{i2}));
(*
    GV[i1_,p1_, i2_,p2_, i3_,p3_, i4_,p4_] := -I g^2 ( ({i1}.{i3} {i2}.{i4} - {i1}.{i4} {i2}.{i3}) + ({i1}.{i2} {i3}.{i4} - {i1}.{i4} {i2}.{i3}) + ({i1}.{i3} {i2}.{i4} - {i1}.{i2} {i3}.{i4}));
*)
    GV[i1_,p1_, i2_,p2_, i3_,p3_, i4_,p4_] := -I g^2 ( 2 {i1}.{i2} {i3}.{i4} - 2 {i1}.{i3} {i2}.{i4} );
    GVc[argv__] := - GV[argv];


    Options[PFi] = {Line -> f1};
    PFi[p_, OptionsPattern[]] := G[p, Line -> OptionValue[Line]];
    Options[PFo] = {Line -> f1};
    PFo[p_, OptionsPattern[]] := G[n, Line -> OptionValue[Line]]/(4 p.n);

    PGi[mu_,nu_,p_] := 1/(2 (1+eps)) (-{mu}.{nu} + (p.{mu} n.{nu} + n.{mu} p.{nu}) / p.n);
    PGo[mu_,nu_] := - {mu}.{nu};

  End[];

EndPackage[];
