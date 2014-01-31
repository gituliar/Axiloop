BeginPackage["Axiloop`GammaTrace`", {
  "Axiloop`Tracer`"}];

  G::usage =
  "G[<vector or index>, Line -> f1] -- a gamma matrix.

  Usage:
      G[{mu}]     a gamma matrix with vector index `mu`;
      G[p]        a gamma matrix convoluted with a vector,
                  the same as `G[{mu}] p.{mu}`;"

  GammaTrace::usage =
  "GammaTrace[expr, NumberOfDimensions -> 4 + 2 eps] calculates trace
  of the gamma matrices product in arbitrary number of dimensions. Be
  sure to use non-commutative product `**` operation instead of commonly
  used commutative product `*`.

  Example:

      In[1] := GammaTrace[G[{mu}]**G[{mu}]]
      Out[1] = 4 (4 + 2 eps)

      In[2] := GammaTrace[G[{mu}]**G[{mu}], NumberOfDimensions -> ndim]
      Out[2] = 4 ndim"

  f1::usage = "";

  Begin["`Private`"]

    $fermionLines = {};   (* A list of fermion lines used by user. `GammaTrace`  *)
                          (* calculates trace over each line from that list.     *)
                          (* By default `f1` line is used.                       *)

    Options[G] = {Line -> f1};
    G[x_, OptionsPattern[]] := (
      $fermionLines = Union[$fermionLines, {OptionValue[Line]}];
      GTrace[OptionValue[Line], x]
    );


    Options[GammaTrace] = {NumberOfDimensions -> 4 + 2 eps};
    GammaTrace[expr_, OptionsPattern[]] := Module[
      {$ndim = OptionValue[NumberOfDimensions], $result},

      Spur[f0];
      $result = Block[
        {Global`d = $ndim},

        Expand[expr
          /. ((#->f0)& /@ $fermionLines)
          /. NonCommutativeMultiply -> Times
        ]
      ];
      NoSpur[f0];
      $result
    ];

  End[];

EndPackage[];
