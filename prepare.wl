(* ::Package:: *)
(*
  Data preparation and evaluation for the linear regression toy problem.

  Generates synthetic data: y = W . x + b + noise
  Fixed random seed ensures reproducibility across experiments.

  DO NOT MODIFY THIS FILE - it is the fixed evaluation harness.

  Usage:
      wolframscript -file prepare.wl        (* generates data and prints summary *)
      (* also loaded by train.wl via Get["prepare.wl"] *)
*)

(* --------------------------------------------------------------------------- *)
(* Constants (fixed, do not modify)                                            *)
(* --------------------------------------------------------------------------- *)

seed = 42;
nFeatures = 5;
nTrain = 500;
nVal = 200;
timeBudget = 10; (* seconds - generous for this problem *)

(* Ground truth coefficients: y = W . x + b + noise *)
trueWeights = {3.0, 7.0, -2.0, 0.5, 4.0};
trueBias = -2.0;
noiseStd = 0.5;

(* --------------------------------------------------------------------------- *)
(* Data generation (deterministic)                                             *)
(* --------------------------------------------------------------------------- *)

generateData[] := Module[{xAll, yAll},
  BlockRandom[
    SeedRandom[seed];
    xAll = RandomVariate[NormalDistribution[], {nTrain + nVal, nFeatures}];
    yAll = xAll . trueWeights + trueBias + RandomVariate[NormalDistribution[], nTrain + nVal] * noiseStd;
  ];
  <|
    "xTrain" -> xAll[[;; nTrain]],
    "yTrain" -> yAll[[;; nTrain]],
    "xVal" -> xAll[[nTrain + 1 ;;]],
    "yVal" -> yAll[[nTrain + 1 ;;]]
  |>
];

(* Cache so repeated loads don't regenerate *)
getData[] := getData[] = generateData[];

(* --------------------------------------------------------------------------- *)
(* Evaluation (DO NOT CHANGE - this is the fixed metric)                       *)
(* --------------------------------------------------------------------------- *)

evaluateMSE[predictions_] := evaluateMSE[predictions, getData[]["yVal"]];

evaluateMSE[predictions_, yTrue_] := Module[{preds, targets},
  preds = Flatten[predictions];
  targets = Flatten[yTrue];
  If[Length[preds] =!= Length[targets],
    Print["Shape mismatch: predictions length ", Length[preds],
          " vs targets length ", Length[targets]];
    Abort[];
  ];
  N[Mean[(preds - targets)^2]]
];

(* --------------------------------------------------------------------------- *)
(* Main - run to verify data generation                                        *)
(* --------------------------------------------------------------------------- *)

(* Equivalent to Python's if __name__ == "__main__": — only runs when executed directly,
   not when loaded via Get["prepare.wl"] from another script. *)
If[
  Length[$ScriptCommandLine] > 0 && StringContainsQ[First[$ScriptCommandLine], "prepare"],
  Module[{data, yTrain, baselineMSE, fmt},
    data = getData[];
    yTrain = data["yTrain"];
    baselineMSE = evaluateMSE[ConstantArray[Mean[yTrain], nVal]];
    fmt[x_] := ToString[NumberForm[x, 6]];

    Print["Linear regression toy problem"];
    Print["  Features:    ", nFeatures];
    Print["  Train size:  ", nTrain];
    Print["  Val size:    ", nVal];
    Print["  Noise std:   ", noiseStd];
    Print["  Time budget: ", timeBudget, "s"];
    Print[];
    Print["  xTrain dims: ", Dimensions[data["xTrain"]]];
    Print["  yTrain range: [", fmt[Min[yTrain]], ", ", fmt[Max[yTrain]], "]"];
    Print["  xVal dims:   ", Dimensions[data["xVal"]]];
    Print["  yVal range:  [", fmt[Min[data["yVal"]]], ", ", fmt[Max[data["yVal"]]], "]"];
    Print[];

    (* Baseline: predict mean of training targets *)
    Print["  Baseline MSE (predict mean): ", fmt[baselineMSE]];
    Print[];
    Print["Ready to train. Run: wolframscript -file train.wl"];
  ];
];
