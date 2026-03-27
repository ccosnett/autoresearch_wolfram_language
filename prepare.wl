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

generateData[] := Module[{xAll, yAll, xTrain, xVal, yTrain, yVal},
  BlockRandom[
    SeedRandom[seed];
    xAll = RandomVariate[NormalDistribution[], {nTrain + nVal, nFeatures}];
    yAll = xAll . trueWeights + trueBias + RandomVariate[NormalDistribution[], nTrain + nVal] * noiseStd;
  ];
  xTrain = xAll[[;; nTrain]];
  xVal = xAll[[nTrain + 1 ;;]];
  yTrain = yAll[[;; nTrain]];
  yVal = yAll[[nTrain + 1 ;;]];
  <|"xTrain" -> xTrain, "yTrain" -> yTrain, "xVal" -> xVal, "yVal" -> yVal|>
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

If[Length[$ScriptCommandLine] > 0 && StringContainsQ[First[$ScriptCommandLine], "prepare"],
  Module[{data, xTrain, yTrain, xVal, yVal, meanPred, baselineMSE},
    data = getData[];
    xTrain = data["xTrain"];
    yTrain = data["yTrain"];
    xVal = data["xVal"];
    yVal = data["yVal"];

    Print["Linear regression toy problem"];
    Print["  Features:    ", nFeatures];
    Print["  Train size:  ", nTrain];
    Print["  Val size:    ", nVal];
    Print["  Noise std:   ", noiseStd];
    Print["  Time budget: ", timeBudget, "s"];
    Print[];
    Print["  xTrain dims: ", Dimensions[xTrain]];
    Print["  yTrain range: [", ToString[NumberForm[Min[yTrain], 6]], ", ", ToString[NumberForm[Max[yTrain], 6]], "]"];
    Print["  xVal dims:   ", Dimensions[xVal]];
    Print["  yVal range:  [", ToString[NumberForm[Min[yVal], 6]], ", ", ToString[NumberForm[Max[yVal], 6]], "]"];
    Print[];

    (* Baseline: predict mean of training targets *)
    meanPred = ConstantArray[Mean[yTrain], nVal];
    baselineMSE = evaluateMSE[meanPred];
    Print["  Baseline MSE (predict mean): ", ToString[NumberForm[baselineMSE, 6]]];
    Print[];
    Print["Ready to train. Run: wolframscript -file train.wl"];
  ];
];
