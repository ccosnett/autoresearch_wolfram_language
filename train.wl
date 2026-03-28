(* ::Package:: *)
(*
  Linear regression training script.
  The agent modifies this file to improve val_mse.

  Usage:
      wolframscript -file train.wl
      (* or paste into a notebook in the same directory and evaluate *)
*)

resolveBaseDirectory[] := Which[
  StringQ[$InputFileName] && $InputFileName =!= "",
  DirectoryName[ExpandFileName[$InputFileName]],
  $FrontEnd =!= Null,
  Quiet[Check[NotebookDirectory[], Directory[]]],
  True,
  Directory[]
];

Get[FileNameJoin[{resolveBaseDirectory[], "prepare.wl"}]];

(* --------------------------------------------------------------------------- *)
(* Hyperparameters                                                              *)
(* --------------------------------------------------------------------------- *)

learningRate = 0.01;
numIterations = 1000;

(* --------------------------------------------------------------------------- *)
(* Helpers                                                                     *)
(* --------------------------------------------------------------------------- *)

formatNumber[x_, decimals_] := ToString @ NumberForm[
  N[x],
  {Infinity, decimals},
  NumberPadding -> {"", "0"}
];

formatVector[vec_, decimals_: 6] :=
  "{" <> StringRiffle[formatNumber[#, decimals] & /@ N[vec], ", "] <> "}";

(* --------------------------------------------------------------------------- *)
(* Training and evaluation                                                     *)
(* --------------------------------------------------------------------------- *)

runTraining[] := Module[
  {
    data, xTrain, yTrain, xVal, nSamples, nFeaturesLocal,
    weights, bias, tStart, completedIterations = 0,
    yPred, error, gradWeights, gradBias, trainMSE,
    trainingTime, valPredictions, valMSE
  },

  data = getData[];
  xTrain = data["xTrain"];
  yTrain = data["yTrain"];
  xVal = data["xVal"];

  {nSamples, nFeaturesLocal} = Dimensions[xTrain];

  weights = ConstantArray[0.0, nFeaturesLocal];
  bias = 0.0;

  tStart = AbsoluteTime[];

  Do[
    If[AbsoluteTime[] - tStart > timeBudget,
      Break[];
    ];

    yPred = xTrain . weights + bias;
    error = yPred - yTrain;

    gradWeights = (2.0 / nSamples) * (Transpose[xTrain] . error);
    gradBias = (2.0 / nSamples) * Total[error];

    weights -= learningRate * gradWeights;
    bias -= learningRate * gradBias;
    completedIterations = i;

    If[Mod[i, 100] == 0,
      trainMSE = Mean[error^2];
      Print[
        "step ",
        IntegerString[i, 10, 5],
        " | train_mse: ",
        formatNumber[trainMSE, 6]
      ];
    ];
  ,
    {i, numIterations}
  ];

  trainingTime = AbsoluteTime[] - tStart;
  valPredictions = xVal . weights + bias;
  valMSE = evaluateMSE[valPredictions];

  Print["---"];
  Print["val_mse:          ", formatNumber[valMSE, 6]];
  Print["training_seconds: ", formatNumber[trainingTime, 1]];
  Print["num_iterations:   ", completedIterations];
  Print["weights:          ", formatVector[weights]];
  Print["bias:             ", formatNumber[bias, 6]];
];

runTrainingQ[] := StringQ[$InputFileName] && $InputFileName =!= "" || $FrontEnd =!= Null;

If[runTrainingQ[], runTraining[]];
