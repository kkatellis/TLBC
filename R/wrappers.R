## wrapper functions - these are the main functions that you will use to train a model, apply a model, and do cross-validation

trainModel = function(annotations, accelerometers=NULL, GPS=NULL, winSize=60, 
                      modelName, names=NULL, strat=TRUE,
                      ntree=500, mtry=NULL, replace=TRUE, 
                      nsample=10000, nodesize=1, sampsize=10000) {
  # function to train a model - either from raw data or from pre-computed features
  # INPUTS:
  # annotations: path to directory containing annotation files
  # accelerometers: path to directory containing raw accelerometer files
  # GPS: path to directory containing GPS files
  # winSize: window size, in seconds (default is 60)
  # modelName: path to where you want to save the model (e.g. "~/myModel.RData")
  # names: (optional) only use these participants to train the model
  # strat: Boolean - use stratified sampling in the random forest - choose equal amounts of each activity type when you're training (default is TRUE)
  # ntree: number of trees in the random forest (default is 500)
  # mtry: number of variables randomly sampled as candidates at each split in a tree (default is square root of the number of features)
  # replace: Should sampling of cases be done with or without replacement? (default is TRUE)
  # nsample: number of data samples to choose BEFORE you train the random forest
  # sampsize: random forest sampling parameter: size of sample to draw
  # nodesize: minimum size of terminal nodes (default=1)
  
  # convert annotations from bout-format to windows
  labelDir = annotationsToLabels(annotations, winSize, names)

  # extract features from sensor data
  featDirs = sensorsToFeatures(accelerometers, GPS, winSize, names)
  if (length(featDirs) == 0) { stop("there was a problem extracting features") }
    
  # train the model
  cat("\ntraining model from", length(featDirs), "devices\n")
  # train the model from features
  trainFromFeatures(labelDir, featDirs, winSize=winSize, modelName=modelName, 
              names=names, strat=strat, ntree=ntree, mtry=mtry, replace=replace,
              nsample=nsample, nodesize=nodesize, sampsize=sampsize)
}
classify = function(accelerometers=NULL, GPS=NULL, modelName, saveDir, names=NULL) {
  # function to classify data - either from raw data or from pre-computed features
  # INPUTS
  # accelerometers: path to a directory containing raw acelerometer files (if NULL, you should give it GPS)
  # GPS: path to a directory containing GPS files (if NULL, you should give it accelerometers)
  # modelName: path to a pre-trained model (.Rdata format)
  # saveDir: path to a directory where you want the output saved
  # names: (optional) if provided, only process these identifiers
  
  # look up the window size from the model
  winSize = loadModel(modelName, "winSize")
  
  # compute features from raw data - return path to where features are saved
  featDirs = sensorsToFeatures(accelerometers, GPS, winSize, names)
  # check that feature extraction step worked
  if (length(featDirs) == 0) { stop("No data directories found") }
  
  cat("\n")
  # do classification
  testAllDir(featDirs, modelName, saveDir, names)
}
looXval = function(annotations, accelerometers=NULL, GPS=NULL, winSize=60, 
                   saveDir, names=NULL, strat=TRUE) {
  # annotations
  labelDir = annotationsToLabels(annotations, winSize, names)
  # features
  featDirs = sensorsToFeatures(accelerometers, GPS, winSize, names)
  if (length(featDirs) == 0) { stop("no data directories found") }
    
  #train
  cat("cross-validating model from", length(featDirs), "devices\n")
  looXvalFromFeats(labelDir, featDirs, saveDir, names, strat)
}
sensorsToFeatures = function(accelerometers=NULL, GPS=NULL, winSize, names=NULL) {
  # extract features (if they don't already exist) and return feature directories from raw sensor directories
  # INPUTS:
  # accelerometers: path to directory containing raw accelerometer files
  # GPS: path to directory containing GPS files
  # winSize: window size, in seconds (default is 60)
  # names: (optional) extract features from these participants
  
  # intialization
  featDirs = character(0)
  
  # do GPS features
  if (!is.null(GPS)) { # check if there are GPS features to compute
    if (!file.exists(GPS)) {
      # the GPS path doesn't exist
      stop("GPS file/directory not found")
    }
    if (file.info(GPS)$isdir) {
      # the GPS path (i.e., GPS) is a directory
      if (isFeatureDirectory(GPS)) {
        # checks if the path is already to the feature directory
        GPSFeatDir = GPS
      } else {
        # set up the feature directory name: GPS_Features_(winSize)
        GPSFeatDir = paste(GPS, "Features", as.character(winSize), sep="_")
        # extract GPS features from a directory
        extractFeatsPALMSDir(GPS, GPSFeatDir, winSize, names)
      }
    } else {
      # the GPS path (i.e., GPS) is a file
      # set up the feature directory name: GPS_Features_(winSize)
      GPSFeatDir = paste(file_path_sans_ext(GPS), "Features", as.character(winSize), 
                         sep="_")
      # extract GPS features from a single file
      extractFeatsPALMSOneFile(GPS, GPSFeatDir, winSize)
    }
    # add the GPS feature directory to the list of feature directories
    featDirs = c(featDirs, GPSFeatDir)
  }
  
  # do accelerometers features
  if (!is.null(accelerometers)) { # check if there are GPS features to compute
    for (acc in accelerometers) { # loop through accelerometers (e.g., wrist, hip)
      if (!file.exists(acc)) {
        # the accelerometer path doesn't exist
        stop("accelerometer directory not found")
      }
      if (isFeatureDirectory(acc)) {
        # checks if the path is already to the feature directory
        accFeatDir = acc
      } else {
        # set up the feature directory name: acc_Features_(winSize)
        accFeatDir = paste(acc, "Features", as.character(winSize), sep="_")
        # extract accelerometer features from a directory
        extractAccelerometerFeatures(acc, accFeatDir, winSize, names)
      }
      # add the accelerometer feature directory to the list of feature directories
      featDirs = c(featDirs, accFeatDir)
    }
  }
  # return the list of feature directories
  return(featDirs)
}
isFeatureDirectory = function(dir) {
  # check if the directory is a feature directory
  file = list.files(dir, full.names=TRUE)[1]
  if (file.info(file)$isdir) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}
looXvalFromFeats = function(labelDir, featDirs, saveDir, names=NULL, strat=TRUE) {
  saveDir1 = paste(saveDir, "Temp", sep="")
  if (is.null(names)) {
    names = list.files(labelDir)
  }
  for (i in 1:length(names)) {
    cat("test subject:", names[i], "\n")
    testNames = names[i]
    trainNames = names[-i]

    # do two-level classification
    modelName1 = "temp.rf"
    modelName2 = "temp.hmm"
    
    # first train RF
    rf = trainRF(labelDir, featDirs, trainNames, strat=strat)
    testRF(featDirs, rf, saveDir1, testNames)
    # calculate performance
    cat(testNames, "\n")
    calcPerformance(labelDir, saveDir1, testNames)
    
    # then apply HMM smoothing to RF outputs
    hmm = trainHMM(labelDir, rf, trainNames)
    testHMM(saveDir1, hmm, saveDir, testNames)
    # calculate performance
    cat(testNames,"\n")
    calcPerformance(labelDir, saveDir, testNames)
    file.remove(modelName1)
    file.remove(modelName2)
    cat("----------------------------------\n")
  }
  cat("Overall RF\n")
  calcPerformance(labelDir, saveDir1, names)
  cat("Overall HMM\n")
  calcPerformance(labelDir, saveDir, names)
  cat("----------------------------------\n")
}
trainFromFeatures = function(labelDir, featDirs, winSize, modelName, names=NULL, 
                       strat=TRUE, ntree=500, mtry=NULL, sampsize=10000,
                       replace=TRUE, nsample=10000, nodesize=1) {
  # function to train a model from pre-computed features
  # INPUTS:
  # labelDir: path to directory containing label files (in window format)
  # featDirs: path to list of feature directories
  # winSize: window size, in seconds (default is 60)
  # modelName: path to where you want to save the model (e.g. "~/myModel.RData")
  # names: (optional) only use these participants to train the model
  # strat: Boolean - use stratified sampling in the random forest - choose equal amounts of each activity type when you're training (default is TRUE)
  # ntree: number of trees in the random forest (default is 500)
  # mtry: number of variables randomly sampled as candidates at each split in a tree (default is square root of the number of features)
  # replace: Should sampling of cases be done with or without replacement? (default is TRUE)
  # nsample: number of data samples to choose BEFORE you train the random forest
  # sampsize: random forest sampling parameter: size of sample to draw
  # nodesize: minimum size of terminal nodes (default=1)
  
  if (is.null(names)) {
    # if no participant names are provided, use all the names in the label directory
    names = list.files(labelDir)
  }
  # train the random forest
  rf = trainRF(labelDir, featDirs, names=names, strat=strat, ntree=ntree, 
               mtry=mtry, replace=replace, nsample=nsample, nodesize=nodesize,
               sampsize=sampsize)
  # train the HMM
  hmm = trainHMM(labelDir, rf, names)
  # create the directory in which to save the model (if it doesn't exist)
  if (!file.exists(dirname(modelName))){
    dir.create(dirname(modelName), recursive=TRUE)
  }
  # save the trained model
  save(rf, hmm, winSize, file=modelName)
  cat("model saved to", modelName, "\n")
}
testAllDir = function(featDirs, modelName, saveDir, names=NULL) {
  if (is.null(names)) {
    names = list.files(featDirs[1])
  }
  saveDir1 = file.path(saveDir, "Temp")
  for (i in 1:length(names)) {
    testRF(featDirs, modelName, saveDir1, names[i])
    testHMM(saveDir1, modelName, saveDir, names[i])
  }
  cat("predictions saved to", saveDir, "\n")
}