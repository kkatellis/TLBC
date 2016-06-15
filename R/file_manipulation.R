## helper functions to load and save data & other extras

loadModel = function(modelName, which) {
  # function to load a model (and important model information)
  # INPUTS
  # modelName: path to a saved model OR the model itself (list structure in R)
  # which: string that tells what kind of information to return
  
  hmm = NULL
  rf = NULL
  winSize = NULL
  
  # check if it's a path or the model itself
  if (typeof(modelName) == "list") {
    # it's the model itself - return it
    return(modelName)
  }
  # otherwise it's a path
  if (file.exists(modelName)) {
    # if the model is a path to file, load it
    load(modelName)
  } else {
    # file doesn't exist - error
    stop("Couldn't find model file")
  }
  if (which=="winSize") {
    return(winSize)
  } else if (which=="rf") {
    return(rf)
  } else if (which=="hmm") {
    return(hmm)
  } else {
    stop("Unknown model return type")
  }
}
loadData = function(labelDir, featDirs, names=NULL) {
  # function to load matched label and feature data
  # INPUTS:
  # labelDir: directory with label files
  # featDirs: list of feature directories
  # names (optional): only load data from these participants
  
  sprintf("loading data from %s\n", featDirs)
  if (is.null(names)) {
    names = list.files(labelDir)
  }
  if (length(names) == 0) {
    stop("no files found in %s", labelDir)
  }
  for (featDir in featDirs){
    if (length(list.files(featDir)) == 0){
      stop("no files found in %s", featDir)
    }
  }
  all_data = data.frame()
  all_labels = data.frame()
  for (name in names) {
    cat(name, "\n")
    # for each participant
    days = list.files(file.path(labelDir, name))
    for (day in days) {
      # for each day
      checkFlag = TRUE
      # check that there each feature file exists for this day
      cat(" ", file_path_sans_ext(day), "\n")
      for (featDir in featDirs) {
        # for each feature type
        featFile = file.path(featDir, name, file_path_sans_ext(day))
        if (!file.exists(featFile)) {
          #skip this day
          checkFlag = FALSE
          break
        }
      }
      if (checkFlag) {
        # read the label file
        labels = read.csv(file.path(labelDir, name, day), stringsAsFactors=FALSE)
        # set up feature data frame
        data = data.frame(timestamp=labels$timestamp, stringsAsFactors=TRUE)
        for (featDir in featDirs) {
          # for each feature type
          featFile = file.path(featDir, name, file_path_sans_ext(day))
          d = read.csv(featFile, header=TRUE, stringsAsFactors=FALSE)
          # merge feature files by timestamp
          data = merge(d, data, by = "timestamp", all=FALSE)
        }
        # merging labels with valid feature timestamps
        l2 = data.frame(timestamp=data$timestamp, stringsAsFactors=FALSE)
        labels = merge(labels, l2, by="timestamp", all=FALSE)
        if (nrow(data) == nrow(labels)) {
          all_data = rbind(all_data, data)
          all_labels = rbind(all_labels, labels)
        } else {
          stop("error loading:", name, day, "(features and labels don't match)\n")
        }
      }
    }
  }
  if (nrow(all_labels) == 0) {
    stop("no matching label & feature data for %s and %s", labelDir, paste(featDirs, collapse=", "))
  }
  return(list(all_labels, all_data))
}
loadFeatures = function(featDirs, names=NULL) {
  # function to load features from directories
  # INPUTS:
  # featDirs: list of feature directories
  # names: (optional) only load data from these participants
  if (is.null(names)) {
    names = list.files(featDirs[1])
  }
  for (featDir in featDirs){
    if (length(list.files(featDir)) == 0){
      stop("no files found in %s", featDir)
    }
  }
  all_feats = data.frame()
  for (i in 1:length(names)) {
    days = list.files(file.path(featDirs[1], names[i]))
    if (length(days) == 0) {
      warning("no data found for %s", file.path(featDirs[1], names[i]))
      next
    }
    for (k in 1:length(days)) {
      # for each day
      checkFlag = TRUE
      if (length(featDirs) > 1) {
        for (j in 2:length(featDirs)) {
          # for each feature type
          # check that each type of feature exists for that day
          featFile = file.path(featDirs[j], names[i], days[k])
          if (!file.exists(featFile)) {
            #skip this day
            checkFlag = FALSE
            break
          }
        }
      }
      if (checkFlag) {
        # load first feature type
        featFile = file.path(featDirs[1], names[i], days[k])
        feats = read.csv(featFile, header=TRUE, stringsAsFactors=FALSE)
        # if there are more feature types load those next
        if (length(featDirs) > 1) {
          for (j in 2:length(featDirs)) {
            # for each feature type
            featFile = file.path(featDirs[j], names[i], days[k])
            f = read.csv(featFile, header=TRUE, stringsAsFactors=FALSE)
            # merge with other features by timestamp
            feats = merge(f, feats, by = "timestamp", all=FALSE)
          }
        }
        all_feats = rbind(all_feats, feats)
      }
    }
  }
  if (nrow(all_feats) == 0) {
    stop("no matching feature data for %s", paste(featDirs, collapse=", "))
  }
  return(all_feats)
}
loadPredictionsAndLabels = function(labelDir, predDir, names=NULL) {
  # function to load matching prediction and label data
  # INPUT: 
  # labelDir: directory with ground truth labels
  # predDir: directory with ML predictions
  # names (optional): list of participants to use
  
  if (is.null(names)) {
    names = list.files(predDir)
  }
  if (length(names) == 0) {
    stop("no files found in %s", predDir)
  }
  all_predictions = data.frame()
  for (i in 1:length(names)) {
    # for each participant
    name = file_path_sans_ext(names[i])
    predFile = paste0(file.path(predDir, name), ".csv")
    labelFile = file.path(labelDir, name)
    
    if (file.exists(predFile) & (file.exists(labelFile) | file.exists(paste0(labelFile, ".csv")))){
      # if both prediction file and label files exist
      predictions = read.csv(predFile, stringsAsFactors=FALSE)
      labels = loadLabels(labelDir, name)
      # merge predictions and labels by timestamp
      predictions = merge(labels, predictions, by="timestamp", all=FALSE)
      all_predictions = rbind(all_predictions, predictions)
    }
  }
  if (nrow(all_predictions) == 0) {
    stop("no matching label and prediction data for %s and %s", labelDir, predDir)
  }
  return(all_predictions)
}
loadLabels = function(labelDir, names=NULL) {
  # function to load label files
  # INPUTS: 
  # labelDir: directory where label files are saved
  # names (optional): list of participants to load
  
  if (is.null(names)) {
    names = list.files(labelDir)
  }
  if (length(names) == 0) {
    stop("no files found in %s", labelDir)
  }
  all_labels = data.frame()
  for (i in 1:length(names)) {
    # for each participant
    if (file.exists(file.path(labelDir, names[i]))){
      labelFiles = list.files(file.path(labelDir, names[i]))
      for (k in 1:length(labelFiles)) {
        # for each day
        labels = read.csv(file.path(labelDir, names[i], labelFiles[k]), 
                          stringsAsFactors=FALSE)
        labels$id = names[i]
        all_labels = rbind(all_labels, labels)
        }
    } else {
      labels = read.csv(file.path(labelDir, names[i]), stringsAsFactors=FALSE)
      labels$id = names[i]
      all_labels = rbind(all_labels, labels)
    }
  }
  if (nrow(all_labels) == 0) {
    stop("no label data found %s", labelDir)
  }
  return(all_labels)
}
loadPredictions = function(predDir, names=NULL) {
  # function to load prediction data
  # INPUTS:
  # predDir: directory where predictions are saved
  # names (optional): list of participants to load
  
  if (is.null(names)) {
    names = list.files(predDir)
  }
  if (length(names) == 0) {
    stop("no files found in %s", predDir)
  }
  all_predictions = data.frame()
  for (i in 1:length(names)) {
    # for each participant
    name = file_path_sans_ext(names[i])
    predFile = file.path(predDir, paste0(name, ".csv"))
    if (file.exists(predFile)) {
      predictions = read.csv(predFile, stringsAsFactors=FALSE)
      predictions$id = name
      all_predictions = rbind(all_predictions, predictions)
    } else {
      warning("prediction file not found for %s", name)
    }
  }
  if (nrow(all_predictions)==0){
    stop("no prediction data found for %s", predDir)
  }
  return(all_predictions)
}
writePredictions = function(values, timestamps, saveFile) {
  # function to write predictions to file
  # INPUTS
  # values: list of predictions
  # timestamps: list of timestamps for predictions
  # saveFile: path to file to save predictions
  if (length(values) != length(timestamps)) {
    stop("error writing predictions: length of timestamps don't match (%s)", saveFile)
  }
  if (file.exists(saveFile)) {
    warning("overwriting file %s", saveFile)
    file.remove(saveFile)
  }
  # create the parent directory if it doesn't exist yet
  if (!file.exists(dirname(saveFile))) {
    dir.create(dirname(saveFile), recursive=TRUE)
  }
  # write the header to the file
  cat("timestamp,prediction\n", file=saveFile, sep="", append=TRUE)
  for (i in 1:length(values)) {
    # write all the predictions to the file
    str = paste(timestamps[i], as.character(values[i]), sep = ",")
    cat(str, "\n", file=saveFile, sep = "", append=TRUE)
  }
}
clearFiles = function(dir) {
  # function to delete files in a directory
  if (!file.exists(dir)) {
    stop("directory doesn't exist")
  }
  if (!file.info(dir)$isdir) {
    stop("isn't a directory")
  }
  files = list.files(dir)
  for (file in files) {
    file.remove(file.path(dir, file))
  }
}
getDateFmt = function(inputString) {
  # function to guess the date format
  dF1 = "%Y-%m-%d %H:%M:%S"
  dF2 = "%m/%d/%Y %H:%M:%S"
  if (!is.na(strptime(str_trim(inputString), dF1))) {
    return(dF1)
  }
  if (!is.na(strptime(str_trim(inputString), dF2))) {
    return(dF2)
  }
  stop("date format not matched")
  return(NULL)
}
alignStart = function(winSize, start) {
  # function to round start time to the next window start
  d0 = trunc(start, "days")
  s = as.numeric(difftime(start, d0, units="secs"))
  w = ceiling(s / winSize)
  newStart = as.POSIXlt(d0 + w * winSize)
  return(newStart)
}