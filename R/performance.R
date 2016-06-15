## functions to calculate performance

calcPerformance = function(annotations, predictions, winSize, names=NULL, 
                           combineStanding=FALSE) {
  # function to calculate performance metrics from predictions and ground truth bout-level annotations
  # INPUTS
  # annotations: directory of annotation files
  # predictions: directory of prediction files
  # winSize: window size in seconds
  # names (optional): list of participant identifiers to use
  # combineStanding: (bool) if true, combine "standing still" and "standing moving" into "standing"
  cat("\n")
  # get the directory with window-level labels
  labelDir = annotationsToLabels(annotations, winSize, names)
  calcPerformanceFromLabels(labelDir, predictions, names, combineStanding)
}
calcPerformanceFromLabels = function(labelDir, predDir, names=NULL, 
                                     combineStanding=FALSE) {
  # function to calculate performance metrics from predictions and ground truth labels
  # INPUTS
  # labelDir: directory of annotation files
  # predDir: directory of prediction files
  # names (optional): list of participant identifiers to use
  # combineStanding: (bool) if true, combine "standing still" and "standing moving" into "standing"
  
  # load matching predictions and labels
  data = loadPredictionsAndLabels(labelDir, predDir, names)
  if (nrow(data) > 0) {
    # get the predictions
    pr = data[data$behavior != "NULL", c("prediction")]
    # get the ground truth
    gt = data[data$behavior != "NULL", c("behavior")]
    
    if (combineStanding) {
      # combine standing
      pr[grepl("Standing", pr)] = "Standing"
      gt[grepl("Standing", gt)] = "Standing"
    }
    
    # get unique labels
    l = unique(c(pr, gt))
    
    # turn them into factors
    pr = factor(pr, levels=l)
    gt = factor(gt, levels=l)
    
    # compute the confusion matrix (from caret package)
    m = confusionMatrix(pr, gt)
    print(m)
    cat("\n")
    return(m)
  }
  else {
    stop("No matching prediction and label data found")
  }
}