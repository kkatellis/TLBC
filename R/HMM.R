## functions to train and test HMMs

trainHMM = function(labelDir, rf, names, combineStanding=FALSE) {
  # function to train a Hidden Markov Model from ground truth labels and learned random forest model
  # INPUTS:
  # labelDir: path to directory with ground truth labels
  # rf: random forest object trained by random forest function
  # names (optional): only train from a list of participants
  # combineStanding (bool): if true, combine standing still and standing moving into a single class
  
  # load ground truth labels
  labels = loadLabels(labelDir, names)
  labels = labels$behavior
  
  cat("training HMM\n")
  # get the unique states (labels)
  states = sort(unique(labels))
  # remove NULL labels
  states = states[!grepl("NULL", states)]
  # the (output) symbols in the HMM are the labels predicted by the random forest
  symbols = sort(rf$classes)
  # compute the transition probabilities
  transProbs = computeTransProbs(labels)
  # compute the emission probabilities
  emissionProbs = computeEmissionProbs(rf)
  # compute the prior probabilities
  startProbs = computePriorProbs(labels)
  # make the HMM
  hmm = initHMM(states, symbols, startProbs, transProbs, emissionProbs)
  return(hmm)
}
testHMM = function(predDir, modelName, saveDir, names) {
  # function to apply HMM to test data
  # INPUTS:
  # predDir: directory containing predictions made by random forest
  # modelName: path to model
  # saveDir: directory where to save HMM smoothed predictions
  # names (optional): apply HMM only to specified participants

  # load model
  hmm = loadModel(modelName, "hmm")
  if (length(names) == 0) {
    stop("no test data\n")
  }
  for (name in names) {
    # load predictions
    pred = loadPredictions(predDir, name)
  
    if (nrow(pred) == 0) {
      next
    }
    # apply HMM
    cat("applying HMM\n")
    
    # computing posterior probability for each minute - changed this to use viterbi algorithm instead
    # post = posterior(hmm, pred$prediction)
    # filtered = as.character(factor(max.col(t(post)), 
                            # levels=1:length(hmm$States), 
     #                              labels=hmm$States))
    
    # use the viterbi algorithm to compute the most likely sequence of states
    filtered = viterbi(hmm, pred$prediction)
    
    # save predictions
    saveFile = file.path(saveDir, paste0(name, ".csv"))
    writePredictions(filtered, pred$timestamp, saveFile)
  }
}
computeTransProbs = function(stateSeq) {
  # computes the transition probabilities between states in HMM (labels)
  # INPUT: stateSeq: sequence of labels
  
  # get the list of unque states
  states = sort(unique(stateSeq))
  # remove NULL
  states = states[!grepl("NULL", states)]
  S = length(states)
  # initialize the transition Probability matrix
  transProbs = matrix(.Machine$double.eps, nrow=S, ncol=S)
  # set the row and column names of the matrix to be the state (label) names
  rownames(transProbs) = states
  colnames(transProbs) = states
  # loop through state sequence and count transitions
  x = stateSeq[1]
  i = 2 # index
  # loop through until we reach the first non-null label
  while (x == "NULL") {
    x = stateSeq[i]
    i = i + 1
  }
  nNull = 0 # null counter
  # count number of transitions - allow for up to 4 NULLs between different labels
  for (ii in i:(length(stateSeq) - 1)) {
    if (stateSeq[ii] != "NULL") {
      if (nNull <= 4){
        transProbs[x,stateSeq[ii]] = transProbs[x, stateSeq[ii]] + 1
        x = stateSeq[ii]
        nNull = 0
      } else {
        nNull = 0
      }
    } else {
      nNull = nNull + 1
    }
  }
  # turn frequency matrix into probability matrix by dividing by row sums
  transProbs = transProbs / rowSums(transProbs)
  return(transProbs)
}
computeEmissionProbs = function(rf) {
  # function to compute emission probabilities from random forest model
  # INPUT: rf: a random forest object
  
  # get the list of unque states
  states = sort(rf$classes)
  symbols = sort(rf$classes)
  S = length(states)
  
  # initialize the emission Probability matrix
  emissionProbs = matrix(0, nrow=S, ncol=S)
  rownames(emissionProbs) = states
  colnames(emissionProbs) = states
  # loop through states
  for (k in 1:length(states)) {
    # probability that an example with ground truth k was predicted as each label
    emissionProbs[k, ] = colSums(rf$votes[rf$groundTruth == states[k], ]) / sum(rf$groundTruth == states[k])
  }
  return(emissionProbs)
}
computePriorProbs = function(stateSeq) {
  # function to compute prior probabilities of each state from sequence of labels
  # INPUT: stateSeq: sequence of labels
  
  states = sort(unique(stateSeq))
  states = states[!grepl("NULL", states)]
  
  # counts frequencies of each label
  priorProbs = tabulate(factor(stateSeq[stateSeq != "NULL"], levels=states))
  # convert frequencies to probabilities
  priorProbs = priorProbs / sum(priorProbs)
  return(priorProbs)
}