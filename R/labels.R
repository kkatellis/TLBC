## functions to deal with annotations/labels

annotationsToLabels = function(annotations, winSize, names=NULL) {
  # function to extract instance-level labels from bout-level annotations
  # INPUTS:
  # annotations: directory (or single file) where bout-level annotations are saved
  # winSize: window size in seconds
  # names (optional): list of selected participants
  if (!file.exists(annotations)) {
    stop("annotation file not found")
  }
  if (file.info(annotations)$isdir){
    if (isInstanceFormat(annotations)) {
      # already in instance format
      cat("instance-level annotations already created\n")
      return(annotations)
    }
    labelDir = paste(annotations, "Labels", as.character(winSize), sep="_")
    if (!file.exists(labelDir)) {
      cat("extracting instance-level annotations...\n")
      # extract labels from a directory
      labels = extractLabelsDir(annotations, labelDir, winSize, names)
    }
  } else {
    labelDir = paste(file_path_sans_ext(annotations), "Labels", as.character(winSize), sep="_")
    if (!file.exists(labelDir)) {
      cat("extracting instance-level annotations...\n")
      # extract lables from a single file
      labels = extractLabelsSingleFile(annotations, labelDir, winSize)
    }
  }
  return(labelDir)
}
isInstanceFormat = function(annotations) {
  # check if the annotations directory contains instance-level labels
  # instance-level labels will be directories of day-level files
  # INPUT:
  # annotations: directory with annotation (or label) files
  file = list.files(annotations)[1]
  # label directories will have day-level sub-directories
  if (file.info(file.path(annotations, file))$isdir) {
    return (TRUE)
  } else {
    return (FALSE)
  }
}
extractLabelsSingleFile = function(inputFile, outputDir, winSize) {
  # splits a bout-level annotation file by identifier and by days
  # INPUTS
  # inputFile: annotation file - column names should be identifier,StartDateTime,EndDateTime,PA1
  # outputDir: directory to save label files
  # winSize: window size (in seconds)
  
  # start reading record file
  all_bouts = read.csv(inputFile, header=TRUE, stringsAsFactors=FALSE)
  # figure out what date format the file uses
  dateFmt = getDateFmt(str_trim(all_bouts[1, ]$StartDateTime))
  
  # get the list of unique activity labels
  annotations = unique(all_bouts$behavior)
  # remove spaces from activity names
  actNames = sub(" ", "", annotations)
  # get the list of unique ids
  identifiers = unique(all_bouts$identifier)
  # loop through ids
  for (id in 1:length(identifiers)) {
    cat(identifiers[id], "\n")
    
    # get all the activity bouts for that participant
    bouts = all_bouts[all_bouts$identifier == identifiers[id], ]
    # set up the output file
    outputFile = file.path(outputDir, identifiers[id])
    r = 1
    l = 1
    label = "NULL"
    # get the start/stop times for the first bout
    boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt)
    boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt)
    # align start with an even multiple of winSize
    timestamp = alignStart(winSize, boutstart)
    
    # get the day of the month to split labels by day
    day = timestamp$mday
    out = file.path(outputFile, paste0(strftime(timestamp, "%Y-%m-%d"), ".csv"))
    cat(strftime(timestamp, "%Y-%m-%d"), '\n')
    if (!file.exists(outputFile)) {
      dir.create(outputFile, recursive=TRUE)
    }
    if (file.exists(out)) {
      file.remove(out)
    }
    # write the header
    cat("timestamp,behavior\n", file=out, append=TRUE)
    
    # loop through the annotation bouts
    while (TRUE) {
      if ((timestamp >= boutstart) & (timestamp + winSize <= boutstop)) {
        # the window is within this bout - add the label
        label = sub(" ", "", str_trim(bouts[r, c("behavior")]))
      } else if (timestamp + winSize > boutstop) {
        # move on to the next bout
        if (r == nrow(bouts)) {
          break
        }
        while (timestamp + winSize > boutstop) {
          if (r == nrow(bouts)) {
            break
          }
          r = r + 1
          boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt)
          boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt)
        }
        if (timestamp >= boutstart) {
          # the window is within this bout - add the label
          label = sub(" ", "", str_trim(bouts[r, c("behavior")]))
        }
      }
      # write the window to the label file
      cat(strftime(timestamp, "%Y-%m-%d %H:%M:%S,"), file=out, append=TRUE)
      cat(label, file=out, append=TRUE)
      cat("\n", file=out, append=TRUE)
      
      # advance to next window
      l = l + 1
      label = "NULL"    
      timestamp = as.POSIXlt(timestamp + winSize)
      # if it's a new day start a new file
      if (timestamp$mday != day) {
        day = timestamp$mday
        out = file.path(outputFile, paste0(strftime(timestamp, "%Y-%m-%d"), ".csv"))
        cat(strftime(timestamp, "%Y-%m-%d"), '\n')
        if (file.exists(out)) {
          file.remove(out)
        }
        cat("timestamp,behavior\n", file=out, append=TRUE)
      }
    }
  }
  return(actNames)
}
extractLabelsDir = function(inputDir, outputDir, winSize) {
  # splits a directory of bout-level annotation files by days
  # INPUTS
  # inputDir: directory of bout-level annotation files (should be saved by identifier name)
  # each annotation file should have the following fields: identifier,StartDateTime,EndDateTime,PA1
  # outputDir: directory to save label files
  # winSize: window size in seconds
  
  # get all files in inputDir
  files = list.files(inputDir)
  for (i in 1:length(files)) {
    cat(files[i], "\n")
    
    # start reading record file
    bouts = read.csv(file.path(inputDir, files[i]), header=TRUE, 
                     stringsAsFactors=FALSE)
    outputFile = file.path(outputDir, file_path_sans_ext(files[i]))
    # figure out the date format
    dateFmt = getDateFmt(str_trim(bouts[1, ]$StartDateTime))
    
    r = 1
    l = 1
    label = "NULL"
    boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt)
    boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt)
    timestamp = alignStart(winSize, boutstart)
    
    day = timestamp$mday
    out = file.path(outputFile, paste0(strftime(timestamp, "%Y-%m-%d"), ".csv"))
    cat(strftime(timestamp, "%Y-%m-%d"), '\n')
    if (!file.exists(outputFile)) {
      dir.create(outputFile, recursive=TRUE)
    }
    if (file.exists(out)) {
      file.remove(out)
    }
    # write header
    cat("timestamp,behavior\n", file=out, append=TRUE)
    
    # loop through bout-level annotations
    while (TRUE) {
      if ((timestamp >= boutstart) & (timestamp + winSize < boutstop)) {
        # the window is within this bout - add the label
        label = sub(" ", "", str_trim(bouts[r, c("behavior")]))
      } else if (timestamp + winSize >= boutstop) {
        # move on to the next bout
        if (r == nrow(bouts)) {
          break
        }
        while (timestamp + winSize >= boutstop) {
          if (r == nrow(bouts)) {
            break
          }
          r = r + 1
          boutstart = strptime(str_trim(bouts[r, ]$StartDateTime), dateFmt)
          boutstop = strptime(str_trim(bouts[r, ]$EndDateTime), dateFmt)
        }
        if (timestamp >= boutstart) {
          # the window is within this bout - add the label
          label = sub(" ", "", str_trim(bouts[r, c("behavior")]))
        }
      }
      # write label to output file
      cat(strftime(timestamp, "%Y-%m-%d %H:%M:%S,"), file=out, append=TRUE)
      cat(label, file=out, append=TRUE)
      cat("\n", file=out, append=TRUE)
      # next window
      l = l + 1
      label = "NULL"
      timestamp = as.POSIXlt(timestamp + winSize)
      # if it's a new day start new file
      if (timestamp$mday != day) {
        day = timestamp$mday
        out = file.path(outputFile, paste0(strftime(timestamp, "%Y-%m-%d"), ".csv"))
        cat(strftime(timestamp, "%Y-%m-%d"), '\n')
        if (file.exists(out)) {
          file.remove(out)
        }
        cat("timestamp,behavior\n", file=out, append=TRUE)
      }
    }
  }
}

senseCamLabelsFile = function(inputFile, outputFile){
  # convert a file with bout-level annotations from senseCam labels to 6 categories
  # inputFile: a file in senseCam annotation format
  # outputFile: a file in 6-activity annotation format
  
  # read the input file
  bouts = read.csv(inputFile, header=TRUE, stringsAsFactors=FALSE)
  # default behavior is null
  bouts$behavior = "NULL"
  bouts$PtID = NULL
  
  for (i in 1:nrow(bouts)){
    # paste together PA1 - PA4
    labels = c(bouts[i, ]$PA1, bouts[i, ]$PA2, bouts[i, ]$PA3, bouts[i, ]$PA4)
    labels[is.na(labels)] = " "
    
    if ("02A. Sedentary" %in% labels){
      if (!("03H. Car" %in% labels)&!("03I. Other Vehicle" %in% labels)&!("03D. Sports" %in% labels)){
        # sedentary but not car, other vehicle, or sports
        bouts[i, ]$behavior = "Sedentary"
      }
    }
    if (("02B. Standing Still" %in% labels)&!("03D. Sports" %in% labels)){
      # standing still but not sports
      bouts[i, ]$behavior = "StandingStill"
    }
    if (("02C. Standing Moving" %in% labels)&!("03D. Sports" %in% labels)){
      # standing moving but not sports
      bouts[i, ]$behavior = "StandingMoving"
    }
    if (("02D. Walking/Running" %in% labels)&!("03D. Sports" %in% labels)){
      # walking/running but not sports
      bouts[i, ]$behavior = "Walking"
    }
    if (("02E. Biking" %in% labels)){
      bouts[i, ]$behavior = "Biking"
    }
    if (("03H. Car" %in% labels)|("03I. Other Vehicle" %in% labels)){
      # car or other vehicle
      bouts[i, ]$behavior = "Vehicle"
    }
  }
  # now delete old columns
  bouts$PA1 = NULL
  bouts$PA2 = NULL
  bouts$PA3 = NULL
  bouts$PA4 = NULL
  bouts = bouts[c("identifier","StartDateTime","EndDateTime","behavior")]
  # write the new file
  write.csv(bouts, file=outputFile, quote=FALSE, row.names=FALSE)
}
senseCamLabels = function(input, output){
  # convert a bout-level annotations from senseCam labels to 6 categories
  # input: input directory or file
  # output: directory to save new annotations
  if (file.info(input)$isdir){
    # input is a directory
    files = list.files(input)
    if (!file.exists(output)) {
      dir.create(output, recursive=TRUE)
    }
    for (file in files){
      cat(file, "\n")
      senseCamLabelsFile(file.path(input, file), file.path(output, file))
    }
  } else {
    senseCamLabelsFile(input, output)
  }
}