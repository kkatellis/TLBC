## functions to compute accelerometer features

computeOneAccFeat = function(w, Fs) {
  # function to compute one accelerometer feature vector from a window
  # axis 1: vertical (z)
  # axis 2: horizontal (y)
  # axis 3: perpindicular (x)
  
  # INPUT: 
  # w: window of raw accelerometer data
  # Fs: sample frequency
  
  # gravity component
  
    g = matrix(0, nrow(w), 3)
    x = 0.9
    g[1, ] = (1-x) * w[1, ]
    for (n in 2:nrow(w)) {
      g[n, ] = x * g[n-1,] + (1-x) * w[n, ]
    }
    g = g[Fs:nrow(g), ] # ignore beggining
    gg = colMeans(g)
    #w = w - gg
    w = t(apply(w,1,function(x) x-gg))

  # compute the vector magnitude
  v = sqrt(rowSums(w ^ 2))
  # mean
  fMean = mean(v)
  # standard deviation
  fStd = sd(v)
  # coefficient of variation
  if (fMean > 0) {
    fCoefVariation = fStd / fMean
  } else {
    fCoefVariation = 0
  }
  # median
  fMedian = median(v)
  # minimum
  fMin = min(v)
  # maximum
  fMax = max(v)
  # 25th percentile
  f25thP = quantile(v, 0.25)[[1]]
  # 75th percentile
  f75thP = quantile(v, 0.75)[[1]]
  
  # autocorrelation
  a = acf(v, plot=FALSE)
  fAutocorr = which.max(abs(a$acf[2:length(a$acf)])) / (nrow(w) / Fs)
  
  # correlation between axes
  if ((sd(w[, 3]) > 0) & (sd(w[, 2]) > 0)) {
    fCorrxy = cor(w[, 3], w[, 2])
  } else {
    fCorrxy = 0
  }
  if ((sd(w[, 3]) > 0) & (sd(w[, 1]) > 0)) {
    fCorrxz = cor(w[, 3], w[, 1])
  } else {
    fCorrxz = 0
  }
  if ((sd(w[, 2]) > 0) & (sd(w[, 1]) > 0)) {
    fCorryz = cor(w[, 2], w[, 1])
  } else {
    fCorryz = 0
  }
  
  if (is.na(fCorrxy)) fCorrxy = 0
  if (is.na(fCorrxz)) fCorrxz = 0
  if (is.na(fCorryz)) fCorryz = 0
  
  # average roll pitch and yaw
  fAvgRoll = mean(atan2(w[, 2],w[, 1]))
  fAvgPitch = mean(atan2(w[, 1],w[, 3]))
  fAvgYaw = mean(atan2(w[, 2],w[, 3]))
  
  # standard deviation of roll pitch and yaw
  fSdRoll = sd(atan2(w[, 2],w[, 1]))
  fSdPitch = sd(atan2(w[, 1],w[, 3]))
  fSdYaw = sd(atan2(w[, 2],w[, 3]))
  
  # roll pitch and yaw of gravity component
  fRollG = atan2(g[2], g[1])
  fPitchG = atan2(g[1], g[3])
  fYawG = atan2(g[2], g[3])
  
  # frequency-based features
  s = specgram(v, n=length(v), Fs=Fs)
  S = abs(s$S)
  f = S / max(S)
  freq = s$f
  f1 = f[freq >= 0.1]
  freq1 = freq[freq >= 0.1]
  # dominant frequency
  fFmax = freq1[which.max(f1)]
  # power at dominant frequency
  fPmax = max(f1)
  
  band = f[freq > 0.3 & freq < 3]
  # dominant frequency within band
  fPmaxBand = max(band)
  freqband = freq[freq > 0.3 & freq < 3]
  fFmaxBand = freqband[which.max(band)]
  # entropy
  fEntropy = - sum(f * log(f))
  
  # components of FFT by frequency band
  s = specgram(v, n=Fs, Fs=Fs)
  S = abs(s$S)
  f = S / max(S)
  freq = s$f
  f = rowSums(f) / ncol(f)
  FFT0 = f[1]
  FFT1 = f[2]
  FFT2 = f[3]
  FFT3 = f[4]
  FFT4 = f[5]
  FFT5 = f[6]
  FFT6 = f[7]
  FFT7 = f[8]
  FFT8 = f[9]
  FFT9 = f[10]
  FFT10 = f[11]
  FFT11 = f[12]
  FFT12 = f[13]
  FFT13 = f[14]
  FFT14 = f[15]
  
  # return the feature vector
  return(c(fMean, fStd, fCoefVariation, fMedian, fMin, fMax, f25thP, f75thP, fAutocorr, fCorrxy, fCorrxz, fCorryz, fAvgRoll, fAvgPitch, fAvgYaw, fSdRoll, fSdPitch, fSdYaw, fRollG, fPitchG, fYawG, fFmax, fPmax, fFmaxBand, fPmaxBand, fEntropy, FFT0, FFT1, FFT2, FFT3, FFT4, FFT5, FFT6, FFT7, FFT8, FFT9, FFT10, FFT11, FFT12, FFT13, FFT14))
}
extractAccFeatsFile = function(inputFile, outputPath, winSize) {
  # function to extract accelerometer features from raw actigraph file
  # INPUT: 
  # inputFile: path to raw actigraph file (Hz level)
  # outputPath: path to directory where feature files will be saved (by day)
  # winSize: window size (in seconds)
  
  # open the file and read the header
  con = file(inputFile, open = "r")
  line1 = readLines(con, n = 1)
  # extract the sampling frequency (Fs)
  Fs = as.numeric(str_match(line1, "(\\d+) Hz")[1, 2])
  # extract the start date and time
  dateFmt = str_match(line1, "date format ([a-z,A-Z,/]*)")[1, 2]
  dateFmt = gsub("yyyy", "%Y", dateFmt)
  dateFmt = gsub("M", "%m", dateFmt)
  dateFmt = gsub("d", "%d", dateFmt)
  line1 = readLines(con, n = 1)
  line1 = readLines(con, n = 1)
  StartTime = gsub("Start Time ", "", line1)
  line1 = readLines(con, n = 1)
  StartDate = gsub("Start Date ", "", line1)
  line1 = readLines(con, n = 6)
  st = strptime(paste(StartDate, StartTime), paste(dateFmt, "%H:%M:%S"))
  # get day of month to organize feature files
  day = st$mday
  # set up file path to save features
  out = file.path(outputPath, strftime(st, "%Y-%m-%d"))
  cat(strftime(st, "%Y-%m-%d"), '\n')
  if (!file.exists(outputPath)) {
    dir.create(outputPath, recursive=TRUE)
  }
  # write header to feature file
  cat("timestamp,mean,sd,coefvariation,median,min,max,25thp,75thp,autocorr,corrxy,corrxz,corryz,avgroll,avgpitch,avgyaw,sdroll,sdpitch,sdyaw,rollg,pitchg,yawg,fmax,pmax,fmaxband,pmaxband,entropy,fft0,fft1,fft2,fft3,fft4,fft5,fft6,fft7,fft8,fft9,fft10,fft11,fft12,fft13,fft14\n", file=out, append=FALSE)
  # skip non-numeric lines at beginning of data
  while (is.na(suppressWarnings(as.numeric(strsplit(line1 <-readLines(con, n=1), ",")[[1]][1])))){}
  
  # read in the rest of the window
  lines1 <- readLines(con, n = Fs * winSize - 1)
  line1 = c(line1, lines1)
  notDone = TRUE
  while (notDone) {
    line1 = gsub("\"", "", line1)
    M = as.matrix(strsplit(line1, " "))
    M = sapply(M, strsplit, ",")
    M = sapply(M, as.numeric)
    M = t(M)
    # compute the feature from the data matrix M
    feat = computeOneAccFeat(M, Fs)
    # write the feature and timestamp to the file
    cat(strftime(st, "%Y-%m-%d %H:%M:%S,"), file=out, sep = "", append=TRUE)
    cat(feat, file=out, sep=",", append=TRUE)
    cat('\n', file=out, append=TRUE)
    
    # advance to next window
    st = as.POSIXlt(st + winSize)
    if (st$mday != day) {
      # if it's a new day start a new feature file
      out = file.path(outputPath, strftime(st, "%Y-%m-%d"))
      cat(strftime(st, "%Y-%m-%d"), '\n')
      cat("timestamp,mean,sd,coefvariation,median,min,max,25thp,75thp,autocorr,corrxy,corrxz,corryz,avgroll,avgpitch,avgyaw,sdroll,sdpitch,sdyaw,rollg,pitchg,yawg,fmax,pmax,fmaxband,pmaxband,entropy,fft0,fft1,fft2,fft3,fft4,fft5,fft6,fft7,fft8,fft9,fft10,fft11,fft12,fft13,fft14\n", file=out, append=FALSE)
      day = st$mday
    }
    # read the next window of data
    line1 <- readLines(con, n = Fs * winSize)
    # if we read less than a full window the file is over and we are done
    if (length(line1) < Fs * winSize) notDone = FALSE
  }
  # close the raw data file
  close(con)
}
extractAccelerometerFeatures = function(input, output, winSize, names = NULL) {
  # function to compute accelerometer features for all actigraph files in a directory
  # INPUTS:
  # input: directory containing raw actigraph files
  # output: directory to save features
  # winSize: window size (in seconds)
  # names (optional): only extract features for these files
  
  # check if input is a directory
  if (file.info(input)$isdir){
    # if no names were provided compute features for all files in directory
    if (is.null(names)) {
      names = file_path_sans_ext(list.files(input))
    }
    if (length(names) == 0) {
      stop("couldn't find any accelerometer files\n")
    }
    for (name in names) {
      outputFile = file.path(output, name)
      if (!file.exists(outputFile)) {
        cat(name, "...\n")
        extractAccFeatsFile(file.path(input, paste0(name, ".csv")), outputFile, winSize)
      } else {
        cat(name, "alreday exists\n")
      }
    }
  } else {
    # if it's not a directory assume it's a actigraph file and extract features
    extractAccFeatsFile(input, output, winSize)
  }
}
