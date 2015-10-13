\name{extractAccFeatsFile}
\alias{extractAccFeatsFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract accelerometer features from a file
}
\description{
Function to extract accelerometer features from a single GT3X+ raw data file
}
\usage{
extractAccFeatsFile(inputFile, outputPath, winSize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputFile}{
  Path to a GT3X+ raw data file.
  }
  \item{outputPath}{
  Path to a directory to save computed features. A separate file for each day will be created inside the directory. 
  }
  \item{winSize}{
  Window size in seconds.
  }
}
\author{
Katherine Ellis
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{extractAccelerometerFeatures}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
