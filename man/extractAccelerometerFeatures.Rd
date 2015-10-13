\name{extractAccelerometerFeatures}
\alias{extractAccelerometerFeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract accelerometer features
}
\description{
Function to extract accelerometer features for all GT3X+ raw data files in a directory.
}
\usage{
extractAccelerometerFeatures(input, output, winSize, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{input}{
  Path to GT3X+ raw data file or a directory containing GT3X+ raw data files.
  }
  \item{output}{
  Path to a directory to save computed features. A separate directory for each GT3X+ file will be created containing features for each day. 
  }
  \item{winSize}{
  Window size in seconds.
  }
  \item{names}{
  (Optional) If provided, compute features only for filenames in this list.
  }
}
\author{
Katherine Ellis
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{extractAccFeatsFile}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
