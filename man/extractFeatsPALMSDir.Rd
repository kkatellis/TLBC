\name{extractFeatsPALMSDir}
\alias{extractFeatsPALMSDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract GPS features from a PALMS directory
}
\description{
Function to extract GPS features from a directory containing PALMS-filtered GPS data files.
}
\usage{
extractFeatsPALMSDir(inputDir, outputDir, winSize, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputDir}{
  Path to a directory containing PALMS-filtered GPS data files. Each file should be a csv file with the following fields: \emph{identifier, dateTime, speed, distance, duration, ele, elevationDelta, lat, lon, nsatUsed, nsatView, snrUsed, snrView, fixType}.
  }
  \item{outputDir}{
  Path to a directory to save computed features. A separate directory for each input file will be created containing features for each day. 
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
\code{\link{extractFeatsPALMSOneFile}, \link{computeOneGPSFeat}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
