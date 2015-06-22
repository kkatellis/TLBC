\name{trainModel}
\alias{trainModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to train a two-level model from accelerometer and/or GPS data
}
\description{
Trains a two-level model from accelerometer and/or GPS data.
}
\usage{
trainModel(annotations, accelerometers = NULL, GPS = NULL, winSize = 60, modelName, names = NULL, strat = TRUE, ntree = 500, mtry = NULL, replace = TRUE, nsample = 10000, nodesize = 1, sampsize = 10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{annotations}{
Path to a file containing bout-level annotations (or directory of files containing bout-level annotations). Should be csv format with fields: \emph{identifier,StartDateTime,EndDateTime,behavior}.

Or, path to a directory containing instance-level annotations, \emph{i.e.}, created by the function \emph{annotationsToLabels}.
}
  \item{accelerometers}{
(Optional) Path to a directory (or list of directories) containing actigraph accelerometer data files. Accelerometer data files should be csv files output in "raw" format by ActiLife (without timestamps), and named by the participant identifier, \emph{e.g.}, Participant01.csv.

Or, path to a directory (or list of directories) containing previously computed accelerometer features, \emph{i.e.}, computed by the function \emph{sensorsToFeatures}.
}
  \item{GPS}{
(Optional) Path to a PALMS-processed GPS data file (or a directory containing GPS data files). GPS data files should be in csv format with the following fields: \emph{identifier, dateTime, speed, ele, elevationDelta, lat, lon, nsatView, snrView}. \emph{identifier} should be the participant identifier, \emph{e.g.} Participant01. If \emph{GPS} is a path to a directory, each file in the directory should correspond to a participant, and the file name should be the participant identifier, \emph{e.g.}, Participant01.csv.

Or, path to a directory containing previously computed GPS features, \emph{i.e.}, computed by the function \emph{sensorsToFeatures}.
}
  \item{winSize}{
%%     ~~Describe \code{winSize} here~~
}
  \item{modelName}{
Path to location to save model.
}
  \item{names}{
(Optional) List of participant identifiers to use.
}
  \item{strat}{
logical: use stratified sampling for the random forest?
}
  \item{ntree}{
Number of trees in the random forest
}
  \item{mtry}{
Number of variables randomly sampled as candidates at each split in the random forest.
}
  \item{replace}{
Should sampling in the random forest be done with or without replacement?
}
  \item{nsample}{
Number of instances to sample.
}
  \item{nodesize}{
Minimum size of terminal nodes in the random forest.
}
  \item{sampsize}{
Size of sample to draw for the random forest.
}
}
\author{
Kat Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{randomForest}}, \code{\link{looXval}}
}
%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
