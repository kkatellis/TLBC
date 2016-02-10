\name{trainModel}
\alias{trainModel}
\alias{trainFromFeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to train a two-level model from accelerometer and/or GPS data
}
\description{
Trains a two-level model from accelerometer and/or GPS data.
}
\usage{
trainModel(annotations, accelerometers=NULL, GPS=NULL, winSize=60, modelName, names=NULL, 
strat=TRUE, ntree=500, mtry=NULL, replace=TRUE, nsample=10000, nodesize=1, sampsize=10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{annotations}{
Path to a file containing bout-level annotations (or directory of files containing bout-level annotations). Should be csv format with fields: \emph{identifier,StartDateTime,EndDateTime,behavior}.

Or, path to a directory containing instance-level annotations, \emph{i.e.}, created by the function \emph{annotationsToLabels}.

Date time format should be: yyyy-mm-dd HH:MM:SS (24-hour clock).

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
Window size in seconds.
}
  \item{modelName}{
Path to location to save model.
}
  \item{names}{
(Optional) List of participant identifiers to use.
}
  \item{strat}{
(Optional) logical: use stratified sampling for the random forest?
}
  \item{ntree}{
(Optional) Number of trees in the random forest
}
  \item{mtry}{
(Optional) Number of variables randomly sampled as candidates at each split in the random forest.
}
  \item{replace}{
(Optional) Should sampling in the random forest be done with or without replacement?
}
  \item{nsample}{
(Optional) Number of instances to sample.
}
  \item{nodesize}{
(Optional) Minimum size of terminal nodes in the random forest.
}
  \item{sampsize}{
(Optional) Size of sample to draw for the random forest.
}
}
\author{
Katherine Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{classify}}, \code{\link{looXval}}
}
\examples{
\dontrun{
myAnotations="~/myStudy/annotations.csv"
myAccel="~/myStudy/HipGT3X+"
myGPS="~/myStudy/GPS.csv"
winSize=60
myModel="~/myStudy/myModel.RData"
trainModel(annotations=myAnotations, accelerometers=myAccel, GPS=myGPS, winSize=WS, 
modelName=myModel)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
