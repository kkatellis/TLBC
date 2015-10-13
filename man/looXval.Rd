\name{looXval}
\alias{looXval}
\alias{looXvalFromFeats}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to perform leave-one-out cross-validation
}
\description{
Performs leave-one-out cross-validation
}
\usage{
looXval(annotations, accelerometers=NULL, GPS=NULL, winSize=60, saveDir, names=NULL, 
strat=TRUE)
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
Window size in seconds.
}
  \item{saveDir}{
Path to a directory where predictions will be saved. Predictions will be saved in files named <\emph{identifier}>.csv with two fields: \emph{timestamp,prediction}.
}
  \item{names}{
(Optional) List of participant identifiers to use.
}
  \item{strat}{
logical: use stratified sampling for the random forest?
}
}\author{
Katherine Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{trainModel}},\code{\link{classify}}
}
\examples{
\dontrun{

# perform leave-one-out cross-validation on a dataset
myAnnotations="~/myStudy/annotations.csv"
myAccel="~/myStudy/HipGT3X+"
winSize=60
myPredictions="~/myStudy/looXvalPredictions"
looXval(annotations=myAnnotations, accelerometers=myAccel, winSize=WS, saveDir=myPredictions)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
