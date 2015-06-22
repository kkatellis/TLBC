\name{sensorsToFeatures}
\alias{sensorsToFeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to extract featurese from raw sensor data
}
\description{
Extracts features from accelerometer and/or GPS data.
}
\usage{
sensorsToFeatures(accelerometers = NULL, GPS = NULL, winSize, names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{accelerometers}{
(Optional) Path to a directory (or a list of paths to multiple directories) containing actigraph accelerometer data files. Accelerometer data files should be csv files output in "raw" format by ActiLife (without timestamps), and named by the participant identifier, \emph{e.g.}, Participant01.csv.
}
  \item{GPS}{
(Optional) Path to a PALMS-processed GPS data file (or a directory containing GPS data files). GPS data files should be in csv format with the following fields: \emph{identifier, dateTime, speed, ele, elevationDelta, lat, lon, nsatView, snrView}. \emph{identifier} should be the participant identifier, \emph{e.g.} Participant01. If \emph{GPS} is a path to a directory, each file in the directory should correspond to a participant, and the file name should be the participant identifier, \emph{e.g.}, Participant01.csv.
}
  \item{winSize}{
Window size in seconds.
}
  \item{names}{
(Optional) List of identifiers to use.
}
}
\value{
List of feature directories created.
}
\author{
Kat Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{trainModel}}
}
%%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
