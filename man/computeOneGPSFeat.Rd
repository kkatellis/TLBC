\name{computeOneGPSFeat}
\alias{computeOneGPSFeat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute one GPS feature
}
\description{
Function to compute one GPS feature from a data window.
}
\usage{
computeOneGPSFeat(w, lastCoordinates)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
  Data frame of GPS measurements from the PALMS system. Should contain the following fields: \emph{identifier, dateTime, speed, distance, duration, ele, elevationDelta, lat, lon, nsatUsed, nsatView, snrUsed, snrView, fixType}
  }
  \item{lastCoordinates}{
  Coordinates of last data sample, to compute change in position - data frame with fields \emph{lat} and \emph{lon}.
  }
}
\value{
Vector of GPS features.
}
\author{
Katherine Ellis
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{extractFeatsPALMSDir}, \link{extractFeatsPALMSOneFile}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
