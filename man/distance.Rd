\name{distance}
\alias{distance}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Distance}
\description{
Function to compute the distance between two sets of latitude and longitude coordiantes.
}
\usage{
distance(origin, destination)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{origin}{
  A data frame with the the fields \emph{lat} and \emph{lon}.
  }
  \item{destination}{
  A data frame with the the fields \emph{lat} and \emph{lon}.
  }
}
\value{
Distance in meters.
}
\author{
Katherine Ellis
}
\seealso{
\code{\link{computeOneGPSFeat}}
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
