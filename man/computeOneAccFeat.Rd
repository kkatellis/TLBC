\name{computeOneAccFeat}
\alias{computeOneAccFeat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute one acceleration feature
}
\description{
Function to compute one acceleration feature from a data window.
}
\usage{
computeOneAccFeat(w, Fs)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{w}{
  N x 3 matrix of 3-axis accelerometer measurements.
  }
  \item{Fs}{
  Sample frequency, in Hertz.
  }
}
\value{
Vector of acceleration features.
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
