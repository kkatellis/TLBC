\name{loadPredictions}
\alias{loadPredictions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load predictions
}
\description{
Function to load predictions from csv files in a directory.
}
\usage{
loadPredictions(predDir, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{predDir}{
   Path to directory containing prediction files.
  }
  \item{names}{
  (Optional) If specified, load only predictions for identifiers in this list.
  }
}
\value{
A data frame of predictions.
}
\author{
Katherine Ellis
}
\seealso{
\code{\link{loadPredictionsAndLabels}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
