\name{loadPredictionsAndLabels}
\alias{loadPredictionsAndLabels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load predictions and labels
}
\description{
Function to load corresponding labels and predictions.
}
\usage{
loadPredictionsAndLabels(labelDir, predDir, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
\item{labelDir}{
   Path to directory containing label files.
  }
  \item{predDir}{
   Path to directory containing prediction files.
  }
  \item{names}{
  (Optional) If specified, load only data for identifiers in this list.
  }
}
\value{
A data frame containing predictions and labels.
}
\author{
Katherine Ellis
}
\seealso{
\code{\link{loadPredictions}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
