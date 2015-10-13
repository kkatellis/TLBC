\name{writePredictions}
\alias{writePredictions}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Write predictions to a file
}
\description{
Function to write predictions made by TLBC model to a csv file.
}
\usage{
writePredictions(values, timestamps, saveFile)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{values}{
  Vector of predicted labels.
  }
  \item{timestamps}{
  Vector of timestamps.
  }
  \item{saveFile}{
  Path to file where predictions will be saved.
  }
}

\author{
Katherine Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
