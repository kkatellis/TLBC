\name{trainHMM}
\alias{trainHMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Train a hidden Markov model
}
\description{
Function to train a HMM classifier from some data and a trained random forest model.
}
\usage{
trainHMM(labelDir, rf, names, combineStanding=FALSE) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labelDir}{
  Path to a directory containing instance-level annotations, \emph{i.e.}, created by the function \emph{annotationsToLabels}.
  }
  \item{rf}{
  A random forest model.
  }
  \item{names}{
  List of participant identifiers to use.
  }
  \item{combineStanding}{
  If TRUE, combine the labels \emph{standing still} and \emph{standing moving} into a single label \emph{standing}.
  }
}
\author{
Katherine Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%
\seealso{
\code{\link{trainModel}, \link{trainRF}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
