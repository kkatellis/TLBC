\name{trainRF}
\alias{trainRF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Train a random forest classifier
}
\description{
Function to train a random forest classifier from some data.
}
\usage{
trainRF(labelDir, featDirs, names, combineStanding=FALSE, strat=TRUE, ntree=500, 
mtry=NULL, replace=TRUE, nsample=10000, nodesize=1, sampsize=10000)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labelDir}{
  Path to a directory containing instance-level annotations, \emph{i.e.}, created by the function \emph{annotationsToLabels}.
  }
  \item{featDirs}{
  Path to a directory (or list of directories) containing features, \emph{i.e.}, computed by the function \emph{sensorsToFeatures}.
  }
  \item{names}{
  List of participant identifiers to use.
  }
  \item{combineStanding}{
  (Optional) If TRUE, combine the labels \emph{standing still} and \emph{standing moving} into a single label \emph{standing}.
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

%%
\seealso{
\code{\link{trainModel}, \link{trainHMM}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
