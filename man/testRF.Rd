\name{testRF}
\alias{testRF}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test a random forest classifier
}
\description{
Function to apply a random forest classifier to some data.
}
\usage{
testRF(featDirs, modelName, saveDir, testNames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{featDirs}{
  Path to a directory (or list of directories) containing features, \emph{i.e.}, computed by the function \emph{sensorsToFeatures}.
  }
  \item{modelName}{
  Path to pre-trained model.
  }
  \item{saveDir}{
  Path to a directory where predictions will be saved. Predictions will be saved in files named <\emph{identifier}>.csv with two fields: \emph{timestamp, prediction}.
  }
  \item{testNames}{
  List of participant identifiers to use.
  }
}
\author{
Katherine Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%
\seealso{
\code{\link{classify}, \link{testHMM}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
