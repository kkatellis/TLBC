\name{testTwoRFs}
\alias{testTwoRFs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test two random forest classifiers
}
\description{
Function to apply two random forest classifiers to some data.
}
\usage{
testTwoRFs(featDirs1, featDirs2, rf1, rf2, saveDir, testNames)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{featDirs1}{
  Path to a directory (or list of directories) containing features corresponding to the first RF classifier.
  }
  \item{featDirs2}{
  Path to a directory (or list of directories) containing features corresponding to the second RF classifier.
  }
  \item{rf1}{
  A random forest model.
  }
  \item{rf2}{
  A second random forest model.
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
\code{\link{testRF}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
