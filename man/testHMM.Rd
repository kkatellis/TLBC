\name{testHMM}
\alias{testHMM}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Test a hidden Markov model
}
\description{
Function to apply a HMM classifier to some data.
}
\usage{
testHMM(predDir, modelName, saveDir, names)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{predDir}{
  Path to a directory containing predictions made by the random forest classiifer (\emph{i.e.}, the \emph{saveDir} argument of \code{\link{testRF}}).
  }
  \item{modelName}{
  Path to pre-trained model.
  }
  \item{saveDir}{
  Path to a directory where predictions will be saved. Predictions will be saved in files named <\emph{identifier}>.csv with two fields: \emph{timestamp, prediction}.
  }
  \item{names}{
  List of participant identifiers to use.
  }
}
\author{
Katherine Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%
\seealso{
\code{\link{classify}, \link{testRF}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
