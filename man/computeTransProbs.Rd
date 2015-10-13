\name{computeTransProbs}
\alias{computeTransProbs}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Compute transition probabilities
}
\description{
Compute the transition probabilities of an HMM from a state sequence.
}
\usage{
computeTransProbs(stateSeq)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stateSeq}{
  Vector of states.
  }
}
\value{
Matrix of probabilities for each transition between states.
}
\author{
Katherine Ellis
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{trainModel}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
