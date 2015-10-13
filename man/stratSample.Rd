\name{stratSample}
\alias{stratSample}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Stratified sample
}
\description{
Function to choose a random sample of data stratified by label.
}
\usage{
stratSample(labels, nsamp)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labels}{
  Vector of strings denoting labels.
  }
  \item{nsamp}{
  Number of items to sample from each label class.
  }
}
\value{
Vector of indices chosen.
}
\author{
Katherine Ellis
}
%% ~Make other sections like Warning with \section{Warning }{....} ~

% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
