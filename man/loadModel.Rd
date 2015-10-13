\name{loadModel}
\alias{loadModel}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load model
}
\description{
Function to load a model.
}
\usage{
loadModel(modelName, which)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{modelName}{
   Path to model.
  }
  \item{which}{
  String specifying which part of model to return. One of "winSize", "rf", or "hmm".
  }
}
\value{
A model (or parameter).
}
\author{
Katherine Ellis
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
