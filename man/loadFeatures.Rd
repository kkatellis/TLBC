\name{loadFeatures}
\alias{loadFeatures}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load features
}
\description{
Function to load feature data.
}
\usage{
loadFeatures(featDirs, names=NULL) 
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{featDirs}{
   List of paths to feature directories containing feature files.
  }
  \item{names}{
  (Optional) If provided, only load data for identifiers provided in this list.
  }
}
\value{
A data frame of features.
}
\author{
Katherine Ellis
}
\seealso{
\code{\link{loadData}, \link{loadLabels}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
