\name{loadData}
\alias{loadData}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load data
}
\description{
Function to load corresponding label and feature data.
}
\usage{
loadData(labelDir, featDirs, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labelDir}{
   Path to a label directory containing instance-level label files.
  }
  \item{featDirs}{
   List of paths to feature directories containing feature files.
  }
  \item{names}{
  (Optional) If provided, only load data for identifiers provided in this list.
  }
}
\value{
A list containing (1) a data frame of labels and (2) a data frame of features.
}
\author{
Katherine Ellis
}
\seealso{
\code{\link{loadFeatures}, \link{loadLabels}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
