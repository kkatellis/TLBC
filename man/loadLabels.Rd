\name{loadLabels}
\alias{loadLabels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Load labels
}
\description{
Function to load label data.
}
\usage{
loadLabels(labelDir, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{labelDir}{
   Path to label directories containing instance-level annotation files.
  }
  \item{names}{
  (Optional) If provided, only load labels for identifiers provided in this list.
  }
}
\value{
A data frame of labels.
}
\author{
Katherine Ellis
}
\seealso{
\code{\link{loadData}, \link{loadFeatures}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
