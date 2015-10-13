\name{annotationsToLabels}
\alias{annotationsToLabels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to convert bout-level annotations to instance-level labels
}
\description{
Converts bout-level annotations to instance-level labels.
}
\usage{
annotationsToLabels(annotations, winSize, names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{annotations}{
Path to file containing bout-level annotations, or directory of files containing bout-level annotations. Should be csv format with fields: \emph{identifier, StartDateTime, EndDateTime, behavior}.
}
  \item{winSize}{
Window size in seconds.
}
  \item{names}{
(Optional) If provided, extract annotations only for identifiers in this list.
}
}
%%\details{}
\value{
Path to directory where instance-level label files are saved.
}
\author{
Katherine Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%\examples{}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
