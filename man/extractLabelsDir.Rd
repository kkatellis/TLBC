\name{extractLabelsDir}
\alias{extractLabelsDir}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract labels from a directory
}
\description{
Function to extract labels from a directory containing annotation files.
}
\usage{
extractLabelsDir(inputDir, outputDir, winSize, names=NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputDir}{
  Path to a directory of files containing bout-level annotations. Should be csv format with fields: \emph{identifier, StartDateTime, EndDateTime, behavior}.
  }
  \item{outputDir}{
  Path to a directory to save labels. 
  }
  \item{winSize}{
  Window size in seconds.
  }
  \item{names}{
  (Optional) If provided, extract labels only for identifiers in this list.
  }
}
\author{
Katherine Ellis
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{annotationsToLabels}, \link{extractLabelsSingleFile}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
