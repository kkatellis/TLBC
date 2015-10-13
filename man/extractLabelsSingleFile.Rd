\name{extractLabelsSingleFile}
\alias{extractLabelsSingleFile}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Extract labels from a directory
}
\description{
Function to extract labels from a directory containing annotation files.
}
\usage{
extractLabelsSingleFile(inputFile, outputDir, winSize)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputFile}{
  Path to a file containing bout-level annotations. Should be csv format with fields: \emph{identifier, StartDateTime, EndDateTime, behavior}.
  }
  \item{outputDir}{
  Path to a directory to save labels. 
  }
  \item{winSize}{
  Window size in seconds.
  }
}
\author{
Katherine Ellis
}
%% ~Make other sections like Warning with \section{Warning }{....} ~
\seealso{
\code{\link{annotationsToLabels}, \link{extractLabelsDir}}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
