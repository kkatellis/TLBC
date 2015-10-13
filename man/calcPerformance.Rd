\name{calcPerformance}
\alias{calcPerformance}
\alias{calcPerformanceFromLabels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to calculate performance of a classification model
}
\description{
Calculates several performance metrics.
}
\usage{
calcPerformance(annotations, predictions, winSize, names=NULL, combineStanding=FALSE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{annotations}{
Path to file containing bout-level annotations, a directory of files containing bout-level annotations, or a directory of instance-level annotations (\emph{i.e.}, the output of funciton \emph{annotationsToLabels}).
}
  \item{predictions}{
Path to directory containing predictions (\emph{i.e.}, the \emph{saveDir} argument to the function \emph{classify}).
}
  \item{winSize}{
Window size in seconds.
}
  \item{names}{
(Optional) List of identifiers to use.
}
  \item{combineStanding}{
logical: combine standing still and standing moving into a single category?
}
}
%%\details{}
\value{
Object containing confusion matrix and several performance metrics.
}
\author{
Katherine Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{confusionMatrix}}
}
\examples{
\dontrun{

# compute the performance of a model on a dataset
myAnnotations="~/myStudy/annotations.csv"
myPredictions="~/myStudy/myModelPredictions"
WS=60
calcPerformance(annotations=myAnnotations, predictions=myPredictions, winSize=WS)
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
