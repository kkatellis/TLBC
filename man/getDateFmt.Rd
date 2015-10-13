\name{getDateFmt}
\alias{getDateFmt}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Get date format
}
\description{
Function to guess the date format from an input string
}
\usage{
getDateFmt(inputString)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{inputString}{
   String containing a date formatted either as yyyy-mm-dd HH:MM:SS or mm/dd/yyyy HH:MM:SS
  }
}
\value{
A date format string, either "\%Y-\%m-\%d \%H:\%M:\%S" or "\%m/\%d/\%Y \%H:\%M:\%S"
}
\author{
Katherine Ellis
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
