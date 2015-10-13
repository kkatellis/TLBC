\name{TLBC-package}
\alias{TLBC-package}
\alias{TLBC}
\docType{package}
\title{
Two-level Behavior Classification
}
\description{
TLBC is a package for using a two-level random forest and hidden Markov model for behavior classification from raw tri-axial accelerometer and/or GPS data.

This code works with csv data from Actigraph accelerometers (please export without timestamps), and with GPS data processed by the PALMS GPS cleaning software. 

The TLBC classifier uses six behavior labels:
\itemize{
\item{Sitting}
\item{Standing Still}
\item{Standing Moving}
\item{Walking/Running}
\item{Bicycling}
\item{Vehicle}
}

Function \code{\link{classify}} uses a pre-learned TLBC model to classify accelerometer and/or GPS data with behavior labels. Pre-trained models that have been trained on three UCSD datasets are \href{http://ieng9.ucsd.edu/~kellis/TLBC.html}{available for download}.

Function \code{\link{trainModel}} trains a TLBC model from annotated accelerometer and/or GPS data.

Function \code{\link{calcPerformance}} computes the accuracy of predictions made on a given dataset.

Function \code{\link{looXval}} performs leave-one-out cross-validation on a dataset.

}
\details{
\tabular{ll}{
Package: \tab TLBC\cr
Type: \tab Package\cr
Version: \tab 1.0\cr
Date: \tab 2015-05-29\cr
License: \tab GPL-2\cr
}
%~~ An overview of how to use the package, including the most important functions ~~
}
\author{
Katherine Ellis <kellis@ucsd.edu>
}
%\references{}
\keyword{ package }
\seealso{
%~~ Optional links to other man pages, e.g. ~~
\code{\link[randomForest]{randomForest}}, \code{\link[HMM]{HMM}}
}
\examples{
%~~ simple examples of the most important functions ~~
\dontrun{

# train a new model
myAnnotations="~/myStudy/annotations.csv"
myAccel="~/myStudy/HipGT3X+"
myGPS="~/myStudy/GPS.csv"
WS=60
myModel="~/myStudy/myModel.RData"
trainModel(annotations=myAnnotations, accelerometers=myAccel, GPS=myGPS, winSize=WS, 
modelName=myModel)

# classify using a model computed yourself
myAccel="~/myStudy/HipGT3X+"
myGPS="~/myStudy/GPS.csv"
myModel="~/myStudy/myModel.RData"
myPredictions="~/myStudy/myModelPredictions"
classify(accelerometers=myAccel, GPS=myGPS, modelName=myModel, saveDir=myPredictions)

# compute the performance of a model on a dataset
myAnnotations="~/myStudy/annotations.csv"
myPredictions="~/myStudy/myModelPredictions"
WS=60
calcPerformance(annotations=myAnnotations, predictions=myPredictions, winSize=WS)

# perform leave-one-out cross-validation on a dataset
myAnnotations="~/myStudy/annotations.csv"
myAccel="~/myStudy/HipGT3X+"
WS=60
myPredictions="~/myStudy/looXvalPredictions"
looXval(annotations=myAnnotations, accelerometers=myAccel, winSize=WS, saveDir=myPredictions)

}

}
