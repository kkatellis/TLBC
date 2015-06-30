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

Function \code{\link{classify}} uses a pre-learned TLBC model to classify accelerometer and/or GPS data with behavior labels. 

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
Kat Ellis <kellis@ucsd.edu>
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

# classify using a pre-trained model included in the package
classify(accelerometers="~/myStudy/HipGT3X+", GPS="~/myStudy/GPS.csv", modelName="CommuterCyclistsHipGT3X+GPS", saveDir="~/myStudy/CyclistHipGPSPredictions")

# train a new model
trainModel(annotations="~/myStudy/annotations.csv", accelerometers="~/myStudy/HipGT3X+", GPS="~/myStudy/GPS.csv", winSize=60, modelName="~/myStudy/myModel.RData")

# classify using a model computed yourself
classify(accelerometers="~/myStudy/HipGT3X+", GPS="~/myStudy/GPS.csv", modelName="~/myStudy/myModel.RData", saveDir="~/myStudy/myModelPredictions")

# compute the performance of a model on a dataset
calcPerformance(annotations="~/myStudy/HipGT3X+", predictions="~/myStudy/myModelPredictions", winSize=60)

# perform leave-one-out cross-validation on a dataset
looXval(annotations="~/myStudy/annotations.csv", accelerometers="~/myStudy/HipGT3X+", winSize=60, saveDir="~/myStudy/looXvalPredictions")

}

}
