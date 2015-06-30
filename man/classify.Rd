\name{classify}
\alias{classify}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Function to classify accelerometer and/or GPS data
}
\description{
Classifies data into behavior categories using a pre-computed two-level model.
}
\usage{
classify(accelerometers = NULL, GPS = NULL, modelName, saveDir, names = NULL)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{accelerometers}{
(Optional) Path to a directory (or list of directories) containing actigraph accelerometer data files. Accelerometer data files should be csv files output in "raw" format by ActiLife (without timestamps), and named by the participant identifier, \emph{e.g.}, Participant01.csv.

Or, path to a directory (or list of directories) containing previously computed accelerometer features, \emph{i.e.}, computed by the function \emph{sensorsToFeatures}.
}
  \item{GPS}{
(Optional) Path to a PALMS-processed GPS data file (or a directory containing GPS data files). GPS data files should be in csv format with the following fields: \emph{identifier, dateTime, speed, ele, elevationDelta, lat, lon, nsatView, snrView}. \emph{identifier} should be the participant identifier, \emph{e.g.} Participant01. If \emph{GPS} is a path to a directory, each file in the directory should correspond to a participant, and the file name should be the participant identifier, \emph{e.g.}, Participant01.csv.

Or, path to a directory containing previously computed GPS features, \emph{i.e.}, computed by the function \emph{sensorsToFeatures}.
}
  \item{modelName}{

Included in the package are pre-trained models from three datasets:

1. Validation study: Two research assistants wore an Actigraph GT3X+ on the hip and a GPS. The research assistants performed a specified sequence of activities around San Diego county. We have used their data to train models to recognize the following behaviors: riding in a car, standing, walking, riding in a bus, sitting, some movement, bicycling. There are three pre-trained models:

 - \emph{ValStudyHipGT3X+GPS}
 
 - \emph{ValStudyHipGT3X+}
 
 - \emph{ValStudyGPS}

2. Free-living commuter cyclist study: Forty commuter cyclists wore an Actigraph GT3x+ on the hip and a GPS and performed their usual daily activities for 2-3 days. Their behaviors were annotated from images collected from a wearable camera. We have used their data to train models to recognize the following behaviors: sitting, standing still, standing moving, walking/running, bicycling and riding in a vehicle. There are three pre-trained models:

 - \emph{CommuterCyclistsGPS}

 - \emph{CommuterCyclistsHipGT3X+GPS}

 - \emph{CommuterCyclistsHipGT3X+}

3. Free-living overweight women study: Thirty-six overweight women wore an Actigraph GT3X+ on the hip and wrist and a GPS and performed their usual daily activities for 5-7 days. Their behaviors were annotated from images collected from a wearable camera. We have used their data to train models to recognize the following behaviors: sitting, standing still, standing moving, walking/running, bicycling and riding in a vehicle. There are five pre-trained models:

 - \emph{OverweightWomenWristGT3X+GPS}

 - \emph{OverweightWomenHipGT3X+GPS}

 - \emph{OverweightWomenWristGT3X+}

 - \emph{OverweightWomenHipGT3X+}

 - \emph{OverweightWomenGPS}

Alternatively, you can provide the path to another model you trained yourself, \emph{i.e.}, the argument \emph{modelName} in the function \emph{trainModel}.

}
  \item{saveDir}{
Path to a directory where predictions will be saved. Predictions will be saved in files named <\emph{identifier}>.csv with two fields: \emph{timestamp,prediction}.
}
  \item{names}{
(Optional) List of participant identifiers to use.
}
}
\author{
Kat Ellis
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

%%
\examples{
\dontrun{

# use a pre-trained model to classify hip accelerometer data
classify(accelerometers="~/myStudy/HipGT3X+", modelName="OverweightWomenHipGT3X+", saveDir="~/myStudy/predictions")

# use a model you trained on your own
classify(accelerometers="~/myStudy/HipGT3X+", GPS="~/myStudy/GPS", modelName="~/myStudy/myModel.RData", saveDir="~/myStudy/predictions")
}
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ ~kwd1 }
\keyword{ ~kwd2 }% __ONLY ONE__ keyword per line
