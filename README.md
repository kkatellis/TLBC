# TLBC: Two-Level behavior classification R package

## About

Two-Level behavior classification (TLBC) is an R package for classifying human behaviors from accelerometer and/or GPS data. The package contains functions for training and applying two-level random forest and hidden Markov models.

The package has been developed for csv data from Actigraph accelerometers (please export in RAW format, without timestamps), and/or GPS data processed by the PALMS GPS cleaning software.

The TLBC classifier uses five behavior labels:

* Sitting
* Standing Still
* Standing Moving
* Walking/Running
* Bicycling
* Vehicle

## Installation

Download and unzip the folder. Install the package by running the following command in R:
```
install.packages("path/to/TLBC", type="source", repos=NULL)
```

Load the package by running the following command in R:

```
library(TLBC)
```

## Formatting the data

Each participant should have a unique participant identifier which will be used to match data between devices (if applicable). You should have a separate directory containing data from each device, and a separate direcory for any annotation files.

### Accelerometer files

Accelerometer data files should be stored in their own directory and should be csv files output in "raw" format by Actilife (without timestamps) named by a unique participant identifier (*e.g.*, "Participant01.csv"). If your study has multiple accelerometers, each accelerometer placement (*e.g.*, hip or waist) should be a separate directory.

### GPS Files

GPS data files should be stored in their own directory and should be in csv format with the following fields: *identifier, dateTime, speed, ele, elevationDelta, lat, lon, nsatView, snrView.* These fields can be exported by UCSD's <a href = "https://ucsd-palms-project.wikispaces.com">PALMS</a> GPS cleaning and processing software, or you can manually create the data file with necessary fields. The *identifier* field should be the unique participant identifier (*e.g.*, "Participant01") - if multiple devices (*e.g.* accelerometer and GPS) are being used, it is essential that the participant identifiers match exactly across devices in order to match the data sources correctly. You can either include all GPS in a single file, or have one file for each participant. If using a separate file for each participant, the file name should be the unique participant identifier (*e.g.*, "Participant01.csv").

### Annotation Files

Annotation files are only necessary if you would like to train your own behavior classifier (using the *trainModel* function). Annotation files should be stored in their own directory. You can either represent annotations in bout-level format or instance-level format. 

If using bout-level format, annotation files should be in csv format with the following fields: *identifier, StartDateTime, EndDateTime, behavior*. The *idenfitier* field should be the unique participant identifier (*e.g.*, "Participant01"). The *StartDateTime* and *EndDateTime* fields should be the start time and end time of the behavior, and can be formatted as either *mm/dd/yyyy HH:MM:SS* or *yyyy-mm-dd HH:MM:SS*. The *behavior* field is a string naming the behavior. 

If using instance-level format, the time steps must match the window size of the classifier you are using. Instance-level annotation files should be in csv format with the following fields: *identifier, timestamp, behavior*. The *idenfitier* field should be the unique participant identifier (*e.g.*, "Participant01"). The *timestamp* field should be the timestamp of the behavior, and should be formatted as *yyyy-mm-dd HH:MM:SS*. The *behavior* field is a string naming the behavior. 

You can either include all annotations in a single file, or have one file for each participant. If using a separate file for each participant, the file name should be the participant identifier (*e.g.*, "Participant01.csv").

## Classifying data

Data from one or more devices can be classified with behavior labels using the *classify* function. You can either use a classifier you have trained yourself using the *trainModel* function, or one that has been pre-trained on one of our datasets. Pre-trained models that have been trained on three UCSD datasets are <a href = "http://ieng9.ucsd.edu/~kellis/TLBC.html"> available for download</a>.

Please see the documentation for the *classify* function for more details.

## Training models

You can train a classifier from your own data using the *trainModel* function. This requires data from either an accelerometer or GPS (or both) along with matched annotations of behaviors. Please see the documentation for the *trainModel* function for more details.

## Calculating performance

If you have annoations for your data, you can check the performance of a classifier on your dataset by using the *calcPerformance* function. Please see the documentation for more details.
