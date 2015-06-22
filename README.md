# TLBC: Two-level behavior classification R package

## Installation

Download and unzip the folder. Install the package by running the following command in R:
```
install.packages("path/to/TLBC", type="source", repos=NULL)
```
## Formatting the data

Each participant should have a unique participant identifier which will be used to match data between devices (if applicable). You should have a separate directory containing data from each device, and a separate direcory for any annotation files.

### Accelerometer files

Accelerometer data files should be stored in their own directory and should be csv files output in "raw" format by Actilife (without timestamps) named by a unique participant identifier (*e.g.*, "Participant01.csv"). If your study has multiple accelerometers, each accelerometer placement (*e.g.*, hip or waist) should be a separate directory.

### GPS Files

GPS data files should be stored in their own directory and should be in csv format with the following fields: *identifier, dateTime, speed, ele, elevationDelta, lat, lon, nsatView, snrView.* These fields can be exported by UCSD's PALMS GPS cleaning and processing software, or you can manually create the data file with necessary fields. The *identifier* field should be the unique participant identifier (*e.g.*, "Participant01") - if multiple devices (*e.g.* accelerometer and GPS) are being used, it is essential that the participant identifiers match exactly across devices in order to match the data sources correctly. You can either include all GPS in a single file, or have one file for each participant. If using a separate file for each participant, the file name should be the unique participant identifier (*e.g.*, "Participant01.csv").

### Annotation Files

Annotation files are only necessary if you would like to train your own behavior classifier (using the *trainModel* function). Annotation files should be stored in their own directory and in csv format with the following fields: *identifier, StartDateTime, EndDateTime, behavior*. The *idenfitier* field should be the unique participant identifier (*e.g.*, "Participant01"). The *StartDateTime* and *EndDateTime* fields should be the start time and end time of the behavior, and can be formatted as either *mm/dd/yyyy HH:MM:SS* or *yyyy-mm-dd HH:MM:SS*. The *behavior* field is a string naming the behavior. You can either include all annotations in a single file, or have one file for each participant. If using a separate file for each participant, the file name should be the participant identifier (*e.g.*, "Participant01.csv").

## Classifying data

Data from one or more devices can be classified with behavior labels using the *classify* function. The package contains pre-trained models that have been trained on three UCSD datasets: 
1. Validation Study: Two research assistants wore an Actigraph GT3X+ on the hip and a GPS. The research assistants performed a specified sequence of activities around San Diego county including Pre-trained models 

Please see the documentation for the *classify* function for more details.

