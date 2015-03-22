# CodeBook.md

## Human Activity Recognition Using Smartphones Data Set

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals were captured at a constant rate of 50 Hz. Then they were filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. Similarly, the acceleration signal was then separated into body and gravity acceleration signals BodyAcc-XYZ and GravityAcc-XYZ using another low pass Butterworth filter with a corner frequency of 0.3 Hz. 

Subsequently, the body linear acceleration and angular velocity were derived in time to obtain Jerk signals (BodyAccJerk-XYZ and BodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (BodyAccMag, GravityAccMag, BodyAccJerkMag, BodyGyroMag, BodyGyroJerkMag). 

Finally a Fast Fourier Transform (FFT) was applied to some of these signals producing BodyAcc-XYZ, BodyAccJerk-XYZ, BodyGyro-XYZ, BodyAccJerkMag, BodyGyroMag, BodyGyroJerkMag. 

This dataset contains the average for each of the mean and standard deviations for the measurements provided in the original dataset grouped by subject and activity. 

These are the variables contained in the output dataset.

Subject:
The subject Id.


Activity:
The activity the subject was doing. It can be one of the following values:
  - Walking
  - Walking Upstairs
  - Walking Downstairs
  - Sitting
  - Standing
  - Laying
  
  
The following variables contain the average for each of these measurements by subject and activity. 

###Time domain signals

- TimeBodyAccMeanX
- TimeBodyAccMeanY
- TimeBodyAccMeanZ
- TimeBodyAccStdDevX
- TimeBodyAccStdDevY
- TimeBodyAccStdDevZ
- TimeGravityAccMeanX
- TimeGravityAccMeanY
- TimeGravityAccMeanZ
- TimeGravityAccStdDevX
- TimeGravityAccStdDevY
- TimeGravityAccStdDevZ
- TimeBodyAccJerkMeanX
- TimeBodyAccJerkMeanY
- TimeBodyAccJerkMeanZ
- TimeBodyAccJerkStdDevX
- TimeBodyAccJerkStdDevY
- TimeBodyAccJerkStdDevZ
- TimeBodyGyroMeanX
- TimeBodyGyroMeanY
- TimeBodyGyroMeanZ
- TimeBodyGyroStdDevX
- TimeBodyGyroStdDevY
- TimeBodyGyroStdDevZ
- TimeBodyGyroJerkMeanX
- TimeBodyGyroJerkMeanY
- TimeBodyGyroJerkMeanZ
- TimeBodyGyroJerkStdDevX
- TimeBodyGyroJerkStdDevY
- TimeBodyGyroJerkStdDevZ
- TimeBodyAccMagMean
- TimeBodyAccMagStdDev
- TimeGravityAccMagMean
- TimeGravityAccMagStdDev
- TimeBodyAccJerkMagMean
- TimeBodyAccJerkMagStdDev
- TimeBodyGyroMagMean
- TimeBodyGyroMagStdDev
- TimeBodyGyroJerkMagMean
- TimeBodyGyroJerkMagStdDev

###Frequency domain signals
- FrequencyBodyAccMeanX
- FrequencyBodyAccMeanY
- FrequencyBodyAccMeanZ
- FrequencyBodyAccStdDevX
- FrequencyBodyAccStdDevY
- FrequencyBodyAccStdDevZ
- FrequencyBodyAccJerkMeanX
- FrequencyBodyAccJerkMeanY
- FrequencyBodyAccJerkMeanZ
- FrequencyBodyAccJerkStdDevX
- FrequencyBodyAccJerkStdDevY
- FrequencyBodyAccJerkStdDevZ
- FrequencyBodyGyroMeanX
- FrequencyBodyGyroMeanY
- FrequencyBodyGyroMeanZ
- FrequencyBodyGyroStdDevX
- FrequencyBodyGyroStdDevY
- FrequencyBodyGyroStdDevZ
- FrequencyBodyAccMagMean
- FrequencyBodyAccMagStdDev
- FrequencyBodyAccJerkMagMean
- FrequencyBodyAccJerkMagStdDev
- FrequencyBodyGyroMagMean
- FrequencyBodyGyroMagStdDev
- FrequencyBodyGyroJerkMagMean
- FrequencyBodyGyroJerkMagStdDev