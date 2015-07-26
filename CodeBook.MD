---
title: "CodeBook.MD"
author: "Dennise"
date: "Sunday, July 26, 2015"
output: html_document
---

This document describes the variable used in the script ```run_analysis.R```

The variable description are as below.

The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix 't' to denote time) were captured at a constant rate of 50 Hz. Then filtered using a median filter and a 3rd order low pass Butterworth filter with a corner frequency of 20 Hz to remove noise. 

The acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) using another low pass Butterworth filter with a corner frequency of 0.3 Hz.

The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). 

The magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

A Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the 'f' to indicate frequency domain signals).

These signals were used to estimate variables of the feature vector for each pattern:

'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
```
-- tBodyAcc-XYZ 
-- tGravityAcc-XYZ 
-- tBodyAccJerk-XYZ 
-- tBodyGyro-XYZ 
-- tBodyGyroJerk-XYZ 
-- tBodyAccMag 
-- tGravityAccMag 
-- tBodyAccJerkMag 
-- tBodyGyroMag 
-- tBodyGyroJerkMag 
-- fBodyAcc-XYZ 
-- fBodyAccJerk-XYZ 
-- fBodyGyro-XYZ 
-- fBodyAccMag 
-- fBodyAccJerkMag 
-- fBodyGyroMag 
-- fBodyGyroJerkMag
```

mean(): Mean value 
std(): Standard deviation