# Readme.md

## Human Activity Recognition Using Smartphones Data Set


This is the Getting and Cleaning Data course assignment.
We are processing the files corresponding to the Human Activity Recognition Using Smartphones Data Set that was built from recordings of 30 subjects performing activities of dailiy living while carrying a waist-mounted smartphone with embedded inertial sensors.

More info at <http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones>.

To get the dataset needed to run the project R script download this file:
<https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip>

This script makes the following assumptions:
  * You the script is located on the same directory as the data.
  * You will change the working directory to the location of the script and data

In order to get the tidy data first change your working directory to where the data and the script are, then run the script in the same. It will save the results to a file called tidyData.txt


The script does the following:
  * Merges the training and the test sets to create one data set.
  * Extracts only the measurements on the mean and standard deviation for each measurement.
  * Uses descriptive activity names to name the activities in the data set
  * Labels the data set with descriptive variable names.
  * Creates a tidy data set with the average of each variable for each activity and each subject.