# Power Consumption Forecasting

## Goal

Given two historics of measures produced by a sensor which performs **96 daily measures**:
- **power consumption** from 01/01/2010 to 16/02/2010
- **temperature** from 01/01/2010 to 17/02/2010

we had to **forecast** power consumption on the 17/02/2010.

## Exploratory Data Analysis

We took **46 days as training set** and **1 day as validation set**, measures of the 16/02/2010. Taking 1 day of data as validation may be a bit suspicious, probably 2 days would have been better. Nevertheless our idea was that since we only had to forecast one day, that is 96 measures of the same day, we should take a validation set which would most likely look similar as the test set.  

<p float="left">
  <img src="img/eda/eda_exam-1-1.jpg" width="210" />
  <img src="img/eda/eda_exam-6-1.jpg" width="210" /> 
  <img src="img/eda/eda_exam-2-1.jpg" width="210" />
  <img src="img/eda/eda_exam-7-1.jpg" width="210" /> 
</p>


<p float="left">
  <img src="img/eda/eda_exam-5-1.jpg" width="210" />
  <img src="img/eda/eda_exam-10-1.jpg" width="210" /> 
</p>

## Baseline models

## Linear Regression models

## Exponential models

## ARIMA model

## NNAR model

