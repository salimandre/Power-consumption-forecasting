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

We observe that both time series have **seasonality**. Although power consumption time series does not have **trend** while temperature time series has a slight trend.

<p align="center">
  <img src="img/eda/eda_exam-5-1.jpg" width="310" />
  <img src="img/eda/eda_exam-10-1.jpg" width="310" /> 
</p>

On **boxplots** we notice that for power consumption most of the measures on a day have low variance. Measures near 8 am and 5 pm have high variance though. It could be expected since it is when the people leave and go back home. In the contrary in the case of temperature we notice high variance for each measure of the day. Indeed temperature cannot be explained only by trend and seasonality. 

## Baseline models

<p align="center">
  <img src="img/baseline/baseline-1.png" width="310" />
</p>

The second baseline model looks like the following (alhough here it has been aggregated over hours instead of daily measures):

<p align="center">
  <img src="img/eda/eda_exam-4-1.jpg" width="310" />
</p>

On validation set we get the following forecasts:

<p align="center">
  <img src="img/baseline/baseline-2.jpg" width="310" />
</p>

On validation set we get the following results:

<p align="center">
  <img src="img/baseline/baseline-3.png" width="310" />
</p>

## Linear Regression models

## Exponential models

## ARIMA model

## NNAR model

