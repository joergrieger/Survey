# Survey

# Overview

Survey is a collection of R routines to download data from the Survey of Professional Forecasters of the Federal Reserve Bank of Philadelphia. It also contains functions to calculate several dispersion measures based on individual forecasts.

## Installation

To install this package you need the devtools package. If you don't have the devtools package, you can install it with

    install.packages("devtools")
    
Once you have installed the devtools package you can install the Survey package with

    library('devtools')
    devtools::install_github('joergrieger/bvar')

## How to use it

To download *mean*, *median* and *mean growth* forecasts you use the function **downloadSPF(survey,type)**, where **survey** is the survey and **type** is the type of survey, i.e. mean, median and mean growth. It can download the three mentioned types of forecasts from *U.S. Business Indicators*, *Real GDP and its components* and *CPI and PCE Inflation*. The function returns a *survey* object that stores information about the which survey and what type of survey you have downloaded as well as the series. For example, the function call

    tmp <- downloadSPF(survey = "NGDP", type = "growth")
    
downloads the mean growth forecasts of nominal GDP.

If you are interested in individual forecasts, you use the function **individualSPF(survey,variable)** where **survey** is the name of the survey, e.g. NGDP, and **variable** is the variable within the survey, e.g. NGDP1. As *downloadSPF()* *individualSPF()* returns a survey object. For example, the function call

    tmp <- individualSPF(survey = "NGDP", variable = "NGDP1")
    
downloads the nowcasts of Nominal GDP.

The function **dispersion(surveyObj,method)** calculates a dispersion measures based on individual forecasts. 3 different dispersion measures can be calculated:

    1 = Interquartile Range of Level-forecasts
    2 = Standard deviation of Level-forecasts
    3 = Interquartile Range of Q/Q growth forecasts

The following code downloads the CPI-Nowcasts and uses the standard deviation as a dispersion measure

    tmp <- individualSPF(survey = "CPI", variable = "CPI1")
    dsp <- dispersion(tmp,method = 2)

## The survey object

The functions *downloadSPF()* and *individualSPF()* both return a survey object. This is an S3-object with the following fields:

    survey = Name of the survey
    type = mean, median, growth, or individual
    variable = for individual SPFs which variable is contained in the time series, for mean, median, or growth the value will be "all"
    series = a time series object with the downloaded series.
