# Statistical Analysis to Generate the Current ATLAS14 IDF Values Using Observation and the Future IDF Values Using the available RCMs  

This repository contains code for current VDOT project to Applying statistical methods to generate the Intensity Duration Frequency (IDF) of precipitation under existing and future climate conditions. 

## preprocessing
This folder includes three Python scripts to preprocess, clean-up, and prepare the data for the statitical analysis to generate the IDF values.

* NCDC_daily_observation_extraction.py	

This code is used to extract the daily rainfall time series from the NCDC datasets at each gauge location. This data is used by "/dataAnalysis/historicalObservation/Atlas14_IDF_historical_observation.R" to generate the current ATLAS 14 IDF values to validate the used methods before generating the future IDF values from the RCMs datasets.

* RCMs_data_extraction_time_series.py

This is a generic code to extract the rainfall data from the RCMs as time series CSV files. Each CSV file contains two columns, the first for date and the seocnd for rainfall values. These CSV files include both the modeled historical period (1950 - 2005) and the modeled future period (2006 - 2100) as one time series. This code uses Xarray library to extract the information from NetCDF files.

* Preprocessing_RCMs_Extracted_time_series_for_R_scripts.py

This code uses the output from the previous code (i.e. RCMs_data_extraction_time_series.py) and separate the date and rainfall values as teo separate CSV file and locate them and specific locations where the R codes need to find them. 

## dataAnalysis
This folder includes two folders: historicalObservation and modeledData. The first folder include the R script (Atlas14_IDF_historical_observation.R) that uses the historical observation to generate the current IDF values that we could compare to ATLAS 14 IDF values. The second folder includes R scripts required to genrate the future IDF values from the RCMs time series extract from the preprocessing step. Following description of the R scripts used to generate the future IDF values:

* BiasCorrection_RCMs.R

This code uses the quantile-quantile mapping available throught (https://github.com/SantanderMetGroup/downscaleR/wiki/bias-correction) to do the bias correction for historical and future RCMs simulations.

* Current_Future_IDF_Modeled_RCMs.R	

This R script uses the bias corrected historical and fututre RCMs data to generate the current and future IDF values. 
