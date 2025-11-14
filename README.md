# climate-downscaling

This project develops and evaluates a statistical downscaling model to estimate local air temperature from large-scale ERA5 reanalysis data using R.

## Data
- Local observed temperature: Environment and Climate Change Canada (Greenwood, NS)
- Large-scale predictor: ERA5 Reanalysis 2 m air temperature (ECMWF)
- Additional variables (optional): precipitation, snow depth, soil temperature

## Methods
- Preprocessing: time conversion, cleaning, QC, and aligning ERA5 with station observations  
- Temporal aggregation (hourly → daily)
- Model development: multiple linear regression between ERA5 temperature and local observations
- Validation using an independent holdout period (2011–2020)
- Performance metrics: RMSE, correlation, bias
- Visualization of observed vs predicted temperatures

## Results
- Downscaled temperature closely follows observed local temperature
- High correlation between ERA5 predictors and observed temperature  
- Validation period shows strong agreement and good model stability

## Tools
R (ncdf4, tidyverse, dplyr, tidyr, lubridate, ggplot2, data.table)