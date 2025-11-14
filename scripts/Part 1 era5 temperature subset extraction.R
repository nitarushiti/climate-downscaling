# NCDF temp subset extraction 
library(ncdf4)
library(dplyr)
library(tidyr)
#Using ERA5 reanalysis data for air temperature
nc_era <- nc_open("era5_t2m.nc")

#Get variables
lat <- ncvar_get(nc_era, varid="lat")
lon <- ncvar_get(nc_era, varid="lon")
airt <- ncvar_get(nc_era, varid="t2m")
time <- ncvar_get(nc_era, varid="time")
time_units <- ncatt_get(nc_era, "time", "units")$value

# Convert time units from "months since YYYY-MM-DD" 
library(lubridate)
library(stringr)
origin_date_str <- unlist(strsplit(time_units, " "))[3]
origin_date <- as.Date(origin_date_str)

dates <- origin_date %m+% months(time)
# Now parse it safely
origin_date <- ymd(origin_date_str)
origin_date
# Convert each time value to actual date
dates <- origin_date %m+% months(time)

#define target lat and lon for greenwood, ns 
target_lat <- 45
target_lon <- 295
lat_indices <- which(lat == target_lat)
lon_indices <- which(lon == target_lon)
#Get subset of temp data for greenwood
data_variable <- ncvar_get(nc_era, "t2m",
                           start = c(min(lon_indices), min(lat_indices), 1),
                           count = c(length(lon_indices), length(lat_indices), -1))
#convert temp from kelvin to celsius
tempc<- data_variable - 273.15

#Create dataframe to hold new subset 
greenwood_airt <- data.frame(Month = dates, Temperature = tempc)

#Split date column into year, month, day 
greenwood_airt <- separate(greenwood_airt, Month, c("year", "month", "day"), sep = "-", remove = FALSE)
greenwood_airt <- select(greenwood_airt, -Month)

write.csv(greenwood_airt, "era5_greenwood_subset.csv", row.names = FALSE)