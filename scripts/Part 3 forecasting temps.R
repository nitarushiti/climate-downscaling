# Model in prediction mode 

#Load necessary data
OBS_2011 <- read.csv("en_climate_monthly_NS_8202000_2011-2017_P1M.csv", header = TRUE) #"future" station data
#Rename observed column to match structure
colnames(OBS_2011)[colO] <- "OBS"
OBS_2011 <- OBS_2011 %>%
  rename(year = Year,
         month = Month)

#Intercept and slope per left out year, for a given month (ie mn=12)
mean_intercept <- mean(alpha12[,1], na.rm = TRUE) #mean intercept 
mean_slope <- mean(alpha12[,2], na.rm = TRUE) #mean slope 

# Apply to ERA5 after 2010 for that month
future_m <- REA %>%
  filter(year > 2010, month == mn) %>%
  transmute(year, month, ERA5 = t2m_C, PRED=mean_slope*t2m_C + mean_intercept)

#Join observations with predicted values (PRED) and original ERA5
merged_temp_m12 <- merge(future_m, OBS_2011[,c("year","month","OBS")], 
                         by=c("year","month"), all=FALSE)

#Calculate RMSE for downscaled model
rmse_mod_forecast <- rmse(merged_temp_m12$PRED, merged_temp_m12$OBS)
#Calculate RMSE for raw ERA5
rmse_ref_forecast <- rmse(merged_temp_m12$ERA5, merged_temp_m12$OBS)

#Skill score relative to ERA5
SSC_forecast <- 1 - (rmse_mod_forecast / rmse_ref_forecast)

#Correlation
r_forecast <- cor(merged_temp_m12$OBS, merged_temp_m12$PRED)

jpeg(filename="Dec_temps2011-17.jpeg", width = 5, height = 5, units = "in", pointsize=12, res=1200)
plot(merged_temp_m12$OBS, merged_temp_m12$PRED,
     xlab="Observed temperature (°C)",
     ylab="Predicted temperature (°C)",
     main="Holdout Validation December (2011–2017)",
     pch=16, col="#CB2314", las=1)
abline(0,1,col="grey40",lty=2,lwd=2)
dev.off()
