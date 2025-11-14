#Model build
library(Metrics)

#user input
mn <- 12; #month of year; 3, 6, 9, or 12 
colO <- 3; #column of desired variable in obs; 
colR <- 4; #column of desired variable in reanalysis data;

#1) load data files
REA = read.csv('era5_greenwood_subset2.csv',header = TRUE); #predictor - global temp data
OBS = read.csv('greenwood_stationdata.csv',header = TRUE); #predictand - greenwood
REAt <- REA %>% filter (year > 1980 & year < 2011)

hist(OBS$mean_temp)

#2) extract time series from both data sources for specific month
REAm = REAt[REAt[,2]==mn,colR]; #predictor at month m
OBSm = OBS[OBS[,2]==mn,colO]; #predictand  at month m 


#3) determine left-out observations
c95 = 2/sqrt(length(OBSm)); #95% confidence threshold for autocorr.
raut = acf(OBSm, plot=F); #ACF for observations
tau = length(which(raut$acf<0.9999 & raut$acf>= c95)); #estimate how many lags (years apart) your observations are still significantly correlated/ how “persistent” the signal is over time 
#a small or zero tau means that each year is mostly independent (happens with local air temp)
#large tau indicates strong year-to-year memory (SST anomalies)


#4) model training considering "leave-one-out" with persistence
alpha12=matrix(0,length(OBSm),2) #pre-define matrix for regression parameters
PREDm=0 #pre-define vector for predicted values

for (ii in 1:length(OBSm)) {
  
  io=1:30; #"io" for included observations; all at the beginning 
  exwin=(ii-tau):(ii+tau); #time window for exclusion
  cut = intersect(exwin,io); #common time points (if exwin <1 or >length(OBS))
  io=io[-cut]; #cut out    
  alpha12[ii,] = coef(lm(OBSm[io] ~ REAm[io])) #regression parameters: 1: offset, 2: slope
  PREDm[ii]=alpha12[ii,2]*REAm[ii] + alpha12[ii,1]; #prediction at month m
  
}

#5) calculate model fit with mean parameters
FITm = mean(alpha12[,2])*REAm + mean(alpha12[,1]) #apply mean parameters


#6) Illustrate obs. versus prediction
jpeg(filename="Dec_temps81-10.jpeg", width = 8, height = 5, units = "in", pointsize=12, res=1200)
p=plot(1:30, OBSm, type="l", 
       xaxs="i", yaxs="i",
       xlim=c(0, 30),
       ylim=c(min(c(OBSm,PREDm))-1, max(c(OBSm,PREDm))+1), 
       xlab="Years since 1980", 
       ylab="Air temperature (\u00B0C)", 
       main=paste0("Results for December, 1981-2010"),
       las=1)
lines(PREDm, col="sienna3")
legend("topleft", c("Observation","Prediction"), cex=0.7, lty=1, col=c(1,"sienna3"))
dev.off()

#re-name output for specific month
assign(paste0("PRED", 12),PREDm)
assign(paste0("OBS", 12),OBSm)
assign(paste0("FIT", 12),FITm)
assign(paste0("REAt", 12),REAm)

rm(PREDm,OBSm,REAm,FITm)

#Assessment 
#MSE 

rmse_mod12 <- rmse(OBSm,PREDm)
rmse_ref12 <- rmse(OBSm,REAm)

#Skill score
SSC12 <- 1-(rmse_mod12/rmse_ref12)

#correlation 
r12 <-cor(OBSm,REAm)


## Merge data  
FITt <- as.vector(t(cbind(FIT3, FIT6, FIT9, FIT12)))
OBSt  <- as.vector(t(cbind(OBS3, OBS6, OBS9, OBS12)))
REAt  <- as.vector(t(cbind(REA3, REA6, REA9, REA12)))

# Create a corresponding month index
months <- rep(c(3, 6, 9, 12), times = length(FIT3))

# Combine into a single data frames
seasonal_temps <- data.frame(
  Month = months,
  ERA5  = REAt,
  Fit = FITt1,
  Obs = OBSt
)
head(seasonal_temp)

#Summary metrics for months 3,6,9,12
seasonal_temps_metrics <- seasonal_temps %>%
  group_by(Month) %>%
  summarize(
    RMSE_fit  = rmse(Obs, Fit),
    RMSE_ERA5 = rmse(Obs, ERA5),
    Corr_fit  = cor(Obs, Fit),
    Corr_ERA5 = cor(Obs, ERA5)
  )
seasonal_temps_metrics

