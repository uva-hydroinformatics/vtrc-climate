# This script is to analysis daily rainfall projection data to get 24-hour rainfall at specific rain guage location
# the available historical daily rainfall data were pulled and pre-processed from the NCDF 
# Statistical library used to generated the IDF values
library(nsRFA)
# Function for computing the annual daily maximum rainfall 
MaxRain <- function(Years){
  Max_DailyRain <- list()
  selected_year <- list()
  for(yr in Years){
    dailyRain <- rainfall_data[rainfall_data$Year==yr,]      
    Max_DailyRain[yr-min(Years)+1] <- max(dailyRain$Rain)
    selected_year[yr-min(Years)+1] <- max(dailyRain$Year)
  }
  return(list(selected_year, Max_DailyRain))
}
PDSRain <- function(Years, Threshold){
  PDS_DailyRain <- list()
  PDS_selected_year <- list()
  threshold <- Threshold # lower threshold for 1-year return period (mm) for the selected station
  dailyRain <- rainfall_data[rainfall_data$Rain>=threshold & rainfall_data$Year >= min(Years) & rainfall_data$Year <= max(Years),]
  selected_year <- dailyRain$Year
  Max_DailyRain <- dailyRain$Rain
  return(list(selected_year, Max_DailyRain))
}
######################################################################################################
#####calculate 24-hour rainfall intensity for different return periods based on given distribution####
rain_24hrs_IDF<- function(selected_DailyRain){
  # selected_DailyRain <- selected_DailyRain[c(4:102)]
  
  # Now using the MSCIaio2008 to apply thr L-moments distribution
  # Set criteria
  criteria <- "AIC"
  
  #MSCBaseline <- MSClaio2008(Max_DailyRain_Baseline, crit = criteria)
  
  # non-exceedance probability for 100, 50, 25, 10, 5, 2, 1.25, 1 years
  non_exc_prob <- c(0.99, 0.98, 0.96, 0.9, 0.8, 0.5,0.2, 0.09090909)
  
  ll <- Lmoments(selected_DailyRain)
  
  parameters <- par.GEV(ll[1],ll[2],ll[4])
  
  returns_Baseline <- invF.GEV(non_exc_prob,parameters$xi,parameters$alfa,parameters$k)
  return(returns_Baseline)
  
}
################################# MAIN SCRIPT  ##################################################################
dirc <- "Z:/Projects/Bridges Climate Change Project/Statistical Analysis/Climate DATA/Climate Models/RCMs_NA_Cordex_i/BiasCorrection/GCMs/RCP85/Projection/"
# create empty list to append selected rainfall data for IDF analysis
selected_rainfall <- list()
# create a list to append the IDF for each model individual
IDF_output <- data.frame(matrix(ncol = 9, nrow = 0))
#rainfall_data <- read.csv(paste0(dirc,"CanESM2_CanRCM4_BiasCorrected_Historical.csv"), header = TRUE)
model_list <- read.csv(paste0(dirc, "model_list_wetThrehold40.csv"), header=FALSE)
# loop through the model_list
for(model in unlist(model_list)){
  print(model)
  rainfall_data <- read.csv(paste0(dirc,model), header = TRUE)
  # Slice the Date into Year Month Day
  rainfall_data$Year <-  format(as.Date(rainfall_data$Date, format="%Y-%m-%d"),"%Y")
  rainfall_data$Month <-  format(as.Date(rainfall_data$Date, format="%Y-%m-%d"),"%m")
  rainfall_data$Day <-  format(as.Date(rainfall_data$Date, format="%Y-%m-%d"),"%d")
  
  # include only the available start and end full year (for observations do it manually by uncommenting the following line)
  YearBaseline <- seq(from=2061, to=2100, by=1)
  #YearBaseline <- seq(from=min(rainfall_data$Year), to=max(rainfall_data$Year), by=1)
  
  # creatre empty list to append the annual max. daily rainfall. Also this list can be used to append the output from PDS
  Max_DailyRain_Baseline <- list()
  
  # get the annual max. daily rainfall
  #year_selected <- MaxRain(YearBaseline)[1]
  #Max_DailyRain_Baseline <- MaxRain(YearBaseline)[2] 
  
  # get the partial duration series
  val <- 10
  year_selected <- PDSRain(YearBaseline, val)[1]
  Max_DailyRain_Baseline <- PDSRain(YearBaseline, val)[2] 
  
  # Convert the list to vector and sort it ascendingly 
  year_selected <- unlist(year_selected, use.name=FALSE)
  
  selected_rainfall <- unlist(Max_DailyRain_Baseline, use.name=FALSE)
  selected_rainfall <- sort(selected_rainfall, decreasing=TRUE) * 1.13 # Maybe this values need to be multiple by 1.13 as in the Maryland report
  selected_rainfall <- selected_rainfall[c(1:length(YearBaseline))]
  
  returns_Baseline <- rain_24hrs_IDF(selected_rainfall)
  IDF_output <- rbind(IDF_output, returns_Baseline)
  
}
colnames(IDF_output) <- c("100yr", "50yr", "25yr", "10yr", "5yr", "2yr", "1.25yr", "1yr")
IDF_output["Models"] <- as.character(unlist(model_list))
model_max <- c(max(IDF_output$"100yr"), 
               max(IDF_output$"50yr"), max(IDF_output$"25yr"), 
               max(IDF_output$"10yr"), max(IDF_output$"5yr"), 
               max(IDF_output$"2yr"), max(IDF_output$"1.25yr"), 
               max(IDF_output$"1yr"), "Maximum")
model_median <- c(median(IDF_output$"100yr"), 
                  median(IDF_output$"50yr"), median(IDF_output$"25yr"), 
                  median(IDF_output$"10yr"), median(IDF_output$"5yr"), 
                  median(IDF_output$"2yr"), median(IDF_output$"1.25yr"), 
                  median(IDF_output$"1yr"), "Median")
model_min <- c(min(IDF_output$"100yr"), 
               min(IDF_output$"50yr"), min(IDF_output$"25yr"), 
               min(IDF_output$"10yr"), min(IDF_output$"5yr"), 
               min(IDF_output$"2yr"), min(IDF_output$"1.25yr"), 
               min(IDF_output$"1yr"), "Minimum")
model_max_inches <- c(max(IDF_output$"100yr")/25.4, 
                      max(IDF_output$"50yr")/25.4, max(IDF_output$"25yr")/25.4, 
                      max(IDF_output$"10yr")/25.4, max(IDF_output$"5yr")/25.4, 
                      max(IDF_output$"2yr")/25.4, max(IDF_output$"1.25yr")/25.4, 
                      max(IDF_output$"1yr")/25.4, "Maximum inches")
model_median_inches <- c(median(IDF_output$"100yr")/25.4, 
                         median(IDF_output$"50yr")/25.4, median(IDF_output$"25yr")/25.4, 
                         median(IDF_output$"10yr")/25.4, median(IDF_output$"5yr")/25.4, 
                         median(IDF_output$"2yr")/25.4, median(IDF_output$"1.25yr")/25.4, 
                         median(IDF_output$"1yr")/25.4, "Median inches")
model_min_inches <- c(min(IDF_output$"100yr")/25.4, 
                      min(IDF_output$"50yr")/25.4, min(IDF_output$"25yr")/25.4, 
                      min(IDF_output$"10yr")/25.4, min(IDF_output$"5yr")/25.4, 
                      min(IDF_output$"2yr")/25.4, min(IDF_output$"1.25yr")/25.4, 
                      min(IDF_output$"1yr")/25.4, "Minimum inches")
IDF_output <- rbind(IDF_output, model_max)
IDF_output <- rbind(IDF_output, model_median)
IDF_output <- rbind(IDF_output, model_min)
IDF_output <- rbind(IDF_output, model_max_inches)
IDF_output <- rbind(IDF_output, model_median_inches)
IDF_output <- rbind(IDF_output, model_min_inches)
# write output to csv
write.csv(IDF_output, file=paste0(dirc, "24Hour_Rainfall_2100_LM_estimation_Median_PDSAtlas_WT40_W40.csv"))
# plot the results
return_period <- c(100, 50, 25, 10, 5, 2, 1.25, 1)
# convert factor to numeric
model_max_inches <- as.numeric(model_max_inches[1:8])
model_median_inches <- as.numeric(model_median_inches[1:8])
model_min_inches <- as.numeric(model_min_inches[1:8])
plot(return_period, model_max_inches, xlab="Return Period (years)",
     ylab="24-hr Rainfall (Inches)", log = "x", main = "Modeled current condition (WT = 30mm)", pch = 2, ylim=c(2, 16)) 
lines(return_period, model_max_inches, lty = 6, lwd = 2) 
points(return_period, model_median_inches)
lines(return_period, model_median_inches, lty=1, lwd = 2)
points(return_period, model_min_inches, pch = 0) 
lines(return_period, model_min_inches, lty = 2, lwd = 2) 
legend(1, 14, legend=c("Max", "Median", "Min"), lty=c(6, 1, 2), cex=0.8)