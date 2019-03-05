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

PDSRain <- function(Years){
  PDS_DailyRain <- list()
  PDS_selected_year <- list()
  threshold <- 81 # lower threshold for 1-year return period (mm) for the selected station
  dailyRain <- rainfall_data[rainfall_data$Rain>=threshold,]
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
  
  # non-exceedance probability for500, 100, 50, 25, 10, 5, 2, 1 years
  non_exc_prob <- c(0.998, 0.99, 0.98, 0.96, 0.9, 0.8, 0.5, 0.2, 0.099099099)
  
  ll <- Lmoments(selected_DailyRain)
  
  parameters <- par.GEV(ll[1],ll[2],ll[4])
  
  returns_Baseline <- invF.GEV(non_exc_prob,parameters$xi,parameters$alfa,parameters$k)
  return(returns_Baseline)
  
}

################################# MAIN SCRIPT  ##################################################################

dirc <- "J:/Projects/Bridges Climate Change Project/Statistical Analysis/Climate DATA/Climate Models/RCMs_NA_Cordex_i/BiasCorrection/GCMs/RCP85/Historical/"

# create empty list to append selected rainfall data for IDF analysis
selected_rainfall <- list()


#rainfall_data <- read.csv(paste0(dirc,"CanESM2_CanRCM4_BiasCorrected_Historical.csv"), header = TRUE)
model_list <- read.csv(paste0(dirc, "model_list.csv"), header=FALSE)

# loop through the model_list
for(model in unlist(model_list)){
  print(model)
  rainfall_data <- read.csv(paste0(dirc,model), header = TRUE)
  # Slice the Date into Year Month Day
  rainfall_data$Year <-  format(as.Date(rainfall_data$Date, format="%Y-%m-%d"),"%Y")
  rainfall_data$Month <-  format(as.Date(rainfall_data$Date, format="%Y-%m-%d"),"%m")
  rainfall_data$Day <-  format(as.Date(rainfall_data$Date, format="%Y-%m-%d"),"%d")
  
  # include only the available start and end full year (for observations do it manually by uncommenting the following line)
  #YearBaseline <- seq(from=1946, to=2017, by=1)
  YearBaseline <- seq(from=min(rainfall_data$Year), to=max(rainfall_data$Year), by=1)
  
  # creatre empty list to append the annual max. daily rainfall. Also this list can be used to append the output from PDS
  Max_DailyRain_Baseline <- list()
  
  # get the annual max. daily rainfall
  #year_selected <- MaxRain(YearBaseline)[1]
  #Max_DailyRain_Baseline <- MaxRain(YearBaseline)[2] 
  
  # get the partial duration series
  year_selected <- PDSRain(YearBaseline)[1]
  Max_DailyRain_Baseline <- PDSRain(YearBaseline)[2] 
  
  # Convert the list to vector and sort it ascendingly 
  year_selected <- unlist(year_selected, use.name=FALSE)
  Max_DailyRain_Baseline <- sort(unlist(Max_DailyRain_Baseline, use.name=FALSE), decreasing=FALSE)
  
  # append the selected rainfall from this model to the selected rainfall list
  selected_rainfall <- append(selected_rainfall, Max_DailyRain_Baseline)
}
selected_rainfall <- sort(unlist(selected_rainfall, use.name=FALSE), decreasing=FALSE) * 1.13 # Maybe this values need to be multiple by 1.13 as in the Maryland report

returns_Baseline <- rain_24hrs_IDF(selected_rainfall)
round(returns_Baseline,1) # values in inches
round(returns_Baseline,1)*0.03937007874015748 #from mm to inches

years <- c("500yr", "100yr", "50yr", "25yr", "10yr", "5yr", "2yr", "1.25yr", "1.11yr") 
output_lines <- c("24-hour Rainfall IDF generated from historical observation:", years, returns_Baseline)
output_IDF <- file(paste0(dirc,"24Hour_Rainfall_Historical_ML_estimation_Onelist_PDS81.csv"))
writeLines(output_lines, output_IDF)
close(output_IDF)
