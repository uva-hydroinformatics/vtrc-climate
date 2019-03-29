# This script is to analysis daily rainfall projection data to get 24-hour rainfall at specific rain guage location
# the available historical daily rainfall data were pulled and pre-processed from the NCDF 
# Statistical library used to generated the IDF values

#clear console, global environment and graphics
rm(list=ls())
graphics.off()
cat("\014")

# Load the required libraries
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

PDSRain <- function(Years, Threshold, rainfall_data){
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

get_rainfall_list <- function(dirc, RCP, station, model_list, start_yr, end_yr){
  
  selected_rainfall <- list()
  # loop through the model_list
  for(model in unlist(model_list)){
    print(model)
    rainfall_data <- read.csv(paste0(dirc,"/BiasCorrection/GCMs/", RCP, "/", station, "/", model, "/", model, "_BiasCorrected_WT01.csv"), header = TRUE)
    # Slice the Date into Year Month Day
    rainfall_data$Year <-  format(as.Date(rainfall_data$Date, format="%Y-%m-%d"),"%Y")
    rainfall_data$Month <-  format(as.Date(rainfall_data$Date, format="%Y-%m-%d"),"%m")
    rainfall_data$Day <-  format(as.Date(rainfall_data$Date, format="%Y-%m-%d"),"%d")
    
    # include only the available start and end full year (for observations do it manually by uncommenting the following line)
    YearBaseline <- seq(from=start_yr, to=end_yr, by=1)
    #YearBaseline <- seq(from=min(rainfall_data$Year), to=max(rainfall_data$Year), by=1)
    
    # creatre empty list to append the annual max. daily rainfall. Also this list can be used to append the output from PDS
    Max_DailyRain_Baseline <- list()
    
    # get the annual max. daily rainfall
    #year_selected <- MaxRain(YearBaseline)[1]
    #Max_DailyRain_Baseline <- MaxRain(YearBaseline)[2] 
    
    # get the partial duration series
    val <- 10
    year_selected <- PDSRain(YearBaseline, val, rainfall_data)[1]
    Max_DailyRain_Baseline <- PDSRain(YearBaseline, val, rainfall_data)[2] 
    
    # Convert the list to vector and sort it ascendingly 
    year_selected <- unlist(year_selected, use.name=FALSE)
    Max_DailyRain_Baseline <- unlist(Max_DailyRain_Baseline, use.name=FALSE)
    Max_DailyRain_Baseline <- sort(Max_DailyRain_Baseline, decreasing=TRUE) * 1.13 # Maybe this values need to be multiple by 1.13 (Maryland report)
    Max_DailyRain_Baseline <- Max_DailyRain_Baseline[c(1:length(YearBaseline))]
    
    # append the selected rainfall from this model to the selected rainfall list
    selected_rainfall <- append(selected_rainfall, Max_DailyRain_Baseline)
  }
  return(selected_rainfall)
  
}

################################# MAIN SCRIPT  ##################################################################

dirc <- "G:/Climate_Change_Project/RCMs_NA_Cordex_i/"

RCP <- "RCP45"

#rainfall_data <- read.csv(paste0(dirc,"CanESM2_CanRCM4_BiasCorrected_Historical.csv"), header = TRUE)
model_list <- read.csv(paste0(dirc, RCP, "/", "GCMs.csv"), header=FALSE)

Stations <- read.csv(file="G:\\Climate_Change_Project\\Statistical Analysis\\vtrc-climate\\statisticalAnalysis\\dataAnalysis\\modeledData\\NCDC_Selected_Raingauge.csv", header=TRUE)

Periods <- data.frame(
  period = c("Historical", "2045", "2085"),
  start_yr = c(1950, 2031, 2071),
  end_yr = c(2017, 2060, 2100)
)

years <- c("100yr", "50yr", "25yr", "10yr", "5yr", "2yr", "1.25yr", "1yr") 


# Start to estimate IDF for each statioon, each RCP, and each target year
for(period in Periods$period){
  print(period)
  start_yr <- Periods$start_yr[Periods$period==period]
  end_yr <- Periods$end_yr[Periods$period==period]
  
  IDF_Summary <- data.frame(matrix(ncol=11, nrow=0))
  colnames(IDF_Summary) <- c("Station_ID", "Lat", "Lon", years)  
  
  i=1
  for(station in Stations$ID){
    station <- toString(station)
    lon <- Stations$LONGITUDE[Stations$ID==station]
    lat <- Stations$LATITUDE[Stations$ID==station]
    print(station)

    # create empty list to append selected rainfall data for IDF analysis
    selected_rainfall <- list()
    selected_rainfall <- get_rainfall_list(dirc, RCP, station, model_list, start_yr, end_yr)
    selected_rainfall <- unlist(selected_rainfall, use.name=FALSE)
    returns_Baseline <- rain_24hrs_IDF(selected_rainfall)
    returns_Baseline_mm <- round(returns_Baseline,1) # values in inches
    returns_Baseline_inches <- round(returns_Baseline/25.4, 1) #from mm to inches
      
    output_lines <- c("24-hour Rainfall IDF generated from historical observation:", years, returns_Baseline_mm, returns_Baseline_inches)
    output_IDF <- file(paste0(dirc,"BiasCorrection/GCMs/", RCP, "/", station, "/","24Hour_Rainfall_",period,"_30yrs_ML_estimation_Onelist_PDSAtlas_WT50.csv"))
    writeLines(output_lines, output_IDF)
    close(output_IDF)
    
    station_IDF <- list(station, lat, lon)
    for(x in seq(1,length(returns_Baseline_inches),1)){
      station_IDF[[length(station_IDF)+1]] <- returns_Baseline_inches[x]
    }
    
    IDF_Summary[i,] <- station_IDF
    i = i+1
  }
  write.csv(IDF_Summary, file=paste0(dirc, "BiasCorrection/GCMs/", RCP, "/IDF_Summary", period, "_", RCP, ".csv"))
  
}







# Dataframe for ATLAS14 PDS IDF
# atlas14 <- data.frame(matrix(ncol = 2, nrow = 8))
# colnames(atlas14) <- c("return_period", "rainfall")
# atlas14$return_period <- c(500, 100, 50, 25, 10, 5, 2, 1.1)
# atlas14$rainfall <- c(12.6, 9.2, 7.95, 6.82, 5.5, 4.62, 3.57, 2.93)

# library(lmomco)
# non_exc_prob <- c(0.99, 0.98, 0.96, 0.9, 0.8, 0.5,0.2, 0.09090909)
# lmr  <- lmr2par(selected_rainfall, type="gev")
# CI <- genci.simple(lmr, n=100, f=non_exc_prob, nsim=200)
# x <- c(100, 50, 25, 10, 5, 2, 1)
# library(ggplot2)
# ggplot(CI, aes(x=1/(1-nonexceed), y=true/25.4)) + geom_line()+ geom_point(size = 1.5) + #geom_text(aes(label=round(true/25.4, 2)),hjust=0, vjust=-5) +
#   geom_point(aes(return_period, rainfall), atlas14, pch=3, size= 2, color = "red") +
#   geom_line(aes(x=1/(1-nonexceed), y=lwr/25.4), CI, linetype = "dashed") + 
#   geom_line(aes(x=1/(1-nonexceed), y=upr/25.4), CI, linetype = "twodash") +
#   geom_ribbon(aes(ymin=lwr/25.4,ymax=upr/25.4), fill="blue", alpha="0.3") +
#   scale_x_log10(breaks=c(1, 2,5, 10, 25, 50, 100), limits = c(1, 100)) +
#   scale_y_continuous() + 
#   labs(x = "Return Period (years)", y = "24-hr Rainfall (Inches)") +
#   ggtitle("Generated IDF rainfall from Historical Records at Norfolk Airport (90% Confidence Level)") +
#   theme_bw() + 
#   theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title = element_text(face = "bold.italic", color = "blue"))
# #theme(legend.position="top" )