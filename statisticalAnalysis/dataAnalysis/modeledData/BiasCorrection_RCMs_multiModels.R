#clear console, global environment and graphics
rm(list=ls())
graphics.off()
cat("\014")

library(loadeR)
library(downscaleR)
library(visualizeR)
library(nsRFA)
library(gsubfn)
library(reshape2)

years_obs <- 1950:2005
years_proj <- 1950:2100

lonLim <- c(-85, -70)
latLim <- c(30, 40)

############################################################################################################
################################## Get Raingauge Observations ##############################################
dirc <- "G:\\Climate_Change_Project\\RCMs_NA_Cordex_i"
VALUE_DATA_Pr <- loadStationData(dataset = paste0(dirc,"\\BiasCorrection\\Observations\\ObservationInput"), 
                              var = "precip", years = years_obs, lonLim = lonLim, latLim=latLim, units="mm")
y <- VALUE_DATA_Pr

Stations <- read.csv(file="G:\\Climate_Change_Project\\Statistical Analysis\\vtrc-climate\\statisticalAnalysis\\dataAnalysis\\modeledData\\NCDC_Selected_Raingauge.csv", header=TRUE)

##################################################
#### RCP define####
RCP <- "RCP85"
GCMs <- read.csv(paste0(dirc, "\\", RCP, "\\GCMs.csv"), header=FALSE)

###########################################################################################################
########################## Build Function to get bias correction rainfall #################################
bias_correction <- function(dirc, RCP, GCM, Stations){
  
  # 
  time <- read.csv(paste0(dirc, "\\Rainfall_Projection_TimeSeries\\", "USC00441136", "\\", RCP, "\\", GCM, "\\time_day.csv"), header = FALSE, sep=",", 
                   col.name = c("Year", "Mon", "Day"), colClasses = c("integer", "integer", "integer"))
  time["YYYYMMDD"] <- with(time, sprintf("%d%02d%02d", Year, Mon, Day))
  
  for(station in Stations$ID){
    station <- toString(station)
    pr <- read.csv(paste0(dirc, "\\Rainfall_Projection_TimeSeries\\", station, "\\", RCP, "\\", GCM, "\\pr_day.csv"), 
                   header = FALSE, sep=",", col.names = c("Rain"), colClasses = c("double"))
    is.num <- sapply(pr, is.numeric)
    pr[is.num] <- lapply(pr[is.num], round, 2)
    
    time[station] <- pr["Rain"]
  }
  
  rain_proj <- time
  
  drops <- c("Year", "Mon", "Day")
  rain_proj <- rain_proj[, !(names(rain_proj) %in% drops)]
  
  precip_file <- paste0(dirc, "\\BiasCorrection\\GCMs\\temp\\precip.txt")
  
  if(exists(precip_file)) {rm(precip_file)}
  
  write.table(rain_proj, precip_file
              , quote =FALSE, sep=",",row.names=FALSE)
  
  station_df <- data.frame(matrix(ncol = 4, nrow = 0))
  colnames(station_df) <- c("station_id", "name", "longitude", "latitude")
  
  i=1
  for(station in Stations$ID){
    station <- toString(station)
    lon <- Stations$LONGITUDE[Stations$ID==station]
    lat <- Stations$LATITUDE[Stations$ID==station]
    station_df[i,] <- list(station, "NIA", lon, lat)
    i = i+1
  }
  
  station_file <- paste0(dirc, "\\BiasCorrection\\GCMs\\temp\\stations.txt")
  if(exists(station_file)) {rm(station_file)}
  
  # 
  write.table(station_df, station_file
              , quote =FALSE, sep=",",row.names=FALSE)
  
  ### Get GCMs historical period ####
  Historical_DATA_Pr <- loadStationData(dataset = paste0(dirc, "\\BiasCorrection\\GCMs\\temp"), 
                                        var = "precip", years = years_obs, lonLim = lonLim, latLim=latLim, units="mm")
  
  x <- Historical_DATA_Pr
  
  ### Get GCMs projection period ####
  Projection_DATA_Pr <- loadStationData(dataset = paste0(dirc, "\\BiasCorrection\\GCMs\\temp"), 
                                        var = "precip", years = years_proj, lonLim = lonLim, latLim=latLim, units="mm")
  newdata <- Projection_DATA_Pr
  
  cal <- biasCorrection(y = y, x = x,
                        newdata = newdata,
                        precipitation = TRUE, method = "eqm", extrapolation = "constant", 
                        window = c(30, 15),wet.threshold = 50)
  
  # GCMs_Projection_Corrected <- subsetGrid(cal, station.id = station)
  
  # plot QQ_plot and output this to tiff files
  # tiff(filename=paste0(dirc, "\\BiasCorrection\\GCMs\\Historical\\", model, "_QQ_Plot.tiff"), width=6.5, height=4, units="in", res=300)
  # quickDiagnostics(y,x,cal, type="daily", location=c(-76.1922,36.9033))
  # dev.off()
  
  # return(GCMs_Projection_Corrected)
  return(cal)
}


################################# MAIN SCRIPT  ##################################################################

for(model in unlist(GCMs)){
  print(model)
    
  # GCMs_Rain <- bias_correction(dirc, RCP , model, Stations)
  cal <- bias_correction(dirc, RCP , model, Stations)
    
  for(station in Stations$ID){
    station <- toString(station)
    GCMs_Rain <- subsetGrid(cal, station.id = station)
    
    rain_proj <- melt(data.frame(GCMs_Rain$Dates$start, GCMs_Rain$Data))
    colnames(rain_proj) <- c("Date", "STR", "Rain")
    # rain_proj$Year <- substr(rain_proj$Date, 1,4)
    
    # write output to csv
    #write.csv(rain_proj, file=paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP,"\\Test\\", model, "_BiasCorrected_Historical.csv"))
    if(!file.exists(paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP))){
      dir.create(paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP))
    }
    if(!file.exists(paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP, "\\", station))){
      dir.create(paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP, "\\", station))
    }
    if(!file.exists(paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP, "\\", station, "\\", model))){
      dir.create(paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP, "\\", station, "\\", model))
    }
    write.csv(rain_proj, file=paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP,"\\", station,"\\", model,"\\", model, "_BiasCorrected_WT01.csv"))
      
  }
}

