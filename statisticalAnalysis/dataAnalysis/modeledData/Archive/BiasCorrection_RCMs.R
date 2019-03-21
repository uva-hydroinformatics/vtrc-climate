library(loadeR)
library(downscaleR)
library(visualizeR)
library(nsRFA)
library(gsubfn)
library(reshape2)

years_obs <- 1951:2005
years_proj <- 1951:2005

lonLim <- c(-78, -60)
latLim <- c(30, 40)

############################################################################################################
################################## Get Raingauge Observations ##############################################
dirc <- "Z:\\Projects\\Bridges Climate Change Project\\Statistical Analysis\\Climate DATA\\Climate Models\\RCMs_NA_Cordex_i"
VALUE_DATA_Pr <- loadStationData(dataset = paste0(dirc,"\\BiasCorrection\\Observations\\ObservationInput"), 
                              var = "precip", years = years_obs, lonLim = lonLim, latLim=latLim, units="mm")
y <- VALUE_DATA_Pr


###########################################################################################################
########################## Build Function to get bias correction rainfall #################################
bias_correction <- function(dirc, RCP, GCM){
  
  pr <- read.csv(paste0(dirc, "\\Rainfall_Projection_TimeSeries\\NorfolkAirport\\", RCP, "\\", GCM, "\\pr_day.csv"), 
                 header = FALSE, sep=",", col.names = c("Rain"), colClasses = c("double"))
  is.num <- sapply(pr, is.numeric)
  pr[is.num] <- lapply(pr[is.num], round, 2)
  
  # time <- read.csv(paste0(dirc, "\\Rainfall_Projection_TimeSeries\\NorfolkAirport\\", RCP, "\\", GCM, "\\time_day.csv"), header = FALSE, sep=",", 
  #                 col.name = c("Index", "Year", "Mon", "Day"), colClasses = c("NULL", "integer", "integer", "integer"))
  time <- read.csv(paste0(dirc, "\\Rainfall_Projection_TimeSeries\\NorfolkAirport\\", RCP, "\\", GCM, "\\time_day.csv"), header = FALSE, sep=",", 
                   col.name = c("Year", "Mon", "Day"), colClasses = c("integer", "integer", "integer"))
  time["YYYYMMDD"] <- with(time, sprintf("%d%02d%02d", Year, Mon, Day))
  
  time["USW00013737"] <- pr["Rain"]
  time["USW00013737_1"] <- pr["Rain"]
  rain_proj <- time
  
  drops <- c("Year", "Mon", "Day")
  rain_proj <- rain_proj[, !(names(rain_proj) %in% drops)]
  
  precip_file <- paste0(dirc, "\\BiasCorrection\\GCMs\\temp\\precip_Feb29.txt")
  
  if(exists(precip_file)) {rm(precip_file)}
  
  write.table(rain_proj, precip_file
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
                        window = c(30, 15),wet.threshold = 0.1)
  
  GCMs_Projection_Corrected <- subsetGrid(cal, station.id = "USW00013737")
  
  # plot QQ_plot and output this to tiff files
  tiff(filename=paste0(dirc, "\\BiasCorrection\\GCMs\\Historical\\", model, "_QQ_Plot.tiff"), width=6.5, height=4, units="in", res=300)
  quickDiagnostics(y,x,cal, type="daily", location=c(-76.1922,36.9033))
  dev.off()
  
  return(GCMs_Projection_Corrected)
  
}


##################################################
#### RCP define####
RCP <- "RCP85"

GCMs <- read.csv(paste0(dirc, "\\", RCP, "\\GCMs_Feb29.csv"), header=FALSE)
DF_output_2100 <- data.frame(matrix(ncol = 6, nrow = 0))

for(model in unlist(GCMs)){
  print(model)

  GCMs_Rain <- bias_correction(dirc, RCP , model)
  
  rain_proj <- melt(data.frame(GCMs_Rain$Dates$start, GCMs_Rain$Data))
  colnames(rain_proj) <- c("Date", "STR", "Rain")
  # rain_proj$Year <- substr(rain_proj$Date, 1,4)
  
  # write output to csv
  #write.csv(rain_proj, file=paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP,"\\Test\\", model, "_BiasCorrected_Historical.csv"))
  write.csv(rain_proj, file=paste0(dirc, "\\BiasCorrection\\GCMs\\",RCP,"\\Projection\\", model, "_BiasCorrected_WT30TEST.csv"))
  
}
