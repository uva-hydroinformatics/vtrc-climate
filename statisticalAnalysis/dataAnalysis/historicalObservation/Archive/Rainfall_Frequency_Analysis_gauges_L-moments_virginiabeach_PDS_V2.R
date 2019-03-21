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
    Max_DailyRain[yr-min(Years)+1] <- max(dailyRain$PRCP.mm.)
    selected_year[yr-min(Years)+1] <- max(dailyRain$Year)
  }
  return(list(selected_year, Max_DailyRain))
}

PDSRain <- function(Years, Threshold){
  PDS_DailyRain <- list()
  PDS_selected_year <- list()
  threshold <- Threshold # lower threshold for 1-year return period (mm) for the selected station
  dailyRain <- rainfall_data[rainfall_data$PRCP.mm.>=threshold,]
  selected_year <- dailyRain$Year
  Max_DailyRain <- dailyRain$PRCP.mm.
  return(list(selected_year, Max_DailyRain))
}

################################# MAIN SCRIPT  ##################################################################

rainfall_data <- read.csv("./output/Virginia Beach study/USW00013737_USC00442368.csv", header = TRUE)


# include only the available start and end full year
YearBaseline <- seq(from= min(rainfall_data$Year), to=max(rainfall_data$Year), by=1)

#decide which data to include in the analysis
selection_threshold <- 300 #to include the Cleaned (full years records) use 365, to include all data use 0
i <- 1
j <- 1
Year_filtered_out <- list()
chosen_year <- list()
for (yr in YearBaseline){
  count_records <- rainfall_data[rainfall_data$Year==yr,]
  if (length(count_records$Year) < selection_threshold){
    Year_filtered_out[[i]] <- yr
    i <- i+1
  } else {
    chosen_year[[j]] <- yr
    j <- j+1
  }
}

Year_filtered_out <- unlist(Year_filtered_out, use.name=FALSE)
chosen_year <- unlist(chosen_year, use.name=FALSE)

for(yr in Year_filtered_out){
  rainfall_data <- rainfall_data[!rainfall_data$Year==yr,]
}

#loop through list of PDS threshold
#PDS_list <- list(90, seq(from= 50, to=85, by=1))
#PDS_list <- sort(unlist(PDS_list, use.name=FALSE), decreasing = TRUE)

#for (val in PDS_list){

# creatre empty list to append the annual max. daily rainfall
Max_DailyRain_Baseline <- list()

# get the annual max. daily rainfall
#year_selected <- MaxRain(chosen_year)[1]
#Max_DailyRain_Baseline <- MaxRain(chosen_year)[2]

# get the partial duration series
val <- 74
year_selected <- PDSRain(chosen_year, val)[1]
Max_DailyRain_Baseline <- PDSRain(chosen_year, val)[2] 

# Convert the list to vector and sort it ascendingly 
year_selected <- unlist(year_selected, use.name=FALSE)
Max_DailyRain_Baseline <- sort(unlist(Max_DailyRain_Baseline, use.name=FALSE), decreasing=FALSE)# * 1.13 # Maybe this values need to be multiple by 1.13 as in the Maryland report

# Max_DailyRain_Baseline <- Max_DailyRain_Baseline[c(4:102)]

# Now using the MSCIaio2008 to apply thr L-moments distribution
# Set criteria
criteria <- "AIC"

#MSCBaseline <- MSClaio2008(Max_DailyRain_Baseline, crit = criteria)

# non-exceedance probability for500, 100, 50, 25, 10, 5, 2, 1 years
non_exc_prob <- c(0.998, 0.99, 0.98, 0.96, 0.9, 0.8, 0.5, 0.2, 0.099099099)

ll <- Lmoments(Max_DailyRain_Baseline)

parameters <- par.GEV(ll[1],ll[2],ll[4])

returns_Baseline <- invF.GEV(non_exc_prob,parameters$xi,parameters$alfa,parameters$k)

years <- c("500yr", "100yr", "50yr", "25yr", "10yr", "5yr", "2yr", "1.25yr", "1.11yr")
output_lines <- c("24-hour Rainfall IDF generated from historical observation:", years, round(returns_Baseline,2)," ", round(returns_Baseline/25.4,2))
#output_IDF <- file(paste0("./output/Virginia Beach study/USW00013737_USC00442368_24H_IDF_AMS_300_no_CF.csv"))
output_IDF <- file(paste0("./output/Virginia Beach study/USW00013737_USC00442368_24H_IDF_PDS", val,"_300_no_CF.csv"))
writeLines(output_lines, output_IDF)
close(output_IDF)
#}