# This script is to analysis daily rainfall projection data to get 24-hour rainfall at specific rain guage location
# the available historical daily rainfall data were pulled and pre-processed from the NCDF 
# Statistical library used to generated the IDF values
library(nsRFA)

# Function for computing the annual daily maximum rainfall 
MaxRain <- function(Years){
  Max_DailyRain <- list()
  for(yr in Years){
    dailyRain <- rainfall_data[rainfall_data$Year==yr,]		
    Max_DailyRain[yr-min(Years)+1] <- max(dailyRain$PRCP.mm.)
  }
  return(Max_DailyRain)
}

################################# MAIN SCRIPT  ##################################################################

rainfall_data <- read.csv("./output/REPORT Maryland/USC00189750.csv", header = TRUE)


# include only the available start and end full year
YearBaseline <- seq(from=1895, to=1998, by=1)

# creatre empty list to append the annual max. daily rainfall
Max_DailyRain_Baseline <- list()

# get the the annual max. daily rainfall
Max_DailyRain_Baseline <- MaxRain(YearBaseline) 

# Convert the list to vector and sort it ascendingly 
Max_DailyRain_Baseline <- sort(unlist(Max_DailyRain_Baseline, use.name=FALSE), decreasing=FALSE) * 1.13 # Maybe this values need to be multiple by 1.13 as in the Maryland report

Max_DailyRain_Baseline <- Max_DailyRain_Baseline[c(2:104)]

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
output_lines <- c("24-hour Rainfall IDF generated from historical observation:", years, round(returns_Baseline,1))
output_IDF <- file("./output/REPORT Maryland/USC00189750_24H_IDF_CF.csv")
writeLines(output_lines, output_IDF)
close(output_IDF)
