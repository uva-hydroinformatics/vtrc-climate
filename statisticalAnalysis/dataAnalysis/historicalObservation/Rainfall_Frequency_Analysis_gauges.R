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

rainfall_data <- read.csv("./output/USW00013743.csv", header = TRUE)


# include only the available start and end full year
YearBaseline <- seq(from=1946, to=2017, by=1)

# creatre empty list to append the annual max. daily rainfall
Max_DailyRain_Baseline <- list()

# get the the annual max. daily rainfall
Max_DailyRain_Baseline <- MaxRain(YearBaseline)

# Convert the list to vector and sort it ascendingly 
Max_DailyRain_Baseline <- sort(unlist(Max_DailyRain_Baseline, use.name=FALSE), decreasing=FALSE)

# Now using the MSCIaio2008 to apply thr L-moments distribution
# Set criteria
criteria <- "AIC"

MSCBaseline <- MSClaio2008(Max_DailyRain_Baseline)


#####calculate 24-hour rainfall intensity for different return periods####
rain_24hrs <- function(MSC, Max_DailyRain){
  # non-exceedance probability for500, 100, 50, 25, 10, 5, 2, 1 years
	non_exc_prob <- c(0.998, 0.99, 0.98, 0.96, 0.9, 0.8, 0.5, 0.01)
	if(MSC$AICdist == "P3"){
		parms <- ML_estimation(Max_DailyRain, dist="P3")
		returns <- invF.gamma(non_exc_prob, parms[1],parms[2], parms[3])
	}

	if(MSC$AICdist == "LP3"){
		parms <- ML_estimation(log(Max_DailyRain), dist="P3")
		returns <- exp(invF.gamma(non_exc_prob, parms[1],parms[2], parms[3]))
	}

	if(MSC$AICdist == "NORM"){
		parms <- ML_estimation(Max_DailyRain, dist="NORM")
		returns <- qnorm(non_exc_prob, mean=parms[1], sd=parms[2])
	}

	if(MSC$AICdist == "LN"){
		parms <- ML_estimation(log(Max_DailyRain), dist="NORM")
		returns <- exp(qnorm(non_exc_prob, mean=parms[1], sd=parms[2]))
	}

	if(MSC$AICdist == "EV1" | MSC$AICdist == "GUMBEL"){
		parms <- ML_estimation(Max_DailyRain, dist="EV1")
		returns <- parms[1] - parms[2]*log(-log(non_exc_prob))
	}

	if(MSC[paste0(criteria, "dist")] == "EV2"){
		parms <- ML_estimation(log(Max_DailyRain), dist="EV1")
		returns <- exp(parms[1] - parms[2]*log(-log(non_exc_prob)))
	}

	if(MSC$AICdist == "GEV"){
		parms <- ML_estimation(Max_DailyRain, dist="GEV")
		returns <- invF.GEV(non_exc_prob, parms[1],parms[2], parms[3])
	}
	
	return(returns)
}

returns_Baseline <- rain_24hrs(MSCBaseline, Max_DailyRain_Baseline)


#summary(MSC2050)
#summary(MSC2100)
#plot(MSCBaseline)
#plot(MSC2050)
#plot(MSC2100)

years <- c("500yr", "100yr", "50yr", "25yr", "10yr", "25yr", "2yr", "1yr")
output_lines <- c("24-hour Rainfall IDF generated from historical observation:", years, round(returns_Baseline,1))
output_IDF <- file("./output/USW00013743_24H_IDF.csv")
writeLines(output_lines, output_IDF)
close(output_IDF)
