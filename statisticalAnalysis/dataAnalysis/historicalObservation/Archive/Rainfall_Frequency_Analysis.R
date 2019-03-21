# This script is to analysis daily rainfall projection data to get 24-hour rainfall 
# intensity for 2050 and 2100
# data from 2035 to 2065 is used to get IDF for 2050
# data from 2071 to 2100 is used to get IDF for 2100

library(nsRFA)

file_dir <- "C:/Users/Yawen Shen/Desktop/ThirdPaper/Climate Change/Rainfall Projection/RCP8/CSIRO-Mk3.6.0/output"

pr <- read.csv(paste0(file_dir, "/pr_day.csv"), header = FALSE, sep=",", col.names = c("Rain"), colClasses = c("double"))
time <- read.csv(paste0(file_dir, "/time_day.csv"), header = FALSE, sep=",", 
	col.name = c("Index", "Year", "Mon", "Day"), colClasses = c("NULL", "integer", "integer", "integer"))

time["Rain"] <- pr["Rain"]

## Dataframe include both time and rainfall data###
rain_proj <- time

#YearBaseline <- seq(from=1976, to=2005, by=1)
#Year2050 <- seq(from=2036, to=2065, by=1)
#Year2100 <- seq(from=2071, to=2100, by=1)

YearBaseline <- seq(from=1976, to=2015, by=1)
Year2050 <- seq(from=2031, to=2070, by=1)
Year2100 <- seq(from=2061, to=2100, by=1)

Max_DailyRain_Baseline <- list()
Max_DailyRain_2050 <- list()
Max_DailyRain_2100 <- list()

MaxRain <- function(Years){
	Max_DailyRain <- list()
	for(yr in Years){
		dailyRain <- rain_proj[rain_proj$Year==yr,]		
		Max_DailyRain[yr-min(Years)-1] <- max(dailyRain$Rain)
	}
	return(Max_DailyRain)
}

Max_DailyRain_Baseline <- MaxRain(YearBaseline)
Max_DailyRain_2050 <- MaxRain(Year2050)
Max_DailyRain_2100 <- MaxRain(Year2100)

Max_DailyRain_Baseline <- unlist(Max_DailyRain_Baseline, use.name=FALSE)
Max_DailyRain_2050 <- unlist(Max_DailyRain_2050, use.name=FALSE)
Max_DailyRain_2100 <- unlist(Max_DailyRain_2100, use.name=FALSE)

Max_DailyRain_Baseline <- sort(Max_DailyRain_Baseline, decreasing=FALSE)
Max_DailyRain_2050 <- sort(Max_DailyRain_2050, decreasing=FALSE)
Max_DailyRain_2100 <- sort(Max_DailyRain_2100, decreasing=FALSE)

# Set criteria
criteria <- "AIC"

MSCBaseline <- MSClaio2008(Max_DailyRain_Baseline, crit=criteria)
MSC2050 <- MSClaio2008(Max_DailyRain_2050, crit=criteria)
MSC2100 <- MSClaio2008(Max_DailyRain_2100, crit=criteria)

#####calculate 24-hour rainfall intensity for different return periods####
rain_24hrs <- function(MSC, Max_DailyRain){
	non_exc_prob <- c(0.99, 0.98, 0.9, 0.5, 0.01)
	if(MSC[paste0(criteria, "dist")] == "P3"){
		parms <- ML_estimation(Max_DailyRain, dist="P3")
		returns <- invF.gamma(non_exc_prob, parms[1],parms[2], parms[3])
	}

	if(MSC[paste0(criteria, "dist")] == "LP3"){
		parms <- ML_estimation(log(Max_DailyRain), dist="P3")
		returns <- exp(invF.gamma(non_exc_prob, parms[1],parms[2], parms[3]))
	}

	if(MSC[paste0(criteria, "dist")] == "NORM"){
		parms <- ML_estimation(Max_DailyRain, dist="NORM")
		returns <- qnorm(non_exc_prob, mean=parms[1], sd=parms[2])
	}

	if(MSC[paste0(criteria, "dist")] == "LN"){
		parms <- ML_estimation(log(Max_DailyRain), dist="NORM")
		returns <- exp(qnorm(non_exc_prob, mean=parms[1], sd=parms[2]))
	}

	if(MSC[paste0(criteria, "dist")] == "EV1" || MSC[paste0(criteria, "dist")] == "GUMBEL"){
		parms <- ML_estimation(Max_DailyRain, dist="EV1")
		returns <- parms[1] - parms[2]*log(-log(non_exc_prob))
	}

	if(MSC[paste0(criteria, "dist")] == "EV2"){
		parms <- ML_estimation(log(Max_DailyRain), dist="EV1")
		returns <- exp(parms[1] - parms[2]*log(-log(non_exc_prob)))
	}

	if(MSC[paste0(criteria, "dist")] == "GEV"){
		parms <- ML_estimation(Max_DailyRain, dist="GEV")
		returns <- invF.GEV(non_exc_prob, parms[1],parms[2], parms[3])
	}
	
	return(returns)
}

returns_Baseline <- rain_24hrs(MSCBaseline, Max_DailyRain_Baseline)
returns_2050 <- rain_24hrs(MSC2050, Max_DailyRain_2050)
returns_2100 <- rain_24hrs(MSC2100, Max_DailyRain_2100)

####Calculate the change of 24hour rainfall change compared to baseline for each return period (percentage, %)
returns_change_2050 <- (returns_2050 - returns_Baseline)/returns_Baseline*100
returns_change_2100 <- (returns_2100 - returns_Baseline)/returns_Baseline*100

#summary(MSC2050)
#summary(MSC2100)
#plot(MSCBaseline)
#plot(MSC2050)
#plot(MSC2100)

years <- c("100yr", "50yr", "10yr", "2yr", "1yr")
cat("24-hour Rainfall Changes Compared to Baseline on 2050:", "\n", years, "\n", round(returns_change_2050,1), sep=" ", "\n")
cat("24-hour Rainfall Changes Compared to Baseline on 2100:", "\n", years, "\n", round(returns_change_2100,1), sep=" ", "\n")

