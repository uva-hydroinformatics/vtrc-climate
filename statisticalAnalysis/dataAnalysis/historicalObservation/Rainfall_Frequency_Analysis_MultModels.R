# This script is to analysis daily rainfall projection data to get 24-hour rainfall 
# intensity for 2050 and 2100
# data from 2035 to 2065 is used to get IDF for 2050
# data from 2071 to 2100 is used to get IDF for 2100

library(nsRFA)
library(gsubfn)

######################################################################################################
#####calculate 24-hour rainfall intensity for different return periods based on given distribution####
rain_24hrs <- function(MSC, Max_DailyRain, criteria ){
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

#####################################################################################################
#####define function to calculate the 24-hour rainfall for each given GCM############################
returns_rain <- function(file_dir){
	pr <- read.csv(paste0(file_dir, "/pr_day.csv"), header = FALSE, sep=",", col.names = c("Rain"), colClasses = c("double"))
	time <- read.csv(paste0(file_dir, "/time_day.csv"), header = FALSE, sep=",", 
		col.name = c("Index", "Year", "Mon", "Day"), colClasses = c("NULL", "integer", "integer", "integer"))

	time["Rain"] <- pr["Rain"]
	rain_proj <- time

	YearBaseline <- seq(from=1985, to=2015, by=1)
	Year2050 <- seq(from=2021, to=2050, by=1)
	Year2100 <- seq(from=2071, to=2100, by=1)

	#YearBaseline <- seq(from=1986, to=2005, by=1)
	#Year2050 <- seq(from=2041, to=2060, by=1)
	#Year2100 <- seq(from=2081, to=2100, by=1)

	#YearBaseline <- seq(from=1976, to=2015, by=1)
	#Year2050 <- seq(from=2031, to=2070, by=1)
	#Year2100 <- seq(from=2061, to=2100, by=1)

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

	returns_Baseline <- rain_24hrs(MSCBaseline, Max_DailyRain_Baseline, criteria)
	returns_2050 <- rain_24hrs(MSC2050, Max_DailyRain_2050, criteria)
	returns_2100 <- rain_24hrs(MSC2100, Max_DailyRain_2100, criteria)

	####Calculate the change of 24hour rainfall change compared to baseline for each return period (percentage, %)
	returns_change_2050 <- (returns_2050 - returns_Baseline)/returns_Baseline*100
	returns_change_2100 <- (returns_2100 - returns_Baseline)/returns_Baseline*100
	
	# return two values
	return(c(returns_change_2050, returns_change_2100))

}


##########################Climate Change Scenario is the only variable to change####################
file_dir <- "C:/Users/Yawen Shen/Desktop/ThirdPaper/Climate Change/Rainfall Projection/RCP26/"


###################################################################################################
GCMs <- read.csv(paste0(file_dir, "GCMs.csv"), header=FALSE)
DF_output_2050 <- data.frame(matrix(ncol = 6, nrow = 0))
DF_output_2100 <- data.frame(matrix(ncol = 6, nrow = 0))

for(model in unlist(GCMs)){
	print(model)
	output <- returns_rain(paste0(file_dir, model, "/output/"))
	returns_change_2050 <- output[1:5]
	returns_change_2100 <- output[6:10]
	DF_output_2050 <- rbind(DF_output_2050, returns_change_2050)
	DF_output_2100 <- rbind(DF_output_2100, returns_change_2100)

	print(returns_change_2050)
	print(returns_change_2100)
}

colnames(DF_output_2050) <- c("YR100", "YR50", "YR10", "YR2", "YR1")
colnames(DF_output_2100) <- c("YR100", "YR50", "YR10", "YR2", "YR1")
DF_output_2050["Models"] <- unlist(GCMs)
DF_output_2100["Models"] <- unlist(GCMs)

# write output to csv
write.csv(DF_output_2050, file=paste0(file_dir, "24Hour_Rainfall_2050_30yr.csv"))
write.csv(DF_output_2100, file=paste0(file_dir, "24Hour_Rainfall_2100_30yr.csv"))
