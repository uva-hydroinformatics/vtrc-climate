# This script is to analysis daily rainfall projection data to get 24-hour rainfall at specific rain guage location
# the available historical daily rainfall data were pulled and pre-processed from the NCDF 
# Statistical library used to generated the IDF values
library(nsRFA)
library(ggplot2)

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
  dailyRain <- rainfall_data[rainfall_data$PRCP.mm.>=threshold & rainfall_data$Year >= min(Years) & rainfall_data$Year <= max(Years),]
  selected_year <- dailyRain$Year
  Max_DailyRain <- dailyRain$PRCP.mm.
  return(list(selected_year, Max_DailyRain))
}

################################# MAIN SCRIPT  ##################################################################

rainfall_data <- read.csv("./output/Virginia Beach study/USW00013737_USC00442368.csv", header = TRUE)


# include only the available start and end full year
#YearBaseline <- seq(from= min(rainfall_data$Year), to=2005, by=1)
YearBaseline <- seq(from= 1911, to=2017, by=1)

#decide which data to include in the analysis
selection_threshold <- 0 #to include the Cleaned (full years records) use 365, to include all data use 0
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
val <- 10
year_selected <- PDSRain(chosen_year, val)[1]
Max_DailyRain_Baseline <- PDSRain(chosen_year, val)[2] 

# Convert the list to vector and sort it ascendingly 
year_selected <- unlist(year_selected, use.name=FALSE)
Max_DailyRain_Baseline <- unlist(Max_DailyRain_Baseline, use.name=FALSE)

# create a dataframe for the selected years and corrosponding values for plot AMS generation only
chosen_year_value <-  data.frame(Year = character(), Rainfall = numeric())
chosen_year_value <- rbind(chosen_year_value, data.frame(Year = year_selected, Rainfall = Max_DailyRain_Baseline/25.4)) #Here rainfall in inches
fit <- lm(Rainfall ~ Year, data = chosen_year_value)
# ggplot(chosen_year_value, aes(x = Year, y = Rainfall)) + geom_bar(stat = "identity") +
#   geom_smooth(method = "lm", linetype = "dashed") +
#   annotate("text", x = 1970, y = 8.5, label = 'bold("LM Slope = 1.98 Inches / Century")', parse = TRUE) +
#   annotate("text", x = 1964, y = 8, label = 'bold("Confidence = 95%")', parse = TRUE) +
#   labs(x = "Year", y = "Daily Rainfall (Inches)") +
#   ggtitle("Annual Maximum Series at Norfolk Airport") +
#   theme_bw() +
#   theme(plot.title = element_text(hjust = 0.5), axis.title = element_text(face = "bold.italic", color = "blue"))

Max_DailyRain_Baseline <- sort(Max_DailyRain_Baseline, decreasing=TRUE)# * 1.13 # Maybe this values need to be multiple by 1.13 as in the Maryland report
Max_DailyRain_Baseline <- Max_DailyRain_Baseline[c(1:length(chosen_year))]

# Max_DailyRain_Baseline <- Max_DailyRain_Baseline[c(4:102)]

# Now using the MSCIaio2008 to apply thr L-moments distribution
# Set criteria
criteria <- "AIC"

#MSCBaseline <- MSClaio2008(Max_DailyRain_Baseline, crit = criteria)

# non-exceedance probability for500, 100, 50, 25, 10, 5, 2, 1.25, 1 years
non_exc_prob <- c(0.99, 0.98, 0.96, 0.9, 0.8, 0.5, 0.2, 0.01)

ll <- Lmoments(Max_DailyRain_Baseline)

parameters <- par.GEV(ll[1],ll[2],ll[4])

returns_Baseline <- invF.GEV(non_exc_prob,parameters$xi,parameters$alfa,parameters$k)
print(round(returns_Baseline,2)/25.4)

years <- c("100yr", "50yr", "25yr", "10yr", "5yr", "2yr", "1.25yr", "1.1yr")
output_lines <- c("24-hour Rainfall IDF generated from historical observation:", years, round(returns_Baseline,2)," ", round(returns_Baseline/25.4,2))
#output_IDF <- file(paste0("./output/Virginia Beach study/USW00013737_USC00442368_24H_IDF_AMS_300_no_CF.csv"))
output_IDF <- file(paste0("./output/Virginia Beach study/USW00013737_USC00442368_24H_IDF_PDS", val,"_full_no_CF_V3.csv"))
writeLines(output_lines, output_IDF)
close(output_IDF)
#}

# Dataframe for ATLAS14 PDS IDF
atlas14 <- data.frame(matrix(ncol = 2, nrow = 8))
colnames(atlas14) <- c("return_period", "rainfall")
atlas14$return_period <- c(500, 100, 50, 25, 10, 5, 2, 1.1)
atlas14$rainfall <- c(12.6, 9.2, 7.95, 6.82, 5.5, 4.62, 3.57, 2.93)

library(lmomco)
lmr  <- lmr2par(Max_DailyRain_Baseline, type="gev")
CI <- genci.simple(lmr, n=1000, f=non_exc_prob, nsim=2000)
x <- c(100, 50, 25, 10, 5, 2, 1)
library(ggplot2)
ggplot(CI, aes(x=1/(1-nonexceed), y=true/25.4)) + geom_line()+ geom_point(size = 1.5) + #geom_text(aes(label=round(true/25.4, 2)),hjust=0, vjust=-5) +
  geom_point(aes(return_period, rainfall), atlas14, pch=3, size= 2, color = "red") +
  geom_line(aes(x=1/(1-nonexceed), y=lwr/25.4), CI, linetype = "dashed") + 
  geom_line(aes(x=1/(1-nonexceed), y=upr/25.4), CI, linetype = "twodash") +
  geom_ribbon(aes(ymin=lwr/25.4,ymax=upr/25.4), fill="blue", alpha="0.3") +
  scale_x_log10(breaks=c(1, 2,5, 10, 25, 50, 100), limits = c(1, 100)) +
  scale_y_continuous() + 
  labs(x = "Return Period (years)", y = "24-hr Rainfall (Inches)") +
  ggtitle("Generated IDF rainfall from Historical Records at Norfolk Airport (90% Confidence Level)") +
  theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5, size = 10, face = "bold"), axis.title = element_text(face = "bold.italic", color = "blue"))
  #theme(legend.position="top" )


