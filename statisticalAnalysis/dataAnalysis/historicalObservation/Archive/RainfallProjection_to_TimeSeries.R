library(ncdf4)
library(ncdf4.helpers)
library(PCICt)
library(readr)
library(dplyr)
library(tidyr)
library(sos)

#' Function to process CMIP5 netCDF files to format required by 
#'    `futureheatwaves` package
#'
#' filepath: A character vector giving the path to the netCDF 
#'    climate model output file
#' output_dir: A character vector giving the path to the directory 
#'    where output should be saved
#' var: The climate model variable ("tas" is near-surface air temperature)
#' append: Logical specifying whether results should be appended to 
#'     existing files in the output directory
#' lon_range: If not NULL, a length two numerical vector giving the range
#'    of longitudes to be saved. Values should be in degrees east in the 
#'    0 to 360 degree range.
#' lat_range: If not NULL, a length two numerical vector giving the range
#'    of latitudes to be saved. Values should be in degrees north in the 
#     -90 to 90 range. 
process_cmip5_file <- function(filepath, output_dir, 
                               var = "pr",
                               append = TRUE, 
                               lon_loc = NULL, 
                               lat_loc = NULL){
        # Open netCDF connection, read in dimensions and climate variable
        climate_output <- ncdf4::nc_open(filepath)
        lon <- ncdf4::ncvar_get(climate_output, "lon")
        lat <- ncdf4::ncvar_get(climate_output, "lat")

	  print(lon)
	  print(lat)

	  print(length(lon))
	  print(length(lat))

	  if(max(lon)>180) {
		lon_loc = lon_loc + 360
		print("Longtitude is in 0 to 360 degree!")
		print(lon_loc)
		}

        pr_time <- ncdf4.helpers::nc.get.time.series(climate_output, 
                                                      v = var,
                                                      time.dim.name = "time")
        pr <- ncdf4::ncvar_get(climate_output, var)
        ncdf4::nc_close(climate_output)
        
        # Print out converted time and filepath name to ensure that 
        # time was converted correctly based on calendar
        cat(paste("First and last two dates processed for file named\n",
                  filepath, "are:\n"))
        print(format(pr_time[c(1:2, length(pr_time) - 1:0)],
                     "%Y-%m-%d"))
        
        # If requested, limit to certain ranges of latitude and/or longitude
	  lon_index <- 0
	  lat_index <- 0
        if(!is.null(lon_loc)){
                lon_index <- which.min(abs(lon_loc - lon)) 
                lon <- lon[lon_index]
		    print(lon_index)
		    print(lon)
        }
        if(!is.null(lat_loc)){
                lat_index <- which.min(abs(lat_loc - lat)) 
                lat <- lat[lat_index]
		    print(lat_index)
		    print(lat)
        }
	  #pr <- pr[lon_index , lat_index, ]*24*3600
	  pr <- pr[lon_index , lat_index, ]
	  print(pr)
	  
        # Change to appropriate format for futureheatwaves package
        time_output <- dplyr::data_frame(index = 1:length(pr_time),
                                         time = format(pr_time, 
                                                       "%Y-%m-%d")) %>%
                tidyr::separate(time, c("year", "month", "day")) %>%
                dplyr::mutate(year = as.integer(year),
                              month = as.integer(month),
                              day = as.integer(day))
        pr_output <- matrix(unlist(pr), 
                             ncol = length(lon) * length(lat),
                             byrow = TRUE) %>%
                as.data.frame()
        
        # Write out to files
        if(!dir.exists(output_dir)){
                dir.create(output_dir, recursive = TRUE)
        }
        readr::write_csv(time_output, 
                         path = paste0(output_dir, "/time_day.csv"),
                         col_names = FALSE, append = append)
        readr::write_csv(pr_output,
                         path = paste0(output_dir, "/pr_day.csv"),
                         col_names = FALSE, append = append)
        
        return("Finished")
}

GCM <- "CMCC-CM"

file_dir <- "C:/Users/Yawen Shen/Desktop/ThirdPaper/Climate Change/Rainfall Projection/"
#historical_dir <- paste0(file_dir, "Historical/", GCM)
file_dir <- paste0(file_dir, "RCP85/", GCM)
files <- list.files(path=file_dir, pattern="*.nc", full.names=TRUE, recursive=FALSE)
#historical_files <- list.files(path=historical_dir, pattern="*.nc", full.names=TRUE, recursive=FALSE)
#files <- append(historical_files, files)

lapply(files, function(x){
	process_cmip5_file(filepath = x,
                    output_dir = paste0(file_dir,
                                      "/output"),
                    lon_loc = -76.3,
                    lat_loc = 36.9)
})