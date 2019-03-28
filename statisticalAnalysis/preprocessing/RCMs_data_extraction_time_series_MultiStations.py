import xarray
import os
import glob
import pandas as pd
from datetime import datetime, timedelta

stations = pd.read_csv("NCDC_Selected_Raingauge.csv")

# create dataframe to store all precip record
start_date = datetime(1950, 1, 1)
end_date = datetime(2100, 12, 31)
days = pd.date_range(start_date, end_date, freq='D')


# data_folders = ["Historical", "RCP26", "RCP45", "RCP85"]
data_folders = ["Historical", "RCP85"]

main_folder = "Z:/Projects/Bridges Climate Change Project/Statistical Analysis/Climate DATA/Climate Models/RCMs_NA_Cordex_i/"

for folder in data_folders[1:]:
    if os.path.isfile(main_folder + folder + "/GCMs.csv"):
        GCMs = open(main_folder + folder + "/GCMs.csv", "r")
        for model in GCMs:
            precip = pd.DataFrame({'date': days})
            precip = precip.set_index('date')

            for stats_ID in stations["ID"]:
                if stats_ID != "ID":
                    precip[stats_ID] = 0.0

            model_folder = model.split()[0]

            for filename in glob.glob(os.path.join(main_folder + data_folders[0] + "/" + model_folder, '*.nc')):
                print(filename)
                ds_hist = xarray.open_dataset(filename)
                pr_hist = ds_hist['pr']
                time_hist = ds_hist['time']

                for stats_ID in stations["ID"]:
                    station_name = stats_ID
                    lat = stations.loc[stations["ID"] == stats_ID, "LATITUDE"].iloc[0]
                    lon = stations.loc[stations["ID"] == stats_ID, "LONGITUDE"].iloc[0]
                    print(station_name + " " + str(lat) + " " + str(lon))
                    pr_p_hist = pr_hist.sel(lat=lat, lon=lon, method='nearest')
                    pr_p_hist = pr_p_hist * 60.0 * 60.0 * 24.0
                    pr_hist_values = [float(x) for x in pr_p_hist]
                    time_hist_values = [x.values for x in time_hist]

                    i = 0
                    for date in time_hist_values:
                        if " " in str(date):
                            date_val = str(date).split(" ")[0].split("-")
                        elif "T" in str(date):
                            date_val = str(date).split("T")[0].split("-")

                        date_val = datetime(int(date_val[0]), int(date_val[1]), int(date_val[2]))
                        if date_val >= start_date and date_val <= end_date:
                            precip.loc[date_val, station_name] = pr_hist_values[i]
                        i += 1
                ds_hist.close()

            for filename in glob.glob(os.path.join(main_folder + folder + "/" + model_folder, '*.nc')):
                print(filename)
                ds_ftr = xarray.open_dataset(filename)
                pr_ftr = ds_ftr['pr']
                time_ftr = ds_ftr['time']

                for stats_ID in stations["ID"]:
                    station_name = stats_ID
                    lat = stations.loc[stations["ID"] == stats_ID, "LATITUDE"].iloc[0]
                    lon = stations.loc[stations["ID"] == stats_ID, "LONGITUDE"].iloc[0]
                    print(station_name + " " + str(lat) + " " + str(lon))
                    pr_p_ftr = pr_ftr.sel(lat=lat, lon=lon, method='nearest')
                    pr_p_ftr = pr_p_ftr * 60.0 * 60.0 * 24.0
                    pr_ftr_values = [float(x) for x in pr_p_ftr]
                    time_ftr_values = [x.values for x in time_ftr]

                    i = 0
                    for date in time_ftr_values:
                        if " " in str(date):
                            date_val = str(date).split(" ")[0].split("-")
                        elif "T" in str(date):
                            date_val = str(date).split("T")[0].split("-")

                        date_val = datetime(int(date_val[0]), int(date_val[1]), int(date_val[2]))
                        if date_val >= start_date and date_val <= end_date:
                            precip.loc[date_val, station_name] = pr_ftr_values[i]
                        i += 1
                ds_ftr.close()

            if not os.path.exists(folder):
                os.makedirs(folder)
            precip.to_csv(folder + "/" + filename.split("\\")[-1] + ".csv")

print("Data extracted for all station locations!")
