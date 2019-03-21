import xarray
import os
import glob

station_name = "NorfolkAirport"
lat = 36.8963
lon = -76.1975
data_folders = ["Historical", "RCP26", "RCP45", "RCP85"]

main_folder = "./RCMs_NA_Cordex_i/"

for folder in data_folders[1:]:
    if os.path.isfile(main_folder+folder+"/GCMs.csv"):
        GCMs = open(main_folder+folder+"/GCMs.csv","r")
        for model in GCMs:
           model_folder = model.split()[0]
           time_values = []
           pr_values = []
           for filename in glob.glob(os.path.join(main_folder+data_folders[0]+"/"+model_folder, '*.nc')):
               print filename
               ds_hist = xarray.open_dataset(filename)
               pr_hist = ds_hist['pr']
               time_hist = ds_hist['time']
               pr_p_hist = pr_hist.sel(lat=lat, lon=lon, method='nearest')
               pr_p_hist = pr_p_hist * 60.0 * 60.0 * 24.0
               pr_hist_values = [float(x) for x in pr_p_hist]
               time_hist_values = [x.values for x in time_hist]
               for val in pr_hist_values:
                   pr_values.append(val)
               for date in time_hist_values:
                   time_values.append(date)
               ds_hist.close()
           for filename in glob.glob(os.path.join(main_folder+folder+"/"+model_folder, '*.nc')):
               print filename
               ds_ftr = xarray.open_dataset(filename)
               pr_ftr = ds_ftr['pr']
               time_ftr = ds_ftr['time']
               pr_p_ftr = pr_ftr.sel(lat=lat, lon=lon, method='nearest')
               pr_p_ftr = pr_p_ftr * 60.0 * 60.0 * 24.0
               pr_ftr_values = [float(x) for x in pr_p_ftr]
               time_ftr_values = [x.values for x in time_ftr]
               for val in pr_ftr_values:
                   pr_values.append(val)
               for date in time_ftr_values:
                   time_values.append(date)
               ds_ftr.close()
           if not os.path.exists(folder):
               os.makedirs(folder)
           csv_file = open(folder+"/"+filename.split("\\")[-1]+".csv", "w")
           for i in range(len(time_values)):
               csv_file.write(str(time_values[i])+","+str(pr_values[i])+"\n")
           csv_file.close()

