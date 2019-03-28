import pandas as pd
import os
import glob


data_folders = ["RCP26", "RCP45", "RCP85"]
main_folder = "./RCMs_NA_Cordex_i/"

if not os.path.exists(main_folder+"Rainfall_Projection_TimeSeries"):
    os.makedirs(main_folder+"Rainfall_Projection_TimeSeries")

for folder in data_folders:
    if os.path.exists("./" + folder):
        print(folder)

        for filename in glob.glob(os.path.join("./" + folder, '*.csv')):
            print(filename)
            GCM = filename.split("\\")[-1].split(".")[2]
            RCM = filename.split("\\")[-1].split(".")[3]
            rcm_data = pd.read_csv(filename)
            station_name = list(rcm_data.columns.values)[1:]

            for i in range(len(station_name)):
                folder_path = main_folder + "Rainfall_Projection_TimeSeries/" + station_name[i]+"/"+folder

                if not os.path.exists(folder_path):
                    os.makedirs(folder_path)

                if not os.path.exists(folder_path+"/"+GCM+"_"+RCM):
                    os.makedirs(folder_path+"/"+GCM+"_"+RCM)

                pr_day = open(folder_path+"/"+GCM+"_"+RCM+"/pr_day.csv", "w")
                time_day = open(folder_path+"/"+GCM+"_"+RCM+"/time_day.csv", "w")

                for date in rcm_data["date"].values:
                    date = date.split("-")
                    time_day.write(date[0]+","+date[1]+","+date[2]+"\n")

                for val in rcm_data[station_name[i]].values:
                    pr_day.write(str(val)+"\n")

                pr_day.close()
                time_day.close()
print("Done Preparing the data for the R Scripts!")
