import xarray
import os
import glob

station_name = "NorfolkAirport"

data_folders = ["RCP26", "RCP45", "RCP85"]

main_folder = "./RCMs_NA_Cordex_i/"

if not os.path.exists(main_folder+"Rainfall_Projection_TimeSeries"):
    os.makedirs(main_folder+"Rainfall_Projection_TimeSeries")

if not os.path.exists(main_folder+"Rainfall_Projection_TimeSeries/"+station_name):
    os.makedirs(main_folder+"Rainfall_Projection_TimeSeries/"+station_name)

for folder in data_folders:
    if os.path.exists("./" + folder):
        print folder
        folder_path = main_folder + "Rainfall_Projection_TimeSeries/" + station_name+"/"+folder
        if not os.path.exists(folder_path):
            os.makedirs(folder_path)
        for filename in glob.glob(os.path.join("./" + folder, '*.csv')):
            print filename
            GCM = filename.split("\\")[-1].split(".")[2]
            RCM = filename.split("\\")[-1].split(".")[3]
            if not os.path.exists(folder_path+"/"+GCM+"_"+RCM):
                os.makedirs(folder_path+"/"+GCM+"_"+RCM)
            data = open(filename, "r")
            pr_day = open(folder_path+"/"+GCM+"_"+RCM+"/pr_day.csv", "w")
            time_day = open(folder_path+"/"+GCM+"_"+RCM+"/time_day.csv", "w")
            for line in data:
                line_data = line.split(",")
                date = line_data[0].split("T")[0].split("-")
                pr = line_data[1]
                pr_day.write(pr)
                time_day.write(date[0]+","+date[1]+","+date[2]+"\n")
            data.close()
            pr_day.close()
            time_day.close()




