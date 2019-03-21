import pandas as pd
import matplotlib.pyplot as plt
import matplotlib
import os
import pytz
from datetime import datetime as dt, timedelta
import numpy as np


station_name = {"02045500": "OS-A", "02047000": "OS-B", "02052000": "OS-C", "02052090": "OS-D",
            "02047500": "OS-E", "02047783": "OS-F", "02049500": "OS-G", "02050000": "OS-H",
            "02047370": "OS-I", "02053200": "OS-J", "02051500": "OS-K", "02051000": "OS-L",
            "02044500": "OS-M", "02046000": "OS-N"}

def utc_to_local(local_tz, utc_dt):
    utc_dt = dt.strptime(utc_dt, '%Y-%m-%dT%H:%M:%SZ')
    local_dt = utc_dt.replace(tzinfo=pytz.utc).astimezone(local_tz)
    return local_tz.normalize(local_dt).strftime('%Y-%m-%dT%H:%M:%SZ')


def local_to_utc(local_tz, local_dt):
    local_dt = dt.strptime(local_dt, '%Y-%m-%dT%H:%M:%SZ')
    local_dt = local_tz.localize(local_dt)
    utc_dt = local_dt.astimezone (pytz.utc)
    return utc_dt.strftime('%Y-%m-%dT%H:%M:%SZ')


def plot_obs_vs_mod_output(y, y_output, y_output_original, y_output_hr, y_output_sv, gage_id, start_data_utc,
                           end_data_utc, directory, grid_res, run_version):
# def plot_obs_vs_mod_output(y, y_output, y_output_original, gage_id, start_data_utc,
#                            end_data_utc, directory, grid_res, run_version):
    matplotlib.rcParams.update({'font.size': 12})
    plt.rcParams["font.family"] = "Times New Roman"
    fig = plt.figure()
    ax = fig.add_subplot(111)
    ax.plot(y, '.', markevery=10, label='Observation') #Dataset A
    ax.plot(y_output_original, '-s', markevery=80, color='g', label='V46 BF 425') #Dataset C
    ax.plot(y_output_hr, '-.o', markevery=80, color='Purple', label='V46 BF 43.5') #Dataset D
    # ax.plot(y_output_sv,'--x', markevery=80, color='Blue', label='30m new DEM') #Dataset E
    ax.plot(y_output, '-^', markevery=80, color='r', label='V46 No BF') #Dataset B
    ax.set_xlim(x.values[0], x.values[-1])
    ax.set_xlabel("Date/Time in UTC", fontweight='bold')
    ax.set_ylabel("Water Elevation (m)", fontweight='bold')
    #ax.set_title(station_name[gage_id]+" ("+start_data_utc+" - "+end_data_utc+")",
    ax.set_title("USGS Gage " + gage_id + " (" + start_data_utc + " - " + end_data_utc + ")",
         fontweight='bold', fontsize=12)
    legend_properties = {'weight': 'bold'}
    plt.legend(prop=legend_properties)
    ytick_min = [y.min().values, y_output_original.min().values, y_output.min().values]
    ytick_max = [y.max().values, y_output_original.max().values, y_output.max().values]
    plt.yticks(np.arange(np.around(min(ytick_min)) - 1, np.around(max(ytick_max)) + 3, 1.0))
    plt.xticks(rotation=90)
    fig.tight_layout()
    plt.rcParams["font.family"] = "Times New Roman"
    plt.show()
    fig.savefig(directory+'/'+gage_id+'.png')


###################################################################################################
# ***************************************** Main Program *****************************************#
###################################################################################################
# modelled start and end date/time in the local time zone using the format of %Y-%m-%dT%H:%M:%SZ
start_datetime = "2016-10-07T00:00:00Z"
end_datetime = "2016-10-24T00:00:00Z"
local_tz = pytz.timezone('US/Eastern')

# locate the USGS station observations #Dataset A
data_directory = "./Data adjusted to the vertical datum/"

# locate the model output time series file final version #Dataset B
output_file = "VU_30m_HPC_GPU_Nicole_2016_gauges_BWND_HQ_b_046_PO.csv"
grid_res = output_file.split('_')[1]
run_version = output_file.split('_')[6]

# locate the old model version output (original model big model) #Dataset C
output_file_original = "VU_30m_HPC_GPU_Nicole_2016_gauges_BWND_HQ_b_046_baseflow_425_00_PO.csv"

# locate the model version applying HT Fixed #Dataset D
output_file_hr = "VU_30m_HPC_GPU_Nicole_2016_gauges_BWND_HQ_b_046_baseflow_43_30_PO.csv"

# locate the model version applying HQ with b value #Dataset E
output_file_sv = "VU_30m_HPC_GPU_Nicole_2016_gauges_BWND_HQ_b_045_newDEM_PO.csv"


# plots directory
destination = output_file.split(".")[0]+"for publication"
if not os.path.exists(destination):
    os.makedirs(destination)


# create empty list to append the filename in the target USGS observation data directory
filename_var = []

# extract the file names and convert them to variable with dataframes include each file info
for filename in os.listdir(data_directory):
    filename_var.append("Gage_Info_"+filename.split(".")[1].split("@")[-1])
    vars()[filename_var[-1]] = pd.read_csv(data_directory+filename, low_memory=False, skiprows=14)

# convert the start and end date/time to UTC
start_datetime_utc = local_to_utc(local_tz, start_datetime)
end_datetime_utc = local_to_utc(local_tz, end_datetime)

# convert the model output to variable with dataframes and convert the hours to date/time
output_timeseries = pd.read_csv(output_file, low_memory=False, usecols=range(1,20))
output_timeseries_original = pd.read_csv(output_file_original, low_memory=False, usecols=range(1,20))
output_timeseries_hr = pd.read_csv(output_file_hr, low_memory=False, usecols=range(1,20))
output_timeseries_sv = pd.read_csv(output_file_sv, low_memory=False, usecols=range(1,20))
x_output = []
x_output_original = []
x_output_hr = []
x_output_sv = []
for hours in output_timeseries[[0]].values[1:]:
    x_output.append(dt.strptime(start_datetime_utc, '%Y-%m-%dT%H:%M:%SZ') +
                    timedelta(hours=float(hours)))

for hours in output_timeseries_original[[0]].values[1:]:
    x_output_original.append(dt.strptime(start_datetime_utc, '%Y-%m-%dT%H:%M:%SZ') +
                    timedelta(hours=float(hours)))

for hours in output_timeseries_hr[[0]].values[1:]:
    x_output_hr.append(dt.strptime(start_datetime_utc, '%Y-%m-%dT%H:%M:%SZ') +
                    timedelta(hours=float(hours)))

for hours in output_timeseries_sv[[0]].values[1:]:
    x_output_sv.append(dt.strptime(start_datetime_utc, '%Y-%m-%dT%H:%M:%SZ') +
                    timedelta(hours=float(hours)))

# get the index for the start and end data from the observation dataset to retrieve
# the corresponding water elevation.

# loop through all of the available USGS station and create the plots
for station in filename_var:
    if start_datetime_utc in eval(station).values and \
            end_datetime_utc in eval(station).values:
        start_datetime_index = int(
            eval(station).index[eval(station)
                                        ['ISO 8601 UTC'] == start_datetime_utc].tolist()[0])


        end_datetime_index = int(
            eval(station).index[eval(station)
                                        ['ISO 8601 UTC'] == end_datetime_utc].tolist()[0])


        eval(station)['ISO 8601 UTC'] = pd.to_datetime(eval(station)['ISO 8601 UTC'],
                                                                format='%Y-%m-%dT%H:%M:%SZ')
        x = eval(station)['ISO 8601 UTC'][(start_datetime_index):(end_datetime_index)]
        y = eval(station)['Water Level (m)'][(start_datetime_index):(end_datetime_index)]
        gage_id = station.split("_")[2]
        start_data_utc = str(x.values[0]).split("T")[0]
        end_data_utc = str(x.values[-1]).split("T")[0]

        # list to include the USGS stations that do not have modeled timeseries by the model
        station_wo_modeled_wl = []

        # change the range to range(1,10) for the new version of the model i.e. 013 and up
        for i in range(1,5):
            if '0'+str(int(output_timeseries[[i]].values[0])) == gage_id:
                y_output = []
                for wl_val in output_timeseries[[i]].values[1:]:
                    y_output.append(float(wl_val))

        for i in range(1,5):
            if '0'+str(int(output_timeseries_hr[[i]].values[0])) == gage_id:
                y_output_hr = []
                for wl_val in output_timeseries_hr[[i]].values[1:]:
                    y_output_hr.append(float(wl_val))

        for i in range(1,5):
            if '0'+str(int(output_timeseries_sv[[i]].values[0])) == gage_id:
                y_output_sv = []
                for wl_val in output_timeseries_sv[[i]].values[1:]:
                    y_output_sv.append(float(wl_val))

        for i in range(1,5):
            if '0'+str(int(output_timeseries_original[[i]].values[0])) == gage_id:
                y_output_original = []
                for wl_val in output_timeseries_original[[i]].values[1:]:
                    y_output_original.append(float(wl_val))

                #time_step_obs = str((x.values[1]-x.values[0]).astype('timedelta64[m]')).split(" ")[0]+"T"
                obs_df = pd.DataFrame({'datetime': x, 'values' : y}).set_index('datetime')
                modeled_df = pd.DataFrame({'datetime': x_output, 'values' : y_output}).set_index('datetime')
                modeled_df_orig = pd.DataFrame({'datetime': x_output_original, 'values' : y_output_original}).set_index('datetime')
                modeled_df_hr = pd.DataFrame({'datetime': x_output_hr, 'values' : y_output_hr}).set_index('datetime')
                modeled_df_sv = pd.DataFrame({'datetime': x_output_sv, 'values' : y_output_sv}).set_index('datetime')
                obs_df_resample = obs_df.resample('15T').mean()
                modeled_df_resample = modeled_df.resample('15T').mean()
                modeled_df_orig_resample = modeled_df_orig.resample('15T').mean()
                modeled_df_hr_resample = modeled_df_hr.resample('15T').mean()
                modeled_df_sv_resample = modeled_df_sv.resample('15T').mean()

                merge = pd.merge(obs_df_resample, modeled_df_resample, how='inner', left_index=True, right_index=True)

                # relative error
                time_diff_to_peak = merge['values_x'].idxmax()-merge['values_y'].idxmax()
                print time_diff_to_peak
                observed_peak = merge['values_x'].max()
                modeled_peak = merge['values_y'].max()
                re = (modeled_peak - observed_peak)/observed_peak *100.0
                print gage_id+' '+station_name[gage_id]+' RE(%): ' + str(np.around(re, 2))

                # NSE
                NSE_list_num = []
                NSE_list_den = []

                for i in range(len(merge)):
                    if np.isnan(merge.values_x[i]):
                        pass
                    else:
                        NSE_i_num = (merge.values_y[i] - merge.values_x[i]) ** 2
                        NSE_i_den = (merge.values_x[i] - merge['values_y'].mean()) ** 2
                        NSE_list_num.append(NSE_i_num)
                        NSE_list_den.append(NSE_i_den)

                NSE = 1 - (sum(NSE_list_num) / sum(NSE_list_den))
                print gage_id+' '+station_name[gage_id]+' NSE Value: ' + str(np.around(NSE, 2))


                plot_obs_vs_mod_output(obs_df_resample, modeled_df_resample, modeled_df_orig_resample, modeled_df_hr_resample, modeled_df_sv_resample, gage_id,
                                       start_data_utc, end_data_utc, destination, grid_res, run_version)
                # plot_obs_vs_mod_output(obs_df_resample, modeled_df_resample, modeled_df_orig_resample, gage_id,
                #                        start_data_utc, end_data_utc, destination, grid_res, run_version)

            elif '0' + str(int(output_timeseries_original[[i]].values[0])) != gage_id and gage_id not in station_wo_modeled_wl:
                    print 'No modeled water elevation timeseries for USGS station No.'+gage_id
                    station_wo_modeled_wl.append(gage_id)
    else:
        print "No observation available for the given start and end dates at USGS No."+\
              station.split("_")[-1]