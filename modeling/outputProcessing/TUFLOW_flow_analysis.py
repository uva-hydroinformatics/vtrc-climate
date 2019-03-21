import pandas as pd
import matplotlib.pyplot as plt
import numpy as np
from scipy.stats import linregress


def adjust_tuflow_po_header(df):
    df_header = []
    for col_nm in range(len(df.columns)):
        df_header.append(str(df.columns[col_nm]) + " " + str(df.values[0][col_nm]).split(".")[0])

    df.columns = df_header
    df = df.iloc[1:].reset_index(drop=True)
    return df


# define parameters
target_year = 2045

for return_period in [1, 2, 5, 10, 25, 50, 100]:
    # Read input data
    po_area = "po_area.csv"
    current = "VU_30m_HPC_GPU_m_h_" + str(return_period) + "yr_BWND_HQ_b_046_baseflow_PO.csv"
    future = "VU_30m_HPC_GPU_m_f_" + str(target_year) + "_" + str(return_period) + "yr_BWND_HQ_b_046_baseflow_PO.csv"

    drainage_area = pd.read_csv(po_area, low_memory=False)
    current_condition = pd.read_csv(current, low_memory=False)
    future_condition = pd.read_csv(future, low_memory=False)

    # Excluded locations due as they are near to the DS boundary
    location_to_exclude = ['10386', '17798', '10388', '2049500', '2050000',
                           '18289', '18288']

    # Adjust headers by merging the first two rows then re-index the dataset
    current_condition = adjust_tuflow_po_header(current_condition)
    future_condition = adjust_tuflow_po_header(future_condition)

    # extract the maximum value from each observation location
    location_fid = []
    location_name = []
    location_drainage_area = []
    location_ratio = []

    for col in range(len(current_condition.columns)):
        if current_condition.columns[col].split(" ")[0].split(".")[0] == "Flow":
            if current_condition.columns[col].split(" ")[-1] == future_condition.columns[col].split(" ")[-1]:
                ratio = ((future_condition.iloc[:, col].max()) -
                        (current_condition.iloc[:, col].max())) / (current_condition.iloc[:, col].max()) * 100.0
                if ratio > 0 and (current_condition.columns[col].split(" ")[-1]) not in location_to_exclude:
                    location_ratio.append(ratio)
                    location_name.append(current_condition.columns[col].split(" ")[-1])
                    location_drainage_area.append(
                        float(drainage_area.loc[drainage_area['gridcode'] ==
                                                np.float64(current_condition.columns[col].split(" ")[-1]), 'area_km2']))
                    location_fid.append(
                        float(drainage_area.loc[drainage_area['gridcode'] ==
                                                np.float64(current_condition.columns[col].split(" ")[-1]), 'FID']))

    # sort the collected data base on the drainage area
    tmp = sorted(zip(location_fid, location_name, location_drainage_area, location_ratio), key=lambda x: x[2])
    location_fid = [x[0] for x in tmp]
    location_name = [x[1] for x in tmp]
    location_drainage_area = [x[2] for x in tmp]
    location_ratio = [x[3] for x in tmp]

    # extract info for drainage basins of 25 km2 areas or less
    location_drainage_area_25 = []
    location_ratio_25 = []
    i = 0
    while location_drainage_area[i] <= 25:
        location_drainage_area_25.append(location_drainage_area[i])
        location_ratio_25.append(location_ratio[i])
        i += 1

    data_out = open('future_Q_ratio/' + str(return_period) + '_yr_' + str(target_year) + '.csv', 'w')
    data_out.write("FID,Location,area_km2,ratio\n")

    for i in range(len(location_fid)):
        data_out.write(str(location_fid[i]) + ',' + str(location_name[i]) + ',' +
                    str(location_drainage_area[i]) + ',' + str(location_ratio[i]) + '\n')

    data_out.close()

    # calculate regression line slope, intercept, r-value, p-value, stderr
    stat_data = linregress(location_drainage_area, location_ratio)

    # generate linear fit equation using numpy
    fit = np.polyfit(location_drainage_area, location_ratio, 1)
    fit_fn = np.poly1d(fit)

    fig = plt.figure()
    ax = fig.add_subplot(121)
    ax.plot(location_drainage_area, location_ratio, '.', location_drainage_area, fit_fn(location_drainage_area), '--k')
    ax.text(200, 85, r'Y = '+ str(round(stat_data.slope, 4)) +'X +' +str(round(stat_data.intercept, 2)), fontsize=12)
    ax.set_ylim(0, 100)
    ax.set_title(" ")
    ax.text(0, 95, r'a)', fontsize=12)
    ax.set_ylabel("Q-Peak Increased Ratio (%)")
    ax.set_xlabel("Drainage Area ($Km^2$)")

    # calculate regression line slope, intercept, r-value, p-value, stderr
    # for drainage basins of 25 km2 areas or less
    stat_data_25 = linregress(location_drainage_area_25, location_ratio_25)

    # generate linear fit equation using numpy
    # for drainage basins of 25 km2 areas or less
    fit_25 = np.polyfit(location_drainage_area_25, location_ratio_25, 1)
    fit_fn_25 = np.poly1d(fit_25)

    ax1 = fig.add_subplot(122)
    ax1.plot(location_drainage_area_25, location_ratio_25, '.', location_drainage_area_25,
            fit_fn(location_drainage_area_25), '--k')
    ax1.text(5, 85, r'Y = '+ str(round(stat_data_25.slope, 4)) +'X +' +str(round(stat_data_25.intercept, 2)),
             fontsize=12)
    ax1.set_ylim(0, 100)
    ax1.text(0, 95, r'b)', fontsize=12)
    ax1.set_xlabel("Drainage Area ($Km^2$)")

    fig.suptitle("Q-Peak Increase Ratio in " + str(target_year) + " (" + str(return_period) + "-Yr 24-hr Rainfall)",
                 fontsize=14)
    fig.tight_layout()
    plt.rcParams["font.family"] = "Times New Roman"
    plt.show()
    fig.savefig('future_Q_ratio/' + str(return_period) + '_yr_' + str(target_year) + '.png')
    print "Done with return period "+ str(return_period)

