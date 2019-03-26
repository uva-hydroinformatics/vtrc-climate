import csv
import pandas as pd
from datetime import datetime, timedelta


id_data = open("NCDC_Selected_Raingauge.csv", 'r')
id_data_csv = csv.reader(id_data, delimiter=',')

# create dataframe to store all precip record
startdate = datetime(1950, 1, 1)
enddate = datetime(2017, 12, 31)
days = pd.date_range(startdate,enddate, freq='D')
precip = pd.DataFrame({'date': days})
precip =precip.set_index('date')

station_list = []

for row in id_data_csv:
    if row[1] != "ID":
        station_list.append(row[1])
        precip[row[1]] = 0.0


for st_id in range(len(station_list)):
    
    num = 0
    f = open("F:\\Projects\\Bridges Climate Change Project\\Statistical Analysis\\NCDC daily rain gauges data until 08-29-2018\\all\\" +
             station_list[st_id] + ".dly", 'r')
    f_out = open(station_list[st_id] +".csv", 'w')
    f_out.write("ID, Year, Month, Day,  PRCP(mm)\n")

    for line in f:
        if line[17:21] == 'PRCP':
            day = 1
            i=21
            j=26
            while i < 269:
                if line[i:j] != "-9999":
                    #print(i, j, type(line[i:j]))
                    f_out.write(line[0:11]+","+line[11:15]+","+line[15:17]+","+str(day)+","+str(float(line[i:j])*0.1)+"\n")
                    if int(line[11:15])>=startdate.year and int(line[11:15])<=enddate.year:
                        num += 1
                        precip.loc[datetime(int(line[11:15]),int(line[15:17]), day),line[0:11]] = float(line[i:j])*0.1
                    day += 1
                i += 8
                j += 8
    
    f.close()
    f_out.close()
    print("processed: " +station_list[st_id] + "   " + str(num))

precip.to_csv("precip.csv")
