f = open("USW00013769.dly", 'r')
f_out = open("USW00013769.csv", 'w')
f_out.write("ID, Year, Month, Day,  PRCP(mm)\n")

for line in f:
    if line[17:21] == 'PRCP':
        day = 1
        i=21
        j=26
        while i < 269:
            if line[i:j] != "-9999":
                print i, j, type(line[i:j])
                f_out.write(line[0:11]+","+line[11:15]+","+line[15:17]+","+str(day)+","+str(float(line[i:j])*0.1)+"\n")
                day += 1
            i += 8
            j += 8
f.close()
f_out.close()
