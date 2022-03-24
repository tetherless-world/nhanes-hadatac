import csv
import glob

input_dir = "data/raw/"
output_dir = "data/"

year_codes = {
    'J' : "2017-2018",
    'I' : "2015-2016",
    'H' : "2013-2014"
}

for input_filename in glob.glob(input_dir + 'RXQ_RX*'):
    print(input_filename)
    year_code = input_filename[-5:-4]
    if year_code not in year_codes.keys():
        continue
    output_filename_p = output_dir + "DA-NHANES-" + year_codes[year_code] + "-RXQ_RX_" + year_code + "-P.csv"
    output_filename_t = output_dir + "DA-NHANES-" + year_codes[year_code] + "-RXQ_RX_" + year_code + "-T.csv"
    output_filename_p_100 = output_dir + "DA-NHANES-" + year_codes[year_code] + "-RXQ_RX_" + year_code + "-P-100.csv"
    output_filename_t_100 = output_dir + "DA-NHANES-" + year_codes[year_code] + "-RXQ_RX_" + year_code + "-T-100.csv"
    print(output_filename_t)
    print(output_filename_p)
    print(output_filename_t_100)
    print(output_filename_p_100)
    with open(input_filename, 'r') as input_file:
        reader = csv.DictReader(input_file)
        output_file_p = open(output_filename_p, "w")
        output_file_t = open(output_filename_t, "w")
        output_file_p_100 = open(output_filename_p_100, "w")
        output_file_t_100 = open(output_filename_t_100, "w")
        output_file_p.write("SEQN,RXDUSE,RXDDRUG,RXDDRGID,RXQSEEN,RXDDAYS,RXDRSC,RXDRSD,RXDCOUNT")
        output_file_t.write("SEQN,RXDUSE,RXDDRUG,RXDDRGID,RXQSEEN,RXDDAYS,RXDRSC,RXDRSD,RXDCOUNT")
        output_file_p_100.write("SEQN,RXDUSE,RXDDRUG,RXDDRGID,RXQSEEN,RXDDAYS,RXDRSC,RXDRSD,RXDCOUNT")
        output_file_t_100.write("SEQN,RXDUSE,RXDDRUG,RXDDRGID,RXQSEEN,RXDDAYS,RXDRSC,RXDRSD,RXDCOUNT")
        seqn = 0
        for row in reader:
            if seqn == 0:
                seqn = int(row["SEQN"])
            if row["RXDUSE"] != "1":
                continue
            for i in range(1, 4):
                rxdrsc = "RXDRSC" + str(i)
                rxdrsd = "RXDRSD" + str(i)
                new_row = "\n"
                new_row += row["SEQN"] + ","
                new_row += row["RXDUSE"] + ","
                new_row += row["RXDDRUG"] + ","
                new_row += row["RXDDRGID"] + ","
                new_row += row["RXQSEEN"] + ","
                new_row += row["RXDDAYS"] + ","
                if row[rxdrsc].endswith(".P"):
                    new_row += row[rxdrsc][:-2] + ","
                    new_row += "\"" + row[rxdrsd] + "\","
                    new_row += row["RXDCOUNT"]
                    output_file_p.write(new_row)
                    if int(row["SEQN"]) - seqn < 100:
                        output_file_p_100.write(new_row)
                elif row[rxdrsc].endswith("P"):
                    new_row += row[rxdrsc][:-1] + ","
                    new_row += "\"" + row[rxdrsd] + "\","
                    new_row += row["RXDCOUNT"]
                    output_file_p.write(new_row)
                    if int(row["SEQN"]) - seqn < 100:
                        output_file_p_100.write(new_row)
                elif row[rxdrsc]:
                    new_row += row[rxdrsc] + ","
                    new_row += "\"" + row[rxdrsd] + "\","
                    new_row += row["RXDCOUNT"]
                    output_file_t.write(new_row)
                    if int(row["SEQN"]) - seqn < 100:
                        output_file_t_100.write(new_row)