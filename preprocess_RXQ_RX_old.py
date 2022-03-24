import csv
import glob

input_dir = "data/raw/"
output_dir = "data/"

year_codes = {
    'G' : "2011-2012",
    'F' : "2009-2010",
    'E' : "2007-2008"
}

for input_filename in glob.glob(input_dir + 'RXQ_RX*'):
    print(input_filename)
    year_code = input_filename[-5:-4]
    if year_code not in year_codes.keys():
        continue
    output_filename = output_dir + "DA-NHANES-" + year_codes[year_code] + "-RXQ_RX_" + year_code + ".csv"
    output_filename_100 = output_dir + "DA-NHANES-" + year_codes[year_code] + "-RXQ_RX_" + year_code + "-100.csv"
    print(output_filename)
    print(output_filename_100)
    with open(input_filename, 'r') as input_file:
        reader = csv.DictReader(input_file)
        output_file = open(output_filename, "w")
        output_file_100 = open(output_filename_100, "w")
        output_file.write("SEQN,RXDUSE,RXDDRUG,RXDDRGID,RXQSEEN,RXDDAYS,RXDCOUNT")
        output_file_100.write("SEQN,RXDUSE,RXDDRUG,RXDDRGID,RXQSEEN,RXDDAYS,RXDCOUNT")
        seqn = 0
        for row in reader:
            if seqn == 0:
                seqn = int(row["SEQN"])
            if row["RXDUSE"] != "1":
                continue
            output_file.write("\n")
            output_file.write(','.join(map(str, list(row.values()))))
            if int(row["SEQN"]) - seqn < 100:
                output_file_100.write("\n")
                output_file_100.write(','.join(map(str, list(row.values()))))