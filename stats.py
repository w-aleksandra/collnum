my_file = open("10chronopress.tsv", "r", encoding="utf-8")  # change filename to execute for a different file

header = next(my_file).strip().split("\t")

numC = header.index("NumC")
nounC = header.index("NounC")

dat_agr_counter = 0
dat_gen_counter = 0
loc_agr_counter = 0
loc_gen_counter = 0
inst_agr_counter = 0
inst_gen_counter = 0

for line in my_file:
    line = line.rstrip()
    line = line.split('\t')

    if line[numC] == 'DAT':
        if line[nounC] == 'DAT':
            dat_agr_counter += 1
        elif line[nounC] == 'GEN':
            dat_gen_counter += 1

    elif line[numC] == 'LOC':
        if line[nounC] == 'LOC':
            loc_agr_counter += 1
        elif line[nounC] == 'GEN':
            loc_gen_counter += 1

    elif line[numC] == 'INST':
        if line[nounC] == 'INST':
            inst_agr_counter += 1
        elif line[nounC] == 'GEN':
            inst_gen_counter += 1

    else:
        print(line)  # to double-check

if dat_agr_counter + dat_gen_counter != 0:
    print("DAT agreeing: " + str(dat_agr_counter) + " (" +
          str(round(dat_agr_counter / (dat_agr_counter + dat_gen_counter) * 100, 1)) + "%)")

    print("DAT GEN: " + str(dat_gen_counter) + " (" +
          str(round(dat_gen_counter / (dat_agr_counter + dat_gen_counter) * 100, 1)) + "%)")

if loc_agr_counter + loc_gen_counter != 0:
    print("LOC agreeing: " + str(loc_agr_counter) + " (" +
          str(round(loc_agr_counter / (loc_agr_counter + loc_gen_counter) * 100, 1)) + "%)")

    print("LOC GEN: " + str(loc_gen_counter) + " (" +
          str(round(loc_gen_counter / (loc_agr_counter + loc_gen_counter) * 100, 1)) + "%)")

if inst_agr_counter + inst_gen_counter != 0:
    print("INST agreeing: " + str(inst_agr_counter) + " (" +
          str(round(inst_agr_counter / (inst_agr_counter + inst_gen_counter) * 100, 1)) + "%)")

    print("INST GEN: " + str(inst_gen_counter) + " (" +
          str(round(inst_gen_counter / (inst_agr_counter + inst_gen_counter) * 100, 1)) + "%)")

my_file.close()
