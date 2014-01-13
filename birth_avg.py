"""
Author: David Selassie Opoku

Calculates average birth population for countries from 
WHO and UNICEF estimates into  a given CSV file

INPUT INFORMATION
1st argument is filename of input CSV
2nd argument is filename of output CSV

OUTPUT INFORMATION
1st column is the country name
2nd column is first avg estimate
3rd column is second avg estimate 

"""

from os import sys
import numpy as np

input_filename = sys.argv[1]
out_filename = sys.argv[2]

f = open(input_filename)
out = open(out_filename, "w")
header = f.readline()
header_list = header.split(",")
country = header_list[1]
gaviPhase = header_list[4]
birth_est1 = header_list[5]
birth_est2 = header_list[6]
print >> out, "%s, %s, %s" %(country, birth_est1, birth_est2) 

birth_avg1 = {}
birth_avg2 = {}

for line in f.readlines():
    f_list = line.split(",")
    country = f_list[1]
    gaviPhase, birth_est1, birth_est2 = f_list[4:7]
    if birth_avg1.has_key(country):
        np.append(birth_avg1[country],birth_est1)
        np.append(birth_avg2[country],birth_est2)
    else:
        birth_avg1[country] = np.array(float(birth_est1))
        birth_avg2[country] = np.array(float(birth_est2))
        
for country1, country2 in zip(birth_avg1.keys(), birth_avg2.keys()):
    assert country1 == country2
    #print "%s == %s" %(country1, country2)
    avg1 = birth_avg1[country1].mean()
    avg2 = birth_avg2[country2].mean()
    print >> out, "%s, %d, %d" %(country1, avg1, avg2)

f.close()
out.close()

