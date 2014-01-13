"""
Author: David Selassie Opoku

Calculates average WHO and UNICEF estimates of national immunization 
coverage(wuenic) for countries  into  a given CSV file

INPUT INFORMATION
1st argument is filename of input CSV
--“WHO and UNICEF estimates of national immunization coverage, 
   2012 revision (completed July 2013)”.
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
est1 = header_list[5]
est2 = header_list[6]
print >> out, "%s, %s, %s" %(country, est1, est2) 

wuenic_avg1 = {}
wuenic_avg2 = {}

for line in f.readlines():
    f_list = line.split(",")
    country = f_list[1]
    gaviPhase, est1, est2 = f_list[4:7]
    if birth_avg1.has_key(country):
        np.append(wuenic_avg1[country],est1)
        np.append(wuenic_avg2[country],est2)
    else:
        wuenic_avg1[country] = np.array(float(est1))
        wuenic_avg2[country] = np.array(float(est2))
        
for country1, country2 in zip(wuenic_avg1.keys(), wuenic_avg2.keys()):
    assert country1 == country2
    #print "%s == %s" %(country1, country2)
    avg1 = wuenic_avg1[country1].mean()
    avg2 = wuenic_avg2[country2].mean()
    print >> out, "%s, %d, %d" %(country1, avg1, avg2)

f.close()
out.close()

