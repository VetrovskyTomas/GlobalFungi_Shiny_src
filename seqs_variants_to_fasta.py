__author__ = 'vetrot'

import sys
import os

table_samples = sys.argv[1]
dereplicated = sys.argv[2]
search_type = sys.argv[3]
search_key = sys.argv[4]
out_fasta = sys.argv[5]

if dereplicated == "True":
    dereplicated = "TRUE"

if dereplicated == "true":
    dereplicated = "TRUE"

out_file = open(out_fasta, 'w')

i = 0
hashcode={}
for line in open(table_samples):
    dat = line.rstrip().split('\t')
    if dereplicated == "TRUE":
        i += 1
        out_file.write(">" + search_type + "_" + search_key + "_" +dat[1]+ "_"+ str(i) +'\n')
        out_file.write(dat[2] +'\n')
    else:
        samples = dat[0].split(';')
        for sample in samples:
            i += 1
            out_file.write(">" + sample + "_" + search_type + "_" + search_key + "_" +dat[1]+ "_"+ str(i) +'\n')
            out_file.write(dat[2] +'\n')

out_file.close()

print "Done."