__author__ = 'vetrot'

import sys
import os

fasta_variants = sys.argv[1]
table_samples = sys.argv[2]
dereplicated = sys.argv[3]
search_type = sys.argv[4]
search_key = sys.argv[5]
out_fasta = sys.argv[6]

if dereplicated == "True":
    dereplicated = "TRUE"

if dereplicated == "true":
    dereplicated = "TRUE"

#print dereplicated

hashcode={}
for line in open(table_samples):
    dat = line.rstrip().split(' ')
    hashcode[dat[1]] = dat[0]+'|'+dat[2]

out_file = open(out_fasta, 'w')

filled = False
title = ''
sequence = ''
i = 0
for n, line in enumerate(open(fasta_variants)):
    if n % 2 == 0:
        title = line.rstrip()
        if title[0] != '>':
            print "fasta format error..." + title
            break
    else:
        if n % 2 == 1:
            sequence = line.rstrip()
            filled = True
    if filled:
        if dereplicated == "TRUE":
            i = i + 1
            vals = hashcode[title[1:]].split('|')
            out_file.write(">" + search_type + "_" + search_key + "_" +vals[1]+ "_"+ str(i) +'\n')
            out_file.write(sequence +'\n')
        else:
            vals = hashcode[title[1:]].split('|')
            samples = vals[0].split(';')
            for sample in samples:
                i = i + 1
                out_file.write(">" + sample + "_" + search_type + "_" + search_key + "_" +vals[1]+ "_"+ str(i) +'\n')
                out_file.write(sequence +'\n')
        filled = False
out_file.close()

print "Done."