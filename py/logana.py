import csv
import numpy as np
import matplotlib.pyplot as plt

def extract_columns(data, columns_no):
    columns = [[] for i in range(len(columns_no))]
    for row in data:
        for i, no in enumerate(columns_no):
            columns[i].append(row[no])
    return columns

def read_log(filename):
    log = []
    for row in csv.reader(open(filename)):
        log.append([int(row[0])] + [float(t) for t in row[1:]])
    return log
#    with open(filename) as f:
#        for line in f:
#            log.append(list(map(float, line.split())))
#    return log


import sys

log = read_log(sys.argv[1])

columns = list(zip(*log))
n = len(columns)

for i in range(1, n):
    for j in range(1, n):
        x = columns[i]
        y = columns[j]
        plt.scatter(x, y)
        plt.savefig('ana/{:02d}_{:02d}.png'.format(i, j))

        plt.clf()

