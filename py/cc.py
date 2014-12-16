import csv

CSV_FILE = 'result.csv'

def read_data(filename):
    data = []
    for row in csv.reader(open(filename)):
        data.append([int(row[0])] + [float(t) for t in row[1:]])
    return data

data = read_data(CSV_FILE)

def extract_columns(data, columns_no):
    columns = [[] for i in range(len(columns_no))]
    for row in data:
        for i, no in enumerate(columns_no):
            columns[i].append(row[no])
    return columns


d = {}
for row in data:
    if row[0] in d:
        if row[1] > d[row[0]][1]:
            d[row[0]] = row
    else:
        d[row[0]] = row

data = list(d.values())
# data = data[:500]


import numpy as np
import matplotlib.pyplot as plt

def plot(data, i, j):
    x, y = extract_columns(data, [i, j])
    plt.scatter(x, y)
    plt.savefig('{:02d}_{:02d}.png'.format(i + 1, j + 1))

    plt.clf()

# for i in range(1, 14):
#     for j in range(1, 14):
#         plot(data, i, j)
#
# exit(0)


# columns = [extract_columns(data, [i])[0] for i in range(14)]
columns = list(zip(*data))

def hist(no):
    a = [0] * 100
    for i in columns[no]:
        k = int(min(99, round(i * 100)))
        a[k] = min(a[k] + 1, 400)
    plt.bar(range(0, 100), a)
    plt.savefig('hist_{}.png'.format(no + 1))

    plt.clf()

for i in range(1, 14):
    hist(i)



def rela():
    for j in range(12, 14):
        for i in range(12):
            cc = np.corrcoef(columns[i], columns[j])
    #         print(cc)
            if abs(cc[0][1]) > 0.2:
                print('{:2d} {:2d}: {}'.format(i + 1, j + 1, cc[0][1]))
