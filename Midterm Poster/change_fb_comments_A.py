# Split Facebook Comments from Summer Reasearch based on their base locations

import pandas as pd
import csv
from itertools import groupby

'''
stuff = pd.read_csv("armybasescommentsA.csv")
id_list = list(stuff['Post_ID'])
keys = []
for item in id_list:
    keys.append(item.split('_')[0])
stuff['keys'] = keys
stuff.to_csv("armybasescommentsB.csv")
'''

B = pd.read_csv("armybasescommentsB.csv")

for group_name, df in B.groupby('keys'):
    key = str(group_name) + '.csv'
    with open(key, 'a') as f:
        df.to_csv(f)
'''
for key, rows in groupby(csv.reader(open("armybasescommentsB.csv")),
                         lambda row: row[8]):
    with open("%s.csv" % key, "w") as output:
        for row in rows:
            output.write(",".join(row) + "\n")
'''