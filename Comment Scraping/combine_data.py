# Combine processed files together

import pandas as pd
import os

# Go through every csv and make it a DataFrame, then add to list
file_list = []
for subdir, dirs, files in os.walk('Processed_Data/'):
    for processed_file in files[1:]:
        file_path = "Processed_Data/" + processed_file
        this_file = pd.read_csv(file_path)
        file_list.append(this_file)
        
# Concatenate all the DateFrames together
df = pd.concat(file_list)

df.to_csv("All_Bases.csv")