# Process the json files of the facebook comments into csv files

import json
import pandas as pd
import os

# Go through all the data that is larger than 20 KB 
    # i.e. the pages that have actual posts and comments
for subdir, dirs, files in os.walk('Data/'):
    for json_file in files:
        file_path = 'Data/' + json_file
        if os.stat(file_path).st_size > 20000:
            comments = json.load(open(file_path))
            
            # Store the data into a DataFrame and export it as a csv
            df = pd.DataFrame()
            df['Post_ID'] = [comment['Post_ID'].encode('ascii', 'replace') for comment in comments]
            df['Comment'] = [comment['Comment'].encode('ascii', 'replace') for comment in comments]
            df['Created_Time'] = [comment['Created_Time'].encode('ascii', 'replace') for comment in comments]
            df['Comment_ID'] = [comment['Comment_ID'].encode('ascii', 'replace') for comment in comments]
            df['Commenter_ID'] = [comment['Commenter_ID'].encode('ascii', 'replace') for comment in comments]
            df['Likes'] = [comment['Likes'] for comment in comments]
            
            csv_name = 'Processed_Data/' + json_file.split('.')[0] + ".csv" 
            df.to_csv(csv_name)
        
        