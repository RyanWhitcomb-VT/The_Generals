# Scrapes the first two posts made by Army Base Facebook Pages

import facebook
import requests
import os
from datetime import datetime
import json
import matplotlib
import numpy as np

# Retrieve Access Token: https://developers.facebook.com/tools/explorer/
access_token = 'EAACEdEose0cBANYYS9JgIqozocnsxzhnYgiFV4DKuI8vkBs7gSXBlFwVp5fqZBAdrRPqCuRvh8hG2QA0BIf9LbY5AkJqSO2LX0K3tohvrnEVpZAZBZBOFkeEyb29KkwRBPvLtlHZBWB9IzSC5qlWLIkZCo3dERRvapXOrzGFkIOgZDZD'
graph = facebook.GraphAPI(access_token)

# Open the text file containing the base names and id's and make it a dictionary
with open("base_IDs") as f:
    content = f.readlines()
bases = {}
for line in content:
    bases[line.split(':')[0].strip()] = line.split(':')[1].strip()
    
# Get the earliest two times that the page has posted and store it as a list 
# of dictionaries
startTime = datetime.now()
base_info_list = []
for base in bases:    
    # Check if the info has already been scraped from this page.  If so, just
    # add that base's info to the list.  Otherwise get the two earliest times
    key = base+'.json'       
    if not os.path.exists(key):
        base_info = {}
        profile = graph.get_object(bases[base])
        posts = graph.get_connections(profile['id'], 'posts')
        while True:
            try:
                if posts['data']:
                    save = posts
                # Attempt to make a request to the next page of data, if it exists.
                posts = requests.get(posts['paging']['next']).json()
            except KeyError:
                # When there are no more pages (['paging']['next']), break from the
                # loop and end the script.
                base_info['Base Name'] = base
                base_info['Base ID'] = bases[base]
                base_info['First Post'] = save['data'][-1]['created_time']
                try:
                    base_info['Second Post'] = save['data'][-2]['created_time']
                except IndexError:
                    base_info['Second Post'] = base_info['First Post']
                base_info_list.append(base_info)
                with open(key, mode='w') as output:
                    json.dump(base_info, output)
                
                break
    else:
        with open(key) as data_file:
            base_info = json.load(data_file)
        base_info_list.append(base_info)
print datetime.now() - startTime

# Extract list of dates for all of the pages - potential to make hist of in future
list_of_datetimes = [datetime.strptime(record['Second Post'].encode('ascii', 'replace').split('T')[0], '%Y-%m-%d') for record in base_info_list]
dates = matplotlib.dates.date2num(list_of_datetimes)