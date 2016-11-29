# Get the Facebook pages for every Army base used

import facebook
import requests
import json

# Retrieve Access Token: https://developers.facebook.com/tools/explorer/
access_token = 'EAACEdEose0cBAE026ZAaTni8kvnbb6BnoEDjnb48fUDSzcLpc3qhkX1ORHM58m71eJTWPTuNrHo4f4pZCJLPe8daAb88laiDX3XQUvCtpw64JBbiMD7YeLERviIudm9ygRWrrpD3fmc1YV2AVEORUeAXt16V0Judyo2bXZB8wZDZD'
graph = facebook.GraphAPI(access_token)

# Open the text file containing the base names and id's and make it a dictionary
with open("base_IDs") as f:
    content = f.readlines()
bases = {}
for line in content:
    bases[line.split(':')[0].strip()] = line.split(':')[1].strip()
    print line.split(':')[1].strip()
    
# Make a dictionary of bases to their Facebook page
page_dict = {}
for base in bases:    
    profile = graph.get_object(bases[base])
    page_dict[base] = profile['link']
    
json.dump(page_dict, open("Base_Pages.txt",'w'))