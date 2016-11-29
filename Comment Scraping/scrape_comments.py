# Scrape the comments and posts made by official Army Base Pages

import facebook
import requests
import os
import os.path
from datetime import datetime
import json
import matplotlib
import numpy as np

# Time how long the script was executing
startTime = datetime.now()

# Retrieve Access Token: https://developers.facebook.com/tools/explorer/
access_token = 'EAACEdEose0cBAP4uTh7qPuMtygUZBFUSXLxQmL7IdGAb3OfzhvyFzbFGhRvDREBtmagzEZCeIZBMYvsCMAYeFfEgA9t9AnXiZBJ93lJtBsijDGRBlmKqCagsZA5fZCXhXhZAoJPmUwqNQp2P3QiszBGDIZAV3fAiZCxv4Swn3GiYWYAZDZD'
graph = facebook.GraphAPI(access_token)

# Open the text file containing the base names and id's and make it a dictionary
with open("base_IDs") as f:
    content = f.readlines()
bases = {}
for line in content:
    bases[line.split(':')[0].strip()] = line.split(':')[1].strip()
    
# Get the comments and the 

for base in bases:    
    # Check if the info has already been scraped from this page.  If so, just
    # add that base's info to the list.  Otherwise get the two earliest times
    key = 'Data/' + base + '_comments.json'  
         
    if not os.path.exists(key):
        comment_info = {}
        comment_list = []
        profile = graph.get_object(bases[base])
        posts = graph.get_connections(profile['id'], 'posts')
        # Go through all the posts on the Facebook page
        while True:
            try:
                # If there is a post available add that post's message and info
                if posts['data']:
                    # Cycle through all the posts in the chunk of posts grabbed
                    for post in posts['data']:
                        #print post['message'][:10]
                        post_ID = post['id']
                                                
                        # Get all the comments on the current post
                        while True:
                            try:
                                comments = post['comments']
                                try:
                                    for comment in comments['data']:
                                        comment_info = {}
                                        comment_info['Post_ID'] = post_ID
                                        comment_info['Comment'] = comment['message']
                                        comment_info['Created_Time'] = comment['created_time']
                                        comment_info['Comment_ID'] = comment['id']
                                        comment_info['Commenter_ID'] = comment['from']['id']
                                        comment_info['Likes'] = comment['like_count']
                                        comment_list.append(comment_info)
                                    comments = requests.get(comments['paging']['next']).json()
                                except KeyError:
                                    break
                                break
                            except KeyError:
                                break
                            break
                            
                # Attempt to make a request to the next page of data, if it exists.
                posts = requests.get(posts['paging']['next']).json()
            
            except KeyError:
                # When there are no more pages (['paging']['next']), break from 
                # the loop and end the script.
                with open(key, mode='w') as output:
                    json.dump(comment_list, output)

# Print out how long the script took
print datetime.now() - startTime