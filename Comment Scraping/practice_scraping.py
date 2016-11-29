# Facebook Scraping Run-Through
# 11-3-16

import facebook
import requests

# An example function declaration to interact with the Facebook posts as they
# are being scraped
def print_post(post):
    print(post['message'])

# Get temporary token here: https://developers.facebook.com/tools/explorer/
access_token = 'EAACEdEose0cBANYYS9JgIqozocnsxzhnYgiFV4DKuI8vkBs7gSXBlFwVp5fqZBAdrRPqCuRvh8hG2QA0BIf9LbY5AkJqSO2LX0K3tohvrnEVpZAZBZBOFkeEyb29KkwRBPvLtlHZBWB9IzSC5qlWLIkZCo3dERRvapXOrzGFkIOgZDZD'

# Set up the GraphAPI for use with Bill Gate's profile
user = '110389852325552'
graph = facebook.GraphAPI(access_token)
profile = graph.get_object(user)
posts = graph.get_connections(profile['id'], 'posts')

# For using as an example during the live demo
newest = posts

# Wrap this block in a while loop so we can keep paginating requests until
# finished.
while True:
    try:
        # Perform some action on each post in the collection we receive from
        # Facebook.
        [print_post(post=post) for post in posts['data']]
        # Attempt to make a request to the next page of data, if it exists.
        posts = requests.get(posts['paging']['next']).json()
    except KeyError:
        # When there are no more pages (['paging']['next']), break from the
        # loop and end the script.
        break