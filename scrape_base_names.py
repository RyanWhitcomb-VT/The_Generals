# Scrape bases list
# 10-11-16

import requests
from bs4 import BeautifulSoup

page = requests.get("http://armybases.org/").content
soup = BeautifulSoup(page, "lxml")
rows = soup.find_all('tr')

bases = []
for row in rows:
    text = row.text.encode('ascii','replace')
    splits = text.split('\n')
    for split in splits:
        if split != '' and not split.isupper():
            bases.append(split)
          
        
thefile = open('Army Bases.txt', 'w')
for base in bases:
    thefile.write("%s\n" % base)
thefile.close()