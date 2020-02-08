# I am currently doing the trials on jupyter lab, and conserving 
# the notes here so that it sticks in my mind.

# importing libraries : 
import requests
import urllib.request
import time
from bs4 import BeautifulSoup
import os

os.chdir("/home/bennour/Rprojects/elections/scrapped_data")

# task uno : 
# getiting the url, the html text, find the links and than store them.

url = "http://www.isie.tn/resultats/proces-verbaux-legislatives/"
r = requests.get(url)
soup = BeautifulSoup(r.text, "html.parser") 

# task 2: find the relevant links, after site inspection

links = []
for link in soup.select('a[href^="http://www.isie.tn/wp-content/uploads/resultatsfinalsleg"]'):
    links.append(link.get('href'))
print(links)

# for some reason there was an issue with the monastir link, so we need to fix it manually

i = links.index('http://www.isie.tn/wp-content/uploads/resultatsfinalsleg/monastir.pdf\r\n')
links[i] = 'http://www.isie.tn/wp-content/uploads/resultatsfinalsleg/monastir.pdf'

# task 3 : download each pdf file in the directory with it's name
# We can also merge all the files together
from PyPDF2 import PdfFileMerger
merger = PdfFileMerger()

for link in links:
    response = requests.get(link)
    title = link.split("/")[-1]
    with open(title, 'wb') as f:
        f.write(response.content)
    merger.append(title)

merger.write("ELECTIONS.pdf")
merger.close()
