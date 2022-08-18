# Run a single scraping

from scraping import run_scraping

run_scraping('pump')



# Download all sponsors
from scraping import run_scraping
import pandas as pd
from tqdm import tqdm

ProsthesisSponsors = []
for sheet in ['ProsthesisSponsor2012','ProsthesisSponsor2017','ProsthesisSponsor2021']:
    ProsthesisSponsors.extend(pd.read_excel('ProsthesisSponsor.xlsx', sheet_name=sheet).iloc[:,0].to_list())

import re
def clean_sponsor(text):
    text = text.lower()
    text = text.replace('pty','')
    text = text.replace('ltd','')
    text = text.replace('limited','')
    text = re.sub(r'[^\w\s]','',text)
    text = text.rstrip().lstrip()
    return text

ProsthesisSponsors = [clean_sponsor(spon) for spon in ProsthesisSponsors]
ProsthesisSponsors = list(set(ProsthesisSponsors))

# Get all loaded in data
import os
completed_sponsors = []
for file in os.listdir("../data"):
    if file[-6:] == 'pickle':
        if file[:16] == 'list_of_reports_':
            completed_sponsors.append(file[16:-7])

todo_sponsors = [spon for spon in ProsthesisSponsors if clean_sponsor(spon) not in completed_sponsors]

manual = ['ROCHE DIABETES', 'Rymed', 'BXTAccelyon', 'Q-Med', 'Roche Diagnostics','ABBOTT','OMX' ,'Kynesis' ,'JOY' ,'Soutter' ,'LTR' ,'RocketAlphatech' ,'BKLT' ,'Loxo' ,'Nvision' ,'Rhino' 'E4 SURGICAL', 'St Jude','Future' ,'Geistlich']
failed = []
for sponsor in tqdm(manual):
    try:
        run_scraping(sponsor, disabletqdm = True)
        print(sponsor, 'succeeded')
    except Exception as e:
        print(sponsor, 'failed to collect.',e)
        failed.append(sponsor)