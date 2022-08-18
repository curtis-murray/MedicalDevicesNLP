G'day there,

Here is a quick summary of what is contained in this codebase and some advice on the next steps. 

# Whats the context & question?
**The task is to explore tools to identify problems in the *text reports* of *implantable medical devices*.**

The current basic approach is to compare the baseline occurrence of problems to potential increases, and similarly relate these increases occurrences to corresponding activities.

### Open Questions
- What is the best vocabulary of problems/activities to search for.
- How can we identify significant words, word pairs or collections of words for each manufacturer.

# What Data?
### Main data
Two sets of main data: 
- From Australia there is the Therapeutic Goods Administration (TGA) which has the [Database of Adverse Event Notifications (medical devices)](https://apps.tga.gov.au/Prod/devices/daen-entry.aspx).
- In the US the *much* larger data from the Food and Drug Administration (FDA) is the [Manufacturer and User Facility Device Experience Database (MAUDE)](https://www.fda.gov/medical-devices/mandatory-reporting-requirements-manufacturers-importers-and-device-user-facilities/manufacturer-and-user-facility-device-experience-database-maude). The file `FDA MAUDE Primer.pdf` may help in understanding this data.

Both of these datasets have two varieties: the device/report data and the text/narrative data. The latter is of interest for text analysis.

The TGA data has been scraped in the `tgaScraping` subfolder. The data is available in the `data/tga` folder.
The MAUDE Jupyter notebook will download this data for you. See the notebooks for a preliminary analysis.

### Helpful data
- `vocabulary/CPA_activities.csv` is a list of physical activities from the [2011 Compendium of Physical Activities](https://sites.google.com/site/compendiumofphysicalactivities/home)
- `vocabulary/annex_*.json` is terminology of possible adverse events as classified by [IMDRF](http://imdrf.org/documents/documents.asp)

# Useful external tools
- [`medaCy`](https://github.com/NLPatVCU/medaCy) medical named entity recognition.
- [`PyMedTermino`](https://pypi.org/project/PyMedTermino/) medical technical terminology mapping.
- [`medpie`](https://www.ncbi.nlm.nih.gov/pmc/articles/PMC3289922/): an information extraction package for medical message board posts


# Other Glossary
- `tgaScraping/ProsthesisSponsors.xlsx` is the list of all sponsors in the DAEN data.
- `random_files/FDA MAUDE Primer.pdf` is a pdf report of what the FDA MAUDE data means.
  `random_files/Grant Proposal.pdf` is the project proposed for this grant.



# Specific Todo-list items:
* Map common words to the same vocabulary ('sexual intercourse', 'sex' and 'intercourse' -> 'sex')
* Map varied manufacturer names to distinct manufacturers.