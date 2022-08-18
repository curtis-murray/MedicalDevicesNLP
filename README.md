# MedicalDevicesNLP

Brief and incomplete overview of some of the processes being used. There may be incomplete code that I need to fix up, and I've noticed some change in tidyverse's behaviour (more on this in the Notes section).

## Topic modelling pipeline

### Preprocessing
- Reports/TGA-DAEN.ipynb
Tobin's code to scrape data, saved to data/all_reports/all_reports_df.csv
- R/TGA-DAEN_match_manufacturers.R
  - This tries to match manufacturer names using fuzzy string detection and does some preprocessing.
- R/gen_train.R
 - A little more preprocessing and formatting for hSBM.

You won't have to run any of this if you don't want to, results are in data/hSBM/train.csv

### Topic Modelling
- Python/hSBM.py
  - Peforms hierarchical stochastic block modelling. Again, no need to run unless you want to change any of the preprocessing steps.

### Post Processing
- R/analyse_topics_clean.R
  - Formats the hSBM results into:
- data/tidy_topics.csv
  - This gives P(word | Level, Topic)
- data/tidy_topics_docs.csv
  - This gives P(topic | Level, Doc)

From here you can do all the fun stuff. 

### Analysis
- R/med_devices_data.R
  - This gets the data for the disproportionality analysis which Ty has been using.

## Notes: 
- You will need to run R/analyse_topics_clean.R to generate data/tidy_topics_docs.R as it is > 200MB and won't push to github.
- R appears to use ...1 instead of X1 now for loading unnamed colums. If you get an error, try finding "X1" and replacing with "...1". Probably the same with "X2" and "...2". #TODO I will need to check this at some point.

