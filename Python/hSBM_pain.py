import matplotlib
import os
#import pylab as plt
from sbmtm import sbmtm
import graph_tool.all as gt
import pandas as pd
import numpy as np
import re
from itertools import chain
import sys
import glob
import time

data = pd.read_csv("data/hSBM_pain/train.csv")

texts = data["content"].values.tolist()
titles = data["Report number"].values.tolist()

texts = [str(c).split() for c in texts]

def run_hSBM(texts, titles):
    # Function to run the hSBM given the data
    model = sbmtm()

    ## we have to create the word-document network from the corpus
    model.make_graph(texts,documents=titles)

    ## we can also skip the previous step by saving/loading a graph
    # model.save_graph(filename = 'graph.xml.gz')
    # model.load_graph(filename = 'graph.xml.gz')

    ## fit the model
    # Loop a few times to make sure we actually get something! (sometimes the number of topics may be 0)
    for i in range(10):
        time_start = time.time()
        model.fit()
        time_end = time.time()
        topics = model.topics(l=0,n=10)
        if len(topics) > 1:
            break
    print(model.L)
    # Write time taken to execute and mdl
    pd.DataFrame.to_csv(pd.DataFrame({"seconds": [time_end - time_start]}),"".join(["data/hSBM_pain/time/time.csv"]))
    pd.DataFrame.to_csv(pd.DataFrame({"mdl": [model.mdl]}),"".join(["data/hSBM_pain/mdl/mdl.csv"]))

    for level in range(0,model.L+1):

        group_results = model.get_groups(l = level)
        p_w_tw = group_results['p_w_tw']
        pd.DataFrame.to_csv(pd.DataFrame(p_w_tw), "".join(["data/hSBM_pain/p_w_tw", str(level), ".csv"]))

        p_tw_td = group_results['p_tw_td']
        p_td_d = group_results['p_td_d']
        p_tw_d = group_results['p_tw_d']

        pd.DataFrame.to_csv(pd.DataFrame(p_tw_td), "".join(["data/hSBM_pain/p_tw_td", str(level), ".csv"]))
        pd.DataFrame.to_csv(pd.DataFrame(p_td_d), "".join(["data/hSBM_pain/p_td_d", str(level), ".csv"]))
        pd.DataFrame.to_csv(pd.DataFrame(p_tw_d), "".join(["data/hSBM_pain/p_tw_d", str(level), ".csv"]))

        np.save("data/hSBM_pain/p_w_tw" + str(level)  + ".npy",p_w_tw)
        np.save("data/hSBM_pain/p_tw_td" + str(level) + ".npy",p_tw_td)
        np.save("data/hSBM_pain/p_td_d" + str(level) + ".npy",p_td_d)

    pd.DataFrame.to_csv(pd.DataFrame(model.words), "".join(["data/hSBM_pain/words_all.csv"]))
    pd.DataFrame.to_csv(pd.DataFrame(model.documents), "".join(["data/hSBM_pain/docs_all.csv"]))

# Run hSBM
while(1):
    try:
        run_hSBM(texts, titles)
        break
    except Exception as e:
        print(e)
        print("Something went wrong, trying again...")
