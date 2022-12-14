##testing mdsstat package
##vignette at https://rdrr.io/cran/mdsstat/f/vignettes/mdsstat_intro.Rmd

library(mds)
library(mdsstat)
library(dplyr)

source("R/Mds/test mds.R")

#Run One Algorithm {#onealgo}

# Example mds_ts data
data <- mds_ts[[3]]
data$rate <- ifelse(is.na(data$nA), 0, data$nA) / data$exposure

# Four different algorithm calls
xbar(data)
prr(data)
xbar(data, ts_event=c(Rate="rate"), we_rule=2)
poisson_rare(data, p_rate=0.3)

# mds_ts Usage
# 
# mds_ts data frames are generated by mds::time_series() from the mds package. These data frames are already structured for seamless integration into mdsstat algorithms.
# 
# Note the following:
#   
#   Disproportionality algorithms will run only if the mds_ts data contains the columns nA, nB, nC, and nD. These are generated by specifying device and event hierarchies using mds package functions.
# Algorithms run by default using the nA column for event occurrence.
# If running on event rate is desired, you may calculate an additional field and specify that field using the ts_event parameter.
# 
# Running an algorithm on event rate instead of event count

data <- mds_ts[[3]]
data$rate <- ifelse(is.na(data$nA), 0, data$nA) / data$exposure
xbar(data, ts_event=c("Rate"="rate"))


# General Usage: Count or Rate Data
# 
# A generic data frame contains two columns, time and event, where for each unique sequential time (numeric or Date), there corresponds a number indicating the event occurrence. The event occurrence may commonly be the count of events or event rate.
# 
# An example:
  
data <- data.frame(time=c(1:25), event=as.integer(stats::rnorm(25, 100, 25)))
xbar(data)


# General Usage: Data for Disproportionality Analysis (DPA)
# 
# Because disproportionality analysis is run on count data in a 2x2 contingency table, this data frame requires five columns, time, nA, nB, nC, and nD. For each unique sequential time (numeric or Date), there corresponds a set of numbers indicating the event counts. The latter four columns correspond to counts for cells A through D of the contingency table.
# 
# An example:
  
data <- data.frame(time=c(1:25),
                     nA=as.integer(stats::rnorm(25, 25, 5)),
                     nB=as.integer(stats::rnorm(25, 50, 5)),
                     nC=as.integer(stats::rnorm(25, 100, 25)),
                     nD=as.integer(stats::rnorm(25, 200, 25)))
prr(data)

# Run Multiple Algorithms {#mutlialgo}
#   
# mdsstat makes it easy to run multiple algorithms and variants of the same algorithm on your data.
#   
# Just two steps are required:
#     
#   Use define_algos() to set a list of algorithms with corresponding parameter settings.
#   Use run_algos() to run the algorithms defined in Step 1 on your data.
#   
#   For example:
    
    # Your data
data <- mds_ts[[3]]
data$rate <- ifelse(is.na(data$nA), 0, data$nA) / data$exposure
  
# Save a list of algorithms to run
x <- list(prr=list(),
            xbar=list(),
            xbar=list(ts_event=c(Rate="rate"), we_rule=2),
            poisson_rare=list(p_rate=0.3))
algos <- define_algos(x)
  
# Run algorithms
run_algos(data, algos)
# By default, run_algos() returns the results of each algorithm as a row in a data frame. This allows for easy tabular review of algorithm performance.

# One Algorithm Returned as a Data Frame Row
# 
# Similar to the default output of run_algos(), you may convert the output of any mdsstat algorithm from the default list to a data frame row. Simply use test_as_row() on any algorithm output.
# 
# For example:
  
data <- data.frame(time=c(1:25), event=as.integer(stats::rnorm(25, 100, 25)))
result <- xbar(data)
test_as_row(result)



##BCPNN
# Basic Example
data <- data.frame(time=c(1:25),
                   nA=as.integer(stats::rnorm(25, 25, 5)),
                   nB=as.integer(stats::rnorm(25, 50, 5)),
                   nC=as.integer(stats::rnorm(25, 100, 25)),
                   nD=as.integer(stats::rnorm(25, 200, 25)))
a1 <- bcpnn(data)
a1
# Example using an mds_ts object
a2 <- bcpnn(mds_ts[[3]])

