library(mds)
library(mdsstat)
library(dplyr)
#devtools::install_github("cran/mdsstat")

data("mds_ts")
data("maude")

maude <- data.frame(maude) %>%
  mutate(date_received=as.Date(date_received, format="%Y%m%d"),
         model_number= ifelse(model_number == "N/A", NA_character_, model_number),
         region=factor(region))


sales <- data.frame(sales) %>%
  mutate(region=factor(region),
         sales_month = as.Date(sales_month, format = "%Y-%m-%d"))

head(mds_ts)
head(maude)

#source("U:/medical devices/MDSstat/mdsstat-master/mdsstat-master/R/bcpnn.R")


##from https://rdrr.io/cran/mds/f/vignettes/mds_intro.Rmd

# Step 1 - Device Events
de <- deviceevent(
  maude,
  time="date_received",
  device_hierarchy=c("device_name", "device_class"),
  event_hierarchy=c("event_type", "medical_specialty_description"),
  key="report_number",
  covariates="region",
  descriptors="_all_")

# Step 2 - Exposures (Optional step)
ex <- exposure(
  sales,
  time="sales_month",
  device_hierarchy="device_name",
  match_levels="region",
  count="sales_volume")

# Step 3 - Define Analyses
da <- define_analyses(
  de,
  device_level="device_name",
  exposure=ex,
  covariates="region")

# Step 4 - Time Series
ts <- time_series(
  da,
  deviceevents=de,
  exposure=ex)

#Summarize Defined Analyses
summary(da)


#how All Analyses as a Data Frame

dadf <- define_analyses_dataframe(da)

head(dadf, 3)

knitr::kable(head(dadf, 3))

# Plot Time Series of Counts and Rates {#quickplot}
  
plot(ts[[1]])
plot(ts[[4]], "rate", type='l')
