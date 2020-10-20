library(qcc)
library(tidyverse)
# qcc is a contributed R package for statistical quality control charts which provides:
#   
# * Shewhart quality control charts for continuous, attribute and count data
# * Cusum and EWMA charts
# * Operating characteristic curves
# * Process capability analysis
# * Pareto chart and cause-and-effect chart
# * Multivariate control charts.
data(pistonrings)

glimpse(pistonrings)

ggplot(pistonrings,aes(x=sample,y=diameter,group=sample)) +
  geom_boxplot(aes(fill=trial))

