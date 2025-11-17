library(tidyverse)
library(vroom)
library(dplyr)



train_walmart <- vroom("train.csv") %>%
  mutate(Store = factor(Store),
         Dept = factor(Dept))
test_walmart <- vroom("test.csv") %>%
  mutate(Store = factor(Store),
         Dept = factor(Dept))
features_walmart <- vroom("features.csv") %>%
  mutate(Store = factor(Store))
stores_walmart <- vroom("stores.csv") %>%
  mutate(Store = factor(Store))




# 1. no markdown data before Nov 2011
# how much is missing? proportion?
sapply(features_walmart, function(x) mean(is.na(x)))
# missing data w employment
# consumer price data



# find which stores and departments are missing
levels(test_walmart$Store)
levels(test_walmart$Dept)

levels(train_walmart$Store)
levels(train_walmart$Dept)

levels(features_walmart$Store)
