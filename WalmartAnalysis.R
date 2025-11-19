# library(tidyverse)
# library(vroom)
# library(dplyr)
# 

# 
# train_walmart <- vroom("train.csv") %>%
#   mutate(Store = factor(Store),
#          Dept = factor(Dept))
# test_walmart <- vroom("test.csv") %>%
#   mutate(Store = factor(Store),
#          Dept = factor(Dept))
# features_walmart <- vroom("features.csv") %>%
#   mutate(Store = factor(Store))
# stores_walmart <- vroom("stores.csv") %>%
#   mutate(Store = factor(Store))




# 1. no markdown data before Nov 2011
# how much is missing? proportion?
# sapply(features_walmart, function(x) mean(is.na(x)))
# missing data w employment
# consumer price data



# find which stores and departments are missing
# levels(test_walmart$Store)
# levels(test_walmart$Dept)
# 
# levels(train_walmart$Store)
# levels(train_walmart$Dept)
# 
# levels(features_walmart$Store)




# TIME SERIES 
# ============================================================================

## Libraries I need
library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)

## Read in the Data
train <- vroom("train.csv") 
# %>% mutate(Store = factor(Store),
#          Dept = factor(Dept))
test <- vroom("test.csv") 
# %>% mutate(Store = factor(Store),
#          Dept = factor(Dept))
features <- vroom("features.csv") 
# %>% mutate(Store = factor(Store))
# stores <- vroom("stores.csv") %>%
#   mutate(Store = factor(Store))

#########
## EDA ##
#########
plot_missing(features)
plot_missing(test)

### Impute Missing Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    TotalMarkdown = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDownFlag = if_else(TotalMarkdown > 0, 1, 0),
    MarkDownLog   = log1p(TotalMarkdown)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe))


## Join imputed features dataset with test and train data
train_imputed <- train %>%
  left_join(imputed_features, by = c("Store", "Date"))

test_imputed <- test %>%
  left_join(imputed_features, by = c("Store", "Date"))
