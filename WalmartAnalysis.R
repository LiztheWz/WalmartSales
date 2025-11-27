# library(tidyverse)
# library(vroom)
# library(dplyr)
# 4322

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
# library(tidyverse)
# library(vroom)
# library(tidymodels)
# library(DataExplorer)
# library(glmnet)
# 
# ## Read in the Data
# train <- vroom("train.csv") 
# # %>% mutate(Store = factor(Store),
# #          Dept = factor(Dept))
# test <- vroom("test.csv") 
# # %>% mutate(Store = factor(Store),
# #          Dept = factor(Dept))
# features <- vroom("features.csv") 
# # %>% mutate(Store = factor(Store))
# # stores <- vroom("stores.csv") %>%
# #   mutate(Store = factor(Store))
# 
# #########
# ## EDA ##
# #########
# plot_missing(features)
# plot_missing(test)
# 
# ### Impute Missing Markdowns
# features <- features %>%
#   mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
#   mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
#   mutate(
#     TotalMarkdown = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
#     MarkDownFlag = if_else(TotalMarkdown > 0, 1, 0),
#     MarkDownLog   = log1p(TotalMarkdown)
#   ) %>%
#   select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)
# 
# # Create tibble of Holidays
# holidays <- tibble(
#   Date = as.Date(c(
#     "2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08",  # Super Bowl
#     "2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06",  # Labor Day
#     "2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29",  # Thanksgiving
#     "2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27"   # Christmas
#   )),
#   HolidayName = c(
#     rep("SuperBowl", 4),
#     rep("LaborDay", 4),
#     rep("Thanksgiving", 4),
#     rep("Christmas", 4)
#   )
# )
# 
# # Replace isHoliday with the actual Holiday
# features <- features %>%
#   left_join(holidays, by = "Date") %>%
#   mutate(
#     Holiday = if_else(IsHoliday, HolidayName, "None")
#   ) %>%
#   select(-HolidayName)
# 
# ## Impute Missing CPI and Unemployment
# feature_recipe <- recipe(~., data=features) %>%
#   step_mutate(DecDate = decimal_date(Date)) %>%
#   step_impute_bag(CPI, Unemployment,
#                   impute_with = imp_vars(DecDate, Store))
# imputed_features <- juice(prep(feature_recipe))
# 
# 
# ## Join imputed features dataset with test and train data
# train_imputed <- train %>%
#   left_join(imputed_features, by = c("Store", "Date")) %>%
#   select(-IsHoliday.x)
# 
# test_imputed <- test %>%
#   left_join(imputed_features, by = c("Store", "Date")) %>%
#   select(-IsHoliday.x)
# 
# 
# 
# # ML FOR TIME SERIES
# # ============================================================================
# 
# set.seed(123)
# 
# store_dept <- train_imputed %>%
#   distinct(Store, Dept) %>%
#   sample_n(30)
# 
# sample_sets <- train_imputed %>%
#   semi_join(store_dept, by = c("Store", "Dept"))
# 
# 
# time_series_recipe <- recipe(Weekly_Sales ~ ., data = sample_sets) %>%
#   step_mutate(Store = factor(Store), Dept = factor(Dept)) %>%
#   step_date(Date, features = "doy") %>%
#   step_range(date_doy, min = 0, max = pi) %>%
#   step_mutate(sinDOY = sin(date_doy), cosDOY = cos(date_doy)) %>%
#   step_dummy(all_nominal_predictors()) %>%
#   step_normalize(all_numeric_predictors()) %>%
#   step_zv(all_predictors())
# 
# # cv_folds <- train_data %>%
# #   sliding_period(~ Date, period = "month", lookback = 6) %>%
# #   rolling_origin(initial = 52, assess = 4, skip = 4)
# 
# rf_model <- rand_forest(mtry = tune(),
#                         trees = 1000,
#                         min_n = tune()) %>% 
#   set_engine("ranger") %>%
#   set_mode("regression")
# 
# 
# forest_wf <- workflow() %>%
#   add_recipe(time_series_recipe) %>%
#   add_model(rf_model)
# 
# forest_grid <- grid_regular(mtry(),
#                             min_n(),
#                             levels = 5)
# 
# folds <- vfold_cv(sample_sets, v = 10, repeats = 1)
# fit(data= sample_sets)
# 
# CV_results <- forest_wf %>%
#   tune_grid(resamples = folds,
#             grid = forest_grid,
#             metrics = rmse)
# 
# bestTune <- CV_results %>%
#   select_best(metric = "rmse")
# 
# final_wf <- forest_wf %>%
#   finalize_workflow(bestTune) %>%
#   fit(data = train_imputed)
# 
# 
# predictions <- final_wf %>%
#   predict(new_data = test_imputed) %>%
#   mutate(Weekly_Sales = exp(.pred)) %>%
#   bind_cols(test_imputed %>% select(datetime)) %>%
#   select(datetime, count) %>%
#   mutate(datetime = format(datetime, "%Y-%m-%d %H:%M:%S"))
# 
# 
# vroom_write(predictions, "random_forests_predictions.csv", delim = ',')
# 
# 
# 





library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)
library(glmnet)
library(lubridate)


train <- vroom("train.csv")
test  <- vroom("test.csv")
features <- vroom("features.csv")


features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    TotalMarkdown = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDownFlag = if_else(TotalMarkdown > 0, 1, 0),
    MarkDownLog   = log1p(TotalMarkdown)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)


# Replace IsHoliday TRUE/FALSE with Actual Holiday Name
holidays <- tibble(
  Date = as.Date(c(
    "2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08",  # Super Bowl
    "2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06",  # Labor Day
    "2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29",  # Thanksgiving
    "2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27"   # Christmas
  )),
  HolidayName = c(
    rep("SuperBowl", 4),
    rep("LaborDay", 4),
    rep("Thanksgiving", 4),
    rep("Christmas", 4)
  )
)

features <- features %>%
  left_join(holidays, by = "Date") %>%
  mutate(
    Holiday = if_else(IsHoliday, HolidayName, "None")
  ) %>%
  select(-HolidayName)


# Impute CPI + Unemployment

feature_recipe <- recipe(~., data = features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment, impute_with = imp_vars(DecDate, Store))

imputed_features <- juice(prep(feature_recipe))


# Join Features With Train and Test
train_imputed <- train %>%
  left_join(imputed_features, by = c("Store", "Date")) %>%
  select(-IsHoliday.x, -IsHoliday.y)

test_imputed <- test %>%
  left_join(imputed_features, by = c("Store", "Date")) %>%
  select(-IsHoliday.x, -IsHoliday.y)


# sample 30 Store–Department combinations
set.seed(123)

store_dept <- train_imputed %>%
  distinct(Store, Dept) %>%
  sample_n(30)

sample_sets <- train_imputed %>%
  semi_join(store_dept, by = c("Store", "Dept"))


# Recipe
time_series_recipe <- recipe(
  Weekly_Sales ~ .,
  data = sample_sets
) %>%
  step_mutate(Store = factor(Store), Dept = factor(Dept)) %>%
  step_date(Date, features = "doy") %>%
  step_range(Date_doy, min = 0, max = pi) %>%
  step_mutate(
    sinDOY = sin(Date_doy),
    cosDOY = cos(Date_doy)
  ) %>%
  step_rm(Date) %>%           
  step_dummy(all_nominal_predictors()) %>%
  step_normalize(all_numeric_predictors()) %>%
  step_zv(all_predictors())


# Random Forest Model

rf_model <- rand_forest(
  mtry = tune(),
  trees = 1000,
  min_n = tune()
) %>%
  set_engine("ranger") %>%
  set_mode("regression")


rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(time_series_recipe)


forest_grid <- grid_regular(mtry(range = c(1,9)), min_n(), levels = 5)
                          
                          
folds <- vfold_cv(sample_sets, v = 10)

CV_results <- rf_workflow %>%
  tune_grid(
    resamples = folds,
    grid = forest_grid,
    metrics = metric_set(rmse)
  )

bestTune <- CV_results %>%
  select_best(metric = "rmse")


final_wf <- rf_workflow %>%
  finalize_workflow(bestTune) %>%
  fit(data = sample_sets)


test_preds <- predict(final_wf, new_data = test_imputed) %>%
  bind_cols(test_imputed %>% select(Store, Dept, Date)) %>%
  mutate(
    Id = paste(Store, Dept, Date, sep = "_"),
    Weekly_Sales = .pred
  ) %>%
  select(Id, Weekly_Sales)

vroom_write(test_preds, "Predictions.csv", delim = ',')












 
# PROPHET 
# ============================================================================
# 
# ## Libraries I need
# library(tidyverse)
# library(vroom)
# library(tidymodels)
# library(DataExplorer)
# library(glmnet)
# library(prophet)
# library(dplyr)
# library(ggplot2)
# library(patchwork)
# 
# ## Read in the Data
# train <- vroom("train.csv") 
# # %>% mutate(Store = factor(Store),
# #          Dept = factor(Dept))
# test <- vroom("test.csv") 
# # %>% mutate(Store = factor(Store),
# #          Dept = factor(Dept))
# features <- vroom("features.csv") 
# # %>% mutate(Store = factor(Store))
# # stores <- vroom("stores.csv") %>%
# #   mutate(Store = factor(Store))
# 


# plot_missing(features)
# plot_missing(test)
# 
# ### Impute Missing Markdowns
# features <- features %>%
#   mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
#   mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
#   mutate(
#     TotalMarkdown = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
#     MarkDownFlag = if_else(TotalMarkdown > 0, 1, 0),
#     MarkDownLog   = log1p(TotalMarkdown)
#   ) %>%
#   select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)
# 
# # Create tibble of Holidays
# holidays <- tibble(
#   Date = as.Date(c(
#     "2010-02-12", "2011-02-11", "2012-02-10", "2013-02-08",  # Super Bowl
#     "2010-09-10", "2011-09-09", "2012-09-07", "2013-09-06",  # Labor Day
#     "2010-11-26", "2011-11-25", "2012-11-23", "2013-11-29",  # Thanksgiving
#     "2010-12-31", "2011-12-30", "2012-12-28", "2013-12-27"   # Christmas
#   )),
#   HolidayName = c(
#     rep("SuperBowl", 4),
#     rep("LaborDay", 4),
#     rep("Thanksgiving", 4),
#     rep("Christmas", 4)
#   )
# )
# 
# # Replace isHoliday with the actual Holiday
# features <- features %>%
#   left_join(holidays, by = "Date") %>%
#   mutate(
#     Holiday = if_else(IsHoliday, HolidayName, "None")
#   ) %>%
#   select(-HolidayName)
# 
# ## Impute Missing CPI and Unemployment
# feature_recipe <- recipe(~., data=features) %>%
#   step_mutate(DecDate = decimal_date(Date)) %>%
#   step_impute_bag(CPI, Unemployment,
#                   impute_with = imp_vars(DecDate, Store))
# imputed_features <- juice(prep(feature_recipe))
# 
# 
# ## Join imputed features dataset with test and train data
# train_imputed <- train %>%
#   left_join(imputed_features, by = c("Store", "Date")) %>%
#   select(-IsHoliday.x)
# 
# test_imputed <- test %>%
#   left_join(imputed_features, by = c("Store", "Date")) %>%
#   select(-IsHoliday.x)
# 
# 
# # Choose the Store and Dept
# store <- 17
# dept <- 17
# 
# # Filter and Rename to match prophet syntax
# sd_train <- train_imputed %>%
#   filter(Store == store, Dept == dept) %>%
#   rename(y = Weekly_Sales, ds = Date)
# sd_test <- test_imputed %>%
#   filter(Store == store, Dept == dept) %>%
#   rename(ds = Date)
# 
# # Fit a prophet model
# prophet_model <- prophet() %>%
#   add_regressor("Unemployment") %>%
#   add_regressor("CPI") %>%
#   add_regressor("TotalMarkdown") %>%
#   fit.prophet(df = sd_train)
# 
# # Predict using fitted prophet model
# fitted_vals <- predict(prophet_model, df = sd_train)
# test_preds <- predict(prophet_model, df = sd_test)
# 
# # Plot fitted and forecast on same plot
# plot1 <- ggplot() +
#   geom_line(data = sd_train, mapping = aes(x = ds, y = y, color = "Data")) +
#   geom_line(data = fitted_vals, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
#   geom_line(data = test_preds, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
#   scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
#   labs(color = "", title = "Store-17, Dept-17")
# 
# 
# 
# 
# 
# 
# # Choose the Store and Dept
# store2 <- 13
# dept2 <- 7
# 
# # Filter and Rename to match prophet syntax
# sd_train2 <- train_imputed %>%
#   filter(Store == store2, Dept == dept2) %>%
#   rename(y = Weekly_Sales, ds = Date)
# sd_test2 <- test_imputed %>%
#   filter(Store == store2, Dept == dept2) %>%
#   rename(ds = Date)
# 
# # Fit a prophet model
# prophet_model <- prophet() %>%
#   add_regressor("Unemployment") %>%
#   add_regressor("CPI") %>%
#   add_regressor("TotalMarkdown") %>%
#   fit.prophet(df = sd_train2)
# 
# # Predict using fitted prophet model
# fitted_vals2 <- predict(prophet_model, df = sd_train2)
# test_preds2 <- predict(prophet_model, df = sd_test2)
# 
# # Plot fitted and forecast on same plot
# plot2 <- ggplot() +
#   geom_line(data = sd_train2, mapping = aes(x = ds, y = y, color = "Data")) +
#   geom_line(data = fitted_vals2, mapping = aes(x = as.Date(ds), y = yhat, color = "Fitted")) +
#   geom_line(data = test_preds2, mapping = aes(x = as.Date(ds), y = yhat, color = "Forecast")) +
#   scale_color_manual(values = c("Data" = "black", "Fitted" = "blue", "Forecast" = "red")) +
#   labs(color = "", title = "Store-13, Dept-7")
# 
# 
# 
# plot1 / plot2











# HEATON'S CODE 
# ============================================================================
## Libraries I need
# library(tidyverse)
# library(vroom)
# library(tidymodels)
# library(DataExplorer)
# 
# ## Read in the Data
# train <- vroom("train.csv")
# test <- vroom("test.csv")
# features <- vroom("features.csv")
# 
# #########
# ## EDA ##
# #########
# plot_missing(features)
# plot_missing(test)
# 
# ### Impute Missing Markdowns
# features <- features %>%
#   mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
#   mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
#   mutate(
#     MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
#     MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
#     MarkDown_Log   = log1p(MarkDown_Total)
#   ) %>%
#   select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)
# 
# ## Impute Missing CPI and Unemployment
# feature_recipe <- recipe(~., data=features) %>%
#   step_mutate(DecDate = decimal_date(Date)) %>%
#   step_impute_bag(CPI, Unemployment,
#                   impute_with = imp_vars(DecDate, Store))
# imputed_features <- juice(prep(feature_recipe))
# 
# ########################
# ## Merge the Datasets ##
# ########################
# 
# fullTrain <- left_join(train, imputed_features, by=c("Store", "Date")) %>%
#   select(-IsHoliday.y) %>%
#   rename(IsHoliday=IsHoliday.x) %>%
#   select(-MarkDown_Total)
# fullTest <- left_join(test, imputed_features, by=c("Store", "Date")) %>%
#   select(-IsHoliday.y) %>%
#   rename(IsHoliday=IsHoliday.x) %>%
#   select(-MarkDown_Total)
# plot_missing(fullTrain)
# plot_missing(fullTest)
# 
# ##################################
# ## Loop Through the Store-depts ## 
# ## and generate predictions.    ##
# ##################################
# all_preds <- tibble(Id = character(), Weekly_Sales = numeric())
# n_storeDepts <- fullTest %>% distinct(Store, Dept) %>% nrow()
# cntr <- 0
# 
# for(store in unique(fullTest$Store)){
#   
#   store_train <- fullTrain %>%
#     filter(Store==store)
#   store_test <- fullTest %>%
#     filter(Store==store)
#   
#   for(dept in unique(store_test$Dept)){
#     
#     ## Filter Test and Training Data
#     dept_train <- store_train %>%
#       filter(Dept==dept)
#     dept_test <- store_test %>%
#       filter(Dept==dept)
#     
#     ## If Statements for data scenarios
#     if(nrow(dept_train)==0){
#       
#       ## Predict 0
#       preds <- dept_test %>%
#         transmute(Id=paste(Store, Dept, Date, sep="_"),
#                   Weekly_Sales=0)
#       
#     } else if(nrow(dept_train) < 10 && nrow(dept_train) > 0){
#       
#       ## Predict the mean
#       preds <- dept_test %>%
#         transmute(Id=paste(Store, Dept, Date, sep="_"),
#                   Weekly_Sales=mean(dept_train$Weekly_Sales))
#       
#     } else {
#       
#       ## Fit a penalized regression model
#       my_recipe <- recipe(Weekly_Sales ~ ., data = dept_train) %>%
#         step_mutate(Holiday = as.integer(IsHoliday)) %>%
#         step_date(Date, features=c("month","year")) %>%
#         step_rm(Date, Store, Dept, IsHoliday)
#       prepped_recipe <- prep(my_recipe)
#       tst <- bake(prepped_recipe, new_data=dept_test)
#       
#       my_model <- rand_forest(mtry=3,
#                               trees=100,
#                               min_n=5) %>%
#         set_engine("ranger") %>%
#         set_mode("regression")
#       
#       my_wf <- workflow() %>%
#         add_recipe(my_recipe) %>%
#         add_model(my_model) %>%
#         fit(dept_train)
#       
#       preds <- dept_test %>%
#         transmute(Id=paste(Store, Dept, Date, sep="_"),
#                   Weekly_Sales=predict(my_wf, new_data = .) %>%
#                     pull(.pred))
#       
#     }
#     
#     ## Bind predictions together
#     all_preds <- bind_rows(all_preds,
#                            preds)
#     
#     ## Print out Progress
#     cntr <- cntr+1
#     cat("Store", store, "Department", dept, "Completed.",
#         round(100 * cntr / n_storeDepts, 1), "% overall complete.\n")
#     
#   } ## End Dept Loop
#   
# } ## End Store Loop
# 
# ## Write out after each store so I don't have to start over
# vroom_write(x=all_preds, 
#             file=paste0("Predictions.csv"), delim=",")
# 






