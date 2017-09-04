# Assignment: Competition 2 -- Housing Prices
# Name: Kennan Grant
# ID: khg3je


# Read in Data ------------------------------------------------------------

# load libraries
library(tidyverse)

# read in train
train <- read_csv("train.csv")
train

# read in test
test <- read_csv("test.csv")
test


# Explore Data ------------------------------------------------------------

# inspect NA's
sapply(train, function(x) sum(is.na(x)))

# inspect univariate relationships with response
for (predictor in names(train)) {
  print(train %>%
          ggplot(aes_string(x=predictor, y="SalePrice")) +
          geom_point())
}


# Clean Data --------------------------------------------------------------

# impute character 'NA' in BsmtQual
train$BsmtQual <- if_else(is.na(train$BsmtQual), "NA", train$BsmtQual)
test$BsmtQual <- if_else(is.na(test$BsmtQual), "NA", test$BsmtQual)

# impute median TotalBsmtSF value where NA
test$TotalBsmtSF <- if_else(is.na(test$TotalBsmtSF)
                            ,median(test$TotalBsmtSF, na.rm = TRUE)
                            ,as.double(test$TotalBsmtSF))

# impute largest MSZoning class where NA
test$MSZoning <- if_else(is.na(test$MSZoning), "RL", test$MSZoning)


# Create new variables ----------------------------------------------------


# split neighborhoods into quintiles based on median saleprice
neighborhoods <- summarise(group_by(train, Neighborhood), # for train
                           median.price = median(SalePrice))
neighborhoods <- neighborhoods %>% mutate(quintile = ntile(median.price, 5))


# create NeighborhoodQuintile variable in train
train$NeighborhoodQuintile <- recode_factor(train$Neighborhood,
                                                Blmngtn = 4,
                                                Blueste = 1,
                                                BrDale = 1,
                                                BrkSide = 2,
                                                ClearCr = 4,
                                                CollgCr = 4,
                                                Crawfor = 4,
                                                Edwards = 2,
                                                Gilbert = 3,
                                                IDOTRR = 1,
                                                MeadowV = 1,
                                                Mitchel = 3,
                                                NAmes = 2,
                                                NoRidge = 5,
                                                NPkVill = 3,
                                                NridgHt = 5,
                                                NWAmes = 3,
                                                OldTown = 1,
                                                Sawyer = 2,
                                                SawyerW = 3,
                                                Somerst = 4,
                                                StoneBr = 5,
                                                SWISU = 2,
                                                Timber = 5,
                                                Veenker = 5)

train$NeighborhoodQuintile <- train$NeighborhoodQuintile %>%
  factor(levels=c("1", "2", "3", "4", "5"), ordered=TRUE)

# create NeighborhoodQuintile variable in test
test$NeighborhoodQuintile <- recode_factor(test$Neighborhood,
                                            Blmngtn = 4,
                                            Blueste = 1,
                                            BrDale = 1,
                                            BrkSide = 2,
                                            ClearCr = 4,
                                            CollgCr = 4,
                                            Crawfor = 4,
                                            Edwards = 2,
                                            Gilbert = 3,
                                            IDOTRR = 1,
                                            MeadowV = 1,
                                            Mitchel = 3,
                                            NAmes = 2,
                                            NoRidge = 5,
                                            NPkVill = 3,
                                            NridgHt = 5,
                                            NWAmes = 3,
                                            OldTown = 1,
                                            Sawyer = 2,
                                            SawyerW = 3,
                                            Somerst = 4,
                                            StoneBr = 5,
                                            SWISU = 2,
                                            Timber = 5,
                                            Veenker = 5)

test$NeighborhoodQuintile <- test$NeighborhoodQuintile %>%
  factor(levels=c("1", "2", "3", "4", "5"), ordered=TRUE)


# add new variables to train
train_mod <- train %>% 
  mutate(Fireplaces_YoN = if_else(Fireplaces == 0, 0, 1)
         ,Log_LA = log(LotArea)
         ,nearest_decade = round(YearBuilt / 10)
         ,remod_add_decade = round(YearRemodAdd / 10))

# add new variables to test
test_mod <- test %>% 
  mutate(Fireplaces_YoN = if_else(Fireplaces == 0, 0, 1)
         ,Log_LA = log(LotArea)
         ,nearest_decade = round(YearBuilt / 10)
         ,remod_add_decade = round(YearRemodAdd / 10))


# Cross-Validation ----------------------------------------------------------

library(caret)

formula_for_cv = formula(SalePrice ~ 
                           Fireplaces_YoN 
                         + Log_LA 
                         + OverallQual
                         + NeighborhoodQuintile
                         + BsmtQual
                         + MSZoning
                         + TotalBsmtSF
                         + CentralAir
                         + remod_add_decade
                         + ExterQual
                         + FullBath
                         + TotRmsAbvGrd
                         + BedroomAbvGr)

# set seed for reproducibility
set.seed(345)

# perform cross-validation and fit final model
my_model <- train(formula_for_cv, train_mod, method = "lm"
               , trControl = trainControl(method = "cv", number = 5
                                          , verboseIter = TRUE)
               )

# extract average root mean squared error of folds
mean_rmse_all_folds <- my_model$results$RMSE
mean_rmse_all_folds # compare this for each model explored (35264.41 == best yet)

# view model summary
summary(my_model)

#
# Notes on model selection:
#
# After univariate plots were explored, promising variables (variables that appeared to be associated with the response -- SalePrice -- were put in model. Collinear variables were identified and removed as necessary.  Variables with statistically significant coefficient were retained and then a model iteration was tested.  RMSE was calculated.  The RMSE of different models were compared using k-fold cross-validation (average test RMSE across all folds), and the model with the lowest test RMSE was selected.
#

# Make Final Predictions for Submission ---------------------------------------

# make predictionson FINAL-TEST
my_pred2 <- predict(my_model, newdata=test_mod)

# create tibble for output
output <- as_tibble(cbind(test_mod$Id, my_pred2))
colnames(output) <- c("Id","SalePrice") # rename cols

# write final output to file
write.csv(output, file = "khg3je_submission7.csv", row.names = FALSE)





