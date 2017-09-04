library(tidyverse)

train <- read_csv("train.csv")
train

test <- read_csv("test.csv")
test


# impute for NA -----------------------------------------------------------

# train %>% 
#   select(BsmtQual) %>% 
#   group_by(BsmtQual) %>% 
#   summarise(n = n())

train$BsmtQual <- if_else(is.na(train$BsmtQual), "NA", train$BsmtQual)
test$BsmtQual <- if_else(is.na(test$BsmtQual), "NA", test$BsmtQual)


test$TotalBsmtSF <- if_else(is.na(test$TotalBsmtSF)
                            ,median(test$TotalBsmtSF, na.rm = TRUE)
                            ,as.double(test$TotalBsmtSF))
test$MSZoning <- if_else(is.na(test$MSZoning), "RL", test$MSZoning)


# split training into train and test --------------------------------------

# create ID col
train <- train %>%
  mutate(ID = row_number()) 

# set seed for reproducibility
set.seed(345)

# select 65% of rows for training
train_sml <- train %>% 
  sample_frac(size = 0.65) 

# select other 35% of rows for test
test_sml <- train %>% 
  filter(!(ID %in% train_sml[["ID"]]))


# Explore -----------------------------------------------------------------


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


# add new variables
train_sml_varsadded <- train_sml %>% 
  mutate(Fireplaces_YoN = if_else(Fireplaces == 0, 0, 1)
         ,Log_LA = log(LotArea)
         ,nearest_decade = round(YearBuilt / 10)
         ,remod_add_decade = round(YearRemodAdd / 10))

test_sml_varsadded <- test_sml %>% 
  mutate(Fireplaces_YoN = if_else(Fireplaces == 0, 0, 1)
         ,Log_LA = log(LotArea)
         ,nearest_decade = round(YearBuilt / 10)
         ,remod_add_decade = round(YearRemodAdd / 10))

train_mod <- train %>% 
  mutate(Fireplaces_YoN = if_else(Fireplaces == 0, 0, 1)
         ,Log_LA = log(LotArea)
         ,nearest_decade = round(YearBuilt / 10)
         ,remod_add_decade = round(YearRemodAdd / 10))

test_mod <- test %>% 
  mutate(Fireplaces_YoN = if_else(Fireplaces == 0, 0, 1)
         ,Log_LA = log(LotArea)
         ,nearest_decade = round(YearBuilt / 10)
         ,remod_add_decade = round(YearRemodAdd / 10))




# fit final model
my_model <- lm(SalePrice ~ 
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
               + BedroomAbvGr
               , data = train_mod )

summary(my_model)

anova(my_model)

# make predictions on TRAIN-TEST
my_pred <- predict(my_model, newdata=test_sml_varsadded)

# calculate MSE
my_MSE <- mean((my_pred - test_sml_varsadded$SalePrice)^2)

# make predictionson FINAL-TEST
my_pred2 <- predict(my_model, newdata=test_mod)


# create tibble for output
output <- as_tibble(cbind(test_mod$Id, my_pred2))
colnames(output) <- c("Id","SalePrice") # rename cols

# remove negative predictions
output <- output %>%
  mutate(SalePrice = if_else(SalePrice < 0, 0, SalePrice))
sum(output$SalePrice < 0) # check count of negative predictions

# write final output to file
write.csv(output, file = "khg3je_submission6.csv", row.names = FALSE)




# other exploratory stuff -------------------------------------------------

# count NA number
sapply(train_sml, function(x) sum(is.na(x)))

sapply(my_model$model, function(x) sum(is.na(x)))

sapply(train_mod, function(x) sum(is.na(x)))


# drop 1 NA row of TotalBsmtSF, drop 4 MSZoning NA row, 

train %>% 
  group_by(MasVnrType) %>% 
  summarise(n = n(), median(SalePrice)) %>% 
  as.data.frame()


for (predictor in names(train)) {
  
  
  print(train %>%
    ggplot(aes_string(x=predictor, y="SalePrice")) +
    geom_point())
  
}


sum(is.na(output$SalePrice))


# RoofStyle, MasVnrType
  
train %>%
  mutate(FullBath = as.factor(TotRmsAbvGrd)) %>% 
        ggplot(aes(x=TotRmsAbvGrd, y=SalePrice)) +
        geom_point() +
  geom_boxplot()



