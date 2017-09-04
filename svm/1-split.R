library(tidyverse)
train <- read_csv("train.csv")

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