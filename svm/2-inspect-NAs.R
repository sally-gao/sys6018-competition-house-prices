# sum NAs for each column and store as dataframe named "NAs"
trainNAs <- train %>% 
  apply(2, function(col) sum(is.na(col))) %>% 
  as.data.frame()

colnames(trainNAs) <- c("numNAs")

# for test data: sum NAs for each column and store as dataframe named "NAs"
testNAs <- test %>% 
  apply(2, function(col) sum(is.na(col))) %>% 
  as.data.frame()

colnames(testNAs) <- c("numNAs")

# We only want to deal with NAs for variables that seem like promising predictors.
# Most of the variables that contain NAs are not promising predictors (see "3-explore").
# There were a few variables with NAs that seemed promising: GarageType, BsmtQual, GarageYrBlt, LotFrontage.

# GarageType and BsmtQual:

# The NAs are addressed in the modify-recode section.

# GarageYrBuilt and LotFronage:

ggplot(data=train) + geom_point(mapping = aes(x=GarageYrBlt,y=SalePrice), shape=1)
ggplot(data=train) + geom_point(mapping = aes(x=LotFrontage,y=SalePrice), shape=1)

# However, upon further inspection, I decided not to use these variables.
# YearBuilt is and GarageYrBlt share a roughly linear relationship, but YearBuilt is a better predictor of SalePrice.
# Similarly, LotFrontage can be predicted using LotArea, but LotArea is a better predictor than LotFrontage.
# Therefore, it would be meaningless to impute any missing values for either of these variables,
# because we found similar and superior predictors to explain SalePrice.

ggplot(data=train) + geom_point(mapping = aes(x=GarageYrBlt,y=YearBuilt), shape=1)
ggplot(data=train) + geom_point(mapping = aes(x=LotArea,y=LotFrontage), shape=1)
