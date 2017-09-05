library(e1071)

#-------------------------------------------------------------------
# Linear model test
lin <- lm(SalePrice ~ YearBuilt+LogLotArea+NeighborhoodSextile+hasGarage+hasFireplace
          + SaleCondition + TotRmsAbvGrd + FullBath + GrLivArea + BedroomAbvGr + CentralAir+OverallQual, data=train_sml)
linpreds <- predict(lin, test_sml)

lin.error <- test_sml$SalePrice - linpreds
lin.rmse <- sqrt(mean(lin.error^2))
lin.rmse # 32427.39

# -------------------------------------------------------------------

# The SVM function only works if we build two separate dataframes containing no unused variables.

svm_data <- data.frame(LogLotArea = train_sml$LogLotArea,
                       SalePrice = train_sml$SalePrice,
                       YearBuilt = train_sml$YearBuilt,
                       NeighborhoodSextile = train_sml$NeighborhoodSextile,
                       hasGarage = train_sml$hasGarage,
                       hasFireplace = train_sml$hasFireplace,
                       SaleCondition = train_sml$SaleCondition,
                       TotRmsAbvGrd = train_sml$TotRmsAbvGrd,
                       BedroomAbvGr = train_sml$BedroomAbvGr,
                       FullBath = train_sml$FullBath,
                       GrLivArea = train_sml$GrLivArea,
                       CentralAir = train_sml$CentralAir,
                       OverallQual = train_sml$OverallQual,
                       BsmtQual = train_sml$BsmtQual,
                       TotalBsmtSF = train_sml$TotalBsmtSF,
                       ExterQual = train_sml$ExterQual)

test_data <- data.frame(LogLotArea = test_sml$LogLotArea, 
                        SalePrice = test_sml$SalePrice, 
                        YearBuilt = test_sml$YearBuilt,
                        NeighborhoodSextile = test_sml$NeighborhoodSextile,
                        hasGarage = test_sml$hasGarage,
                        hasFireplace = test_sml$hasFireplace,
                        SaleCondition = test_sml$SaleCondition,
                        TotRmsAbvGrd = test_sml$TotRmsAbvGrd,
                        BedroomAbvGr = test_sml$BedroomAbvGr,
                        FullBath = test_sml$FullBath,
                        GrLivArea = test_sml$GrLivArea,
                        CentralAir = test_sml$CentralAir,
                        OverallQual = test_sml$OverallQual,
                        BsmtQual = test_sml$BsmtQual,
                        TotalBsmtSF = test_sml$TotalBsmtSF,
                        ExterQual = test_sml$ExterQual)

simple_svm <- svm(SalePrice ~ LogLotArea+YearBuilt+hasGarage+hasFireplace
                  +SaleCondition+TotRmsAbvGrd+BedroomAbvGr+FullBath
                  +GrLivArea+CentralAir+OverallQual+NeighborhoodSextile
                  +BsmtQual+TotalBsmtSF+ExterQual, svm_data)

svmpreds <- predict(simple_svm, test_data)

svm.error <- test_data$SalePrice - svmpreds
svm.rmse <- sqrt(mean(svm.error^2))
svm.rmse # 23494.8 -- better than linear regression.