# SIMPLE SVM -------------------------------------------------------------------

# Create fulltrain and fulltest dataframes containing only the variables we want to use

fulltrain <- data.frame(LogLotArea = train.mod$LogLotArea,
                        SalePrice = train.mod$SalePrice,
                        YearBuilt = train.mod$YearBuilt,
                        NeighborhoodSextile = train.mod$NeighborhoodSextile,
                        hasGarage = train.mod$hasGarage,
                        hasFireplace = train.mod$hasFireplace,
                        SaleCondition = train.mod$SaleCondition,
                        TotRmsAbvGrd = train.mod$TotRmsAbvGrd,
                        BedroomAbvGr = train.mod$BedroomAbvGr,
                        FullBath = train.mod$FullBath,
                        GrLivArea = train.mod$GrLivArea,
                        CentralAir = train.mod$CentralAir,
                        OverallQual = train.mod$OverallQual,
                        BsmtQual = train.mod$BsmtQual,
                        TotalBsmtSF = train.mod$TotalBsmtSF,
                        ExterQual = train.mod$ExterQual)

fulltest <- data.frame(LogLotArea = test.mod$LogLotArea, 
                       YearBuilt = test.mod$YearBuilt,
                       NeighborhoodSextile = test.mod$NeighborhoodSextile,
                       hasGarage = test.mod$hasGarage,
                       hasFireplace = test.mod$hasFireplace,
                       SaleCondition = test.mod$SaleCondition,
                       TotRmsAbvGrd = test.mod$TotRmsAbvGrd,
                       BedroomAbvGr = test.mod$BedroomAbvGr,
                       FullBath = test.mod$FullBath,
                       GrLivArea = test.mod$GrLivArea,
                       CentralAir = test.mod$CentralAir,
                       OverallQual = test.mod$OverallQual,
                       BsmtQual = test.mod$BsmtQual,
                       TotalBsmtSF = test.mod$TotalBsmtSF,
                       ExterQual = test.mod$ExterQual)

# Run SVM without tuning
simple_svm_full <- svm(SalePrice ~
                         LogLotArea
                       +YearBuilt
                       +hasGarage
                       +hasFireplace
                       +SaleCondition
                       +TotRmsAbvGrd
                       +BedroomAbvGr
                       +FullBath
                       +GrLivArea
                       +CentralAir
                       +OverallQual
                       +NeighborhoodSextile
                       +BsmtQual
                       +TotalBsmtSF
                       +ExterQual,
                       fulltrain)

# Predict
svmpreds_full <- predict(simple_svm_full, fulltest)

mypreds <- data.frame(Id = test.mod$Id, SalePrice = svmpreds_full)
write.table(mypreds, file = "svm_submission_simple1.csv", row.names=F, sep=",")


# TUNED SVM -------------------------------------------------------------------

# Run SVM with tuning (epsilon and cost selected based on inspection of plot(svm.tune))

svm.tune <- tune(svm, SalePrice ~ 
                   LogLotArea
                 +YearBuilt
                 +hasGarage
                 +hasFireplace
                 +SaleCondition
                 +TotRmsAbvGrd
                 +BedroomAbvGr
                 +FullBath
                 +GrLivArea
                 +CentralAir
                 +OverallQual
                 +NeighborhoodSextile
                 +BsmtQual
                 +TotalBsmtSF
                 +ExterQual,
                 data=fulltrain,
                 ranges = list(epsilon = seq(0,0.3,0.05), cost = 2^(2:6)))

# Predict
svmtune.preds <- predict(svm.tune$best.model, fulltest)

mytunedpreds <- data.frame(Id = test.mod$Id, SalePrice = svmtune.preds)
write.table(mytunedpreds, file = "svm_submission_tuned1.csv", row.names=F, sep=",")
