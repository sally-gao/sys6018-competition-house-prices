
for (predictor in names(train)) {
  print(train %>%
          ggplot(aes_string(x=predictor, y="SalePrice")) +
          geom_point())
}

# Promising predictors include:
# LotArea 
# YearBuilt
# GarageType (not the GarageTypes per say, but whether or not Garage is NA)
# Fireplaces (zero or non-zero)
# SaleCondition
# TotRmsAbvGrd
# BedroomAbvGr
# FullBath
# GrLivArea
# CentralAir
# OverallQual
# Neighborhood
# BsmtQual
# TotalBsmtSF
# ExterQual