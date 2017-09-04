test <- read_csv("test.csv")

# GARAGES ----------------------------------------------------------------------

# Create hasGarage variable. It's 0 if there is no garage and 1 if there is a garage.
train.mod <- train %>%
  mutate(hasGarage = factor(ifelse(is.na(GarageType), 0, 1)))

test.mod <- test %>%
  mutate(hasGarage = factor(ifelse(is.na(GarageType), 0, 1)))

# FIREPLACES ----------------------------------------------------------------------

# Create hasFireplace variable.
train.mod$hasFireplace <- as.factor(ifelse(train.mod$Fireplaces==0, 0, 1))
test.mod$hasFireplace <- as.factor(ifelse(test.mod$Fireplaces==0, 0, 1))

# BSMTQUAL AND TOTALBSMTSF ----------------------------------------------------------------------

# There's 1 NA in test.mod$TotalBsmtSF, so recode as 0.
test.mod$TotalBsmtSF[is.na(test.mod$TotalBsmtSF)] <- 0

# Plotting BsmtQual against SalePrice reveals that N/A is similar to "Fa", so I'll just recode N/As as "Fa"
train.mod$BsmtQual[is.na(train.mod$BsmtQual)] <- "Fa"
test.mod$BsmtQual[is.na(test.mod$BsmtQual)] <- "Fa"

# Treat BsmtQual as factor
train.mod$BsmtQual <- as.factor(train.mod$BsmtQual)
test.mod$BsmtQual <- as.factor(test.mod$BsmtQual)

# NEIGHBORHOOD ----------------------------------------------------------------------

# Neighborhood has too many categories - put into smaller buckets
# split neighborhoods into quintiles based on median saleprice
neighborhoods <- summarise(group_by(train.mod, Neighborhood),
                           median.price = median(SalePrice))
neighborhoods <- neighborhoods %>% mutate(quintile = ntile(median.price, 5))

# create NeighborhoodQuintile variable in train.mod
train.mod$NeighborhoodQuintile <- recode_factor(train.mod$Neighborhood,
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

train.mod$NeighborhoodQuintile <- train.mod$NeighborhoodQuintile %>%
  factor(levels=c("1", "2", "3", "4", "5"), ordered=TRUE)

# Recode test data
test.mod$NeighborhoodQuintile <- recode_factor(test.mod$Neighborhood,
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

test.mod$NeighborhoodQuintile <- test.mod$NeighborhoodQuintile %>%
  factor(levels=c("1", "2", "3", "4", "5"), ordered=TRUE)

# We can also try splitting into sextiles instead of quintiles
neighborhoods <- neighborhoods %>% mutate(sextile = ntile(median.price, 6))

train.mod$NeighborhoodSextile <- recode_factor(train.mod$Neighborhood,
                                               Blmngtn = 4,
                                               Blueste = 1,
                                               BrDale = 1,
                                               BrkSide = 2,
                                               ClearCr = 5,
                                               CollgCr = 4,
                                               Crawfor = 5,
                                               Edwards = 2,
                                               Gilbert = 4,
                                               IDOTRR = 1,
                                               MeadowV = 1,
                                               Mitchel = 3,
                                               NAmes = 2,
                                               NoRidge = 6,
                                               NPkVill = 3,
                                               NridgHt = 6,
                                               NWAmes = 3,
                                               OldTown = 1,
                                               Sawyer = 2,
                                               SawyerW = 4,
                                               Somerst = 5,
                                               StoneBr = 6,
                                               SWISU = 3,
                                               Timber = 5,
                                               Veenker = 6)

train.mod$NeighborhoodSextile <- train.mod$NeighborhoodSextile %>%
  factor(levels=c("1", "2", "3", "4", "5", "6"), ordered=TRUE)

test.mod$NeighborhoodSextile <- recode_factor(test.mod$Neighborhood,
                                              Blmngtn = 4,
                                              Blueste = 1,
                                              BrDale = 1,
                                              BrkSide = 2,
                                              ClearCr = 5,
                                              CollgCr = 4,
                                              Crawfor = 5,
                                              Edwards = 2,
                                              Gilbert = 4,
                                              IDOTRR = 1,
                                              MeadowV = 1,
                                              Mitchel = 3,
                                              NAmes = 2,
                                              NoRidge = 6,
                                              NPkVill = 3,
                                              NridgHt = 6,
                                              NWAmes = 3,
                                              OldTown = 1,
                                              Sawyer = 2,
                                              SawyerW = 4,
                                              Somerst = 5,
                                              StoneBr = 6,
                                              SWISU = 3,
                                              Timber = 5,
                                              Veenker = 6)

test.mod$NeighborhoodSextile <- test.mod$NeighborhoodSextile %>%
  factor(levels=c("1", "2", "3", "4", "5", "6"), ordered=TRUE)

# SALE CONDITION ----------------------------------------------------------------------

# Recode SaleCondition so that anything other than Normal and Partial falls into "Other"
train.mod$SaleCondition <- recode_factor(train.mod$SaleCondition,
                                         Normal = "Normal",
                                         Partial = "Partial",
                                         .default = "Other")

test.mod$SaleCondition <- recode_factor(test.mod$SaleCondition,
                                        Normal = "Normal",
                                        Partial = "Partial",
                                        .default = "Other")

# BEDROOM.ABV.GR ----------------------------------------------------------------------

# Recode BedroomAbvGr so that anything above 3 falls into "4+"
train.mod$BedroomAbvGr <- recode_factor(train.mod$BedroomAbvGr,
                                        "0" = "0",
                                        "1" = "1",
                                        "2" = "2",
                                        "3" = "3",
                                        .default = "4+",
                                        .ordered = TRUE)

test.mod$BedroomAbvGr <- recode_factor(test.mod$BedroomAbvGr,
                                       "0" = "0",
                                       "1" = "1",
                                       "2" = "2",
                                       "3" = "3",
                                       .default = "4+",
                                       .ordered = TRUE)

# TOT.RMS.ABV.GRD ----------------------------------------------------------------------

# Treat TotRmsAbvGrd as factor and add collapsed buckets "2-3" and "10+"
train.mod$TotRmsAbvGrd <- recode_factor(train.mod$TotRmsAbvGrd,
                                        "2" = "2-3", "3" = "2-3", "4" = "4", "5" = "5", "6" = "6", "7" = "7",
                                        "8" = "8", "9" = "9",
                                        .default = "10+",
                                        .ordered = TRUE)

test.mod$TotRmsAbvGrd <- recode_factor(test.mod$TotRmsAbvGrd,
                                       "3" = "2-3", "4" = "4", "5" = "5", "6" = "6", "7" = "7",
                                       "8" = "8", "9" = "9",
                                       .default = "10+",
                                       .ordered = TRUE)

# OVERALL.QUAL  ----------------------------------------------------------------------

# Recode OverallQual so 1 and 2 are treated as "1-2"

train.mod$OverallQual <- recode_factor(train.mod$OverallQual,
                                       "1" = "1-2", "2" = "1-2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7",
                                       "8" = "8", "9" = "9", "10"= "10",
                                       .ordered = TRUE)

test.mod$OverallQual <- recode_factor(test.mod$OverallQual,
                                      "1" = "1-2", "2" = "1-2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7",
                                      "8" = "8", "9" = "9", "10"= "10",
                                      .ordered = TRUE)

# FULL BATH ----------------------------------------------------------------------

# Treat FullBath as factor and recode so 3 and 4 are both "3+"
train.mod$FullBath <- recode_factor(train.mod$FullBath,
                                "0" = "0",
                                "1" = "1",
                                "2" = "2",
                                .default = "3+",
                                .ordered = TRUE)

test.mod$FullBath <- recode_factor(test.mod$FullBath,
                               "0" = "0",
                               "1" = "1",
                               "2" = "2",
                               .default = "3+",
                               .ordered = TRUE)
# OTHER FACTORS ----------------------------------------------------------------------

# Treat CentralAir as factor
train.mod$CentralAir <- as.factor(train.mod$CentralAir)
test.mod$CentralAir <- as.factor(test.mod$CentralAir)

# Treat MSZoning as factor
train.mod$MSZoning <- as.factor(train.mod$MSZoning)
test.mod$MSZoning <- as.factor(test.mod$MSZoning)

# Treat ExterQual as factor
train.mod$ExterQual <- as.factor(train.mod$ExterQual)
test.mod$ExterQual <- as.factor(test.mod$ExterQual)


# LOG LOT AREA ----------------------------------------------------------------------

train.mod$LogLotArea <- log(train.mod$LotArea)
test.mod$LogLotArea <- log(test.mod$LotArea)

# DECADE BUILT ----------------------------------------------------------------------

train.mod <- train.mod %>% 
  mutate(DecadeBuilt = round(YearBuilt / 10))

train.mod$DecadeBuilt <- as.factor(train.mod$DecadeBuilt)

test.mod <- test.mod %>% 
  mutate(DecadeBuilt = round(YearBuilt / 10))

test.mod$DecadeBuilt <- as.factor(test.mod$DecadeBuilt)

train.mod$DecadeBuilt <- recode_factor(train.mod$DecadeBuilt,
                                       "187"="188")
