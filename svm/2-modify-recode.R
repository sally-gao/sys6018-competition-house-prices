# GARAGES ----------------------------------------------------------------------

# Create hasGarage variable. It's 0 if there is no garage and 1 if there is a garage.
train_sml$hasGarage <- as.factor(ifelse(is.na(train_sml$GarageType), 0, 1))
test_sml$hasGarage <- as.factor(ifelse(is.na(test_sml$GarageType), 0, 1))

# FIREPLACES ----------------------------------------------------------------------

# Create hasFireplace variable.
train_sml$hasFireplace <- as.factor(ifelse(train_sml$Fireplaces==0, 0, 1))
test_sml$hasFireplace <- as.factor(ifelse(test_sml$Fireplaces==0, 0, 1))

# BSMTQUAL ----------------------------------------------------------------------

# Plotting BsmtQual against SalePrice reveals that N/A is similar to "Fa", so I'll just recode N/As as "Fa"
train_sml$BsmtQual[is.na(train_sml$BsmtQual)] <- "Fa"
test_sml$BsmtQual[is.na(test_sml$BsmtQual)] <- "Fa"

# Treat BsmtQual as factor
train_sml$BsmtQual <- as.factor(train_sml$BsmtQual)
test_sml$BsmtQual <- as.factor(test_sml$BsmtQual)

# NEIGHBORHOOD ----------------------------------------------------------------------

# Neighborhood has too many categories - put into smaller buckets
# split neighborhoods into quintiles based on median saleprice
neighborhoods <- summarise(group_by(train_sml, Neighborhood),
                           median.price = median(SalePrice))
neighborhoods <- neighborhoods %>% mutate(quintile = ntile(median.price, 5))

# create NeighborhoodQuintile variable in train_sml
train_sml$NeighborhoodQuintile <- recode_factor(train_sml$Neighborhood,
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

train_sml$NeighborhoodQuintile <- train_sml$NeighborhoodQuintile %>%
  factor(levels=c("1", "2", "3", "4", "5"), ordered=TRUE)

# Recode test data
test_sml$NeighborhoodQuintile <- recode_factor(test_sml$Neighborhood,
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

test_sml$NeighborhoodQuintile <- test_sml$NeighborhoodQuintile %>%
  factor(levels=c("1", "2", "3", "4", "5"), ordered=TRUE)

# We can also try splitting into sextiles instead of quintiles
neighborhoods <- neighborhoods %>% mutate(sextile = ntile(median.price, 6))

train_sml$NeighborhoodSextile <- recode_factor(train_sml$Neighborhood,
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

train_sml$NeighborhoodSextile <- train_sml$NeighborhoodSextile %>%
  factor(levels=c("1", "2", "3", "4", "5", "6"), ordered=TRUE)

test_sml$NeighborhoodSextile <- recode_factor(test_sml$Neighborhood,
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

test_sml$NeighborhoodSextile <- test_sml$NeighborhoodSextile %>%
  factor(levels=c("1", "2", "3", "4", "5", "6"), ordered=TRUE)

# SALE CONDITION ----------------------------------------------------------------------

# Recode SaleCondition so that anything other than Normal and Partial falls into "Other"
train_sml$SaleCondition <- recode_factor(train_sml$SaleCondition,
                                         Normal = "Normal",
                                         Partial = "Partial",
                                         .default = "Other")

test_sml$SaleCondition <- recode_factor(test_sml$SaleCondition,
                                        Normal = "Normal",
                                        Partial = "Partial",
                                        .default = "Other")

# BEDROOM.ABV.GR ----------------------------------------------------------------------

# Recode BedroomAbvGr so that anything above 3 falls into "4+"
train_sml$BedroomAbvGr <- recode_factor(train_sml$BedroomAbvGr,
                                        "0" = "0",
                                        "1" = "1",
                                        "2" = "2",
                                        "3" = "3",
                                        .default = "4+",
                                        .ordered = TRUE)

test_sml$BedroomAbvGr <- recode_factor(test_sml$BedroomAbvGr,
                                       "0" = "0",
                                       "1" = "1",
                                       "2" = "2",
                                       "3" = "3",
                                       .default = "4+",
                                       .ordered = TRUE)

# TOT.RMS.ABV.GRD ----------------------------------------------------------------------

# Treat TotRmsAbvGrd as factor and add collapsed buckets "2-3" and "10+" so the levels match in test and train
train_sml$TotRmsAbvGrd <- recode_factor(train_sml$TotRmsAbvGrd,
                                    "2" = "2-3", "3" = "2-3", "4" = "4", "5" = "5", "6" = "6", "7" = "7",
                                    "8" = "8", "9" = "9",
                                    .default = "10+",
                                    .ordered = TRUE)

test_sml$TotRmsAbvGrd <- recode_factor(test_sml$TotRmsAbvGrd,
                                   "3" = "2-3", "4" = "4", "5" = "5", "6" = "6", "7" = "7",
                                   "8" = "8", "9" = "9",
                                   .default = "10+",
                                   .ordered = TRUE)

# OVERALL.QUAL  ----------------------------------------------------------------------

# Recode OverallQual so 1 and 2 are treated as "1-2"

train_sml$OverallQual <- recode_factor(train_sml$OverallQual,
                                        "2" = "1-2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7",
                                        "8" = "8", "9" = "9", "10"= "10",
                                        .ordered = TRUE)

test_sml$OverallQual <- recode_factor(test_sml$OverallQual,
                                       "1" = "1-2", "2" = "1-2", "3" = "3", "4" = "4", "5" = "5", "6" = "6", "7" = "7",
                                       "8" = "8", "9" = "9", "10"= "10",
                                       .ordered = TRUE)

# OTHER FACTORS ----------------------------------------------------------------------

# Treat FullBath as factor
train_sml$FullBath <- as.factor(train_sml$FullBath)
test_sml$FullBath <- as.factor(test_sml$FullBath)

# Treat CentralAir as factor
train_sml$CentralAir <- as.factor(train_sml$CentralAir)
test_sml$CentralAir <- as.factor(test_sml$CentralAir)

# Treat MSZoning as factor
train_sml$MSZoning <- as.factor(train_sml$MSZoning)
test_sml$MSZoning <- as.factor(test_sml$MSZoning)

# Treat ExterQual as factor
train_sml$ExterQual <- as.factor(train_sml$ExterQual)
test_sml$ExterQual <- as.factor(test_sml$ExterQual)


# LOG LOT AREA ----------------------------------------------------------------------

train_sml$LogLotArea <- log(train_sml$LotArea)
test_sml$LogLotArea <- log(test_sml$LotArea)

# DECADE BUILT ----------------------------------------------------------------------

train_sml <- train_sml %>% 
  mutate(DecadeBuilt = round(YearBuilt / 10))

train_sml$DecadeBuilt <- as.factor(train_sml$DecadeBuilt)

test_sml <- test_sml %>% 
  mutate(DecadeBuilt = round(YearBuilt / 10))

test_sml$DecadeBuilt <- as.factor(test_sml$DecadeBuilt)

train_sml$DecadeBuilt <- recode_factor(train_sml$DecadeBuilt,
                                       "187"="188")
