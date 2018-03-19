# Loading data ####
clean_train <- read.csv("./Data/clean_train.csv", stringsAsFactors = FALSE)
clean_test <- read.csv("./Data/clean_test.csv", stringsAsFactors = FALSE)

# Feature engineering ####

set.seed(42)

# Transformations that have no degrees of freedom
# Interesting interactions:
# - OverallQual x OverallCond
# - YearBuilt x YearRemodAdd
# - PorchType x PorchArea
# - BsmtQual x BsmtCond
# - BsmFinType1 x BsmtFinSF1
# - BsmFinType2 x BsmtFinSF2
# - KitchenAbvGr x KitchenQual
# - Fireplaces x FireplaceQu
# - PoolArea x PoolQc
# - Fence x LotSideLength
# - MoSold; use cyclic spline or transform to polar positions

independent_engineer <- function(my_data){
  
  eng <- my_data
  
  # Impute mising 'GarageYrBuilt' data by selecting random possible year ##
  # FIXME: this is a terrible solution; the data is not truly missing
  end_year <- max(as.numeric(eng$GarageYrBlt), na.rm = T)
  
  for (i in 1:nrow(eng)){
    if (eng[i, 'GarageYrBlt'] == 'None'){
      eng[i, 'GarageYrBlt'] <- sample(eng[i, 'YearBuilt']:end_year, 1)
    }
  }
  
  eng['GarageYrBlt'] <- lapply(eng['GarageYrBlt'], as.numeric)
    
  # Remove low information columns ##
  # Possibile choices:
  # Utilities, MiscFeature, Street, Heating
  within(eng, rm(Utilities, MiscFeature))
      
  # Combine imbalanced columns ##
  # Possibile choices:
  # MSZoning,
  # RoofStyle, RoofMatl, 
  # SaleType, SaleCondition, 
  # Heating
    
  
  # ClyTile	Clay or Tile
  # CompShg	Standard (Composite) Shingle
  # Membran	Membrane
  # Metal	Metal
  # Roll	Roll
  # Tar&Grv	Gravel & Tar
  # WdShake	Wood Shakes
  # WdShngl	Wood Shingles
  fancy_mat <- c('ClyTile', 'Membran', 'Metal')
  cheap_mat <- c('Roll', 'Tar&Grv', 'WdShake', 'WoodShngl')
  eng[eng$RoofMatl %in% fancy_mat, 'RoofMatl'] <- 'Fancy'
  eng[eng$RoofMatl %in% cheap_mat,'RoofMatl'] <- 'Cheap'
    
  
  # WD 	Warranty Deed - Conventional
  # CWD	Warranty Deed - Cash
  # VWD	Warranty Deed - VA Loan
  # New	Home just constructed and sold
  # COD	Court Officer Deed/Estate
  # Con	Contract 15% Down payment regular terms
  # ConLw	Contract Low Down payment and low interest
  # ConLI	Contract Low Interest
  # ConLD	Contract Low Down
  # Oth	Other
  misc_saletype <- c('Con', 'ConLD', 'ConLI', 'ConLW', 'CWD', 'Oth')
  eng[eng$SaleType %in% misc_saletype,'SaleType'] <- 'Misc'
  
  
  # Derived variables ##
   
  # Square root of lot area
  eng$LotSide <- sqrt(eng$LotArea)
  
  # Fraction unfinished basement
  # BsmtUnfSF / TotalBsmtSF
  eng$BsmtUnfPercent <- eng$BsmtUnfSF / eng$TotalBsmtSF
  eng$BsmtUnfPercent[is.na(eng$BsmtUnfPercent)] <- 0
  
  # Any low quality finished
  # LowQualFinSF > 0
  eng$LowQualFinPresence <- (eng$LowQualFin > 0)
  
  return (eng)
}

engineered_train <- independent_engineer(clean_train)
engineered_test <- independent_engineer(clean_test)

# Transformations that are learned from the data
dependent_engineer <- function(train, test){
  
  eng_train <- train
  eng_test <- test
  
  # Impute missing values
  # LotFrontage is the biggest offender by far
  # FIXME: still some missing values??
  frontage_model <- lm(LotFrontage ~ LotArea + sqrt(LotArea) + LotShape + LotConfig, data=train)
  
  frontage_train <- predict(frontage_model, newdata = train[is.na(train$LotFrontage),])
  eng_train[is.na(train$LotFrontage),'LotFrontage'] <- frontage_train
  
  frontage_test <- predict(frontage_model, newdata = test[is.na(test$LotFrontage),])
  eng_test[is.na(test$LotFrontage),'LotFrontage'] <- frontage_test
  
  # Impute rest of missing values
  # TODO: implement knn-imputation
  
  
  return (list(eng_train, eng_test))
}

dependent_data <- dependent_engineer(engineered_train, engineered_test)
final_train <- dependent_data[[1]]
final_test  <- dependent_data[[2]]

# Saving data ####
write.csv(final_train, "./Data/engineered_train.csv", row.names=FALSE)
write.csv(final_test,  "./Data/engineered_test.csv", row.names=FALSE)