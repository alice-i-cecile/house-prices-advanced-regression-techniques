# Loading data ####
train <- read.csv("./Data/train.csv", stringsAsFactors = FALSE)
test <- read.csv("./Data/test.csv", stringsAsFactors = FALSE)

# Cleaning data ####

# ID: Good
# MSSubclass: Change to factor
# MSZoning: Good, maybe combine
# LotFrontage: Some missing values; sanity check frontage vs. area by LotShape
# LotArea: Good
# Street: Very imbalanced; consider discarding
# Alley: False NAs
# LandContour: Good
# Utilities: Extremely imbalanced; discard
# LotConfig: Good
# LandSlope: Good
# Neighbourhood: Good
# Condition1, Condition2: Combine, change to 1-hot encoding
# BldgType: Good
# HouseStyle: Good
# OverallQual: Good; treat as numeric
# OverallCond: Good; treat as numeric
# YearBuilt: Good; treat as numeric
# YearRemodAdd: Good; treat as numeric
# RoofStyle: Some classes very rare
# RoofMatl: Some classes very rare
# Exterior1st, Exterior2nd: Combine, change to 2-hot encoding
# MasVnrType: True missing values; impute
# MasVnrArea: True missing values; impute
# ExterQual: Change to ordered factor or numeric
# ExterCond: Change to ordered factor or numeric
# Foundation: Some classes very rare
# BsmtQual: False NAs
# BsmtCond: False NAs
# BsmtExposure: False NA's, one true NA
# BsmtFinType1, BsmtFinType2: False NAs, one true NA
# BsmtFinSF1, BsmtFinSF2, BsmtUnfSF, TotalBsmtSF: Good
# Heating: Some classes very rare
# HeatingQC: Some classes very rare
# CentralAir: Good
# Electrical: One true NA
# X1stFlrSF: Good, but referred to as "1stFlrSF" in docs 
# X2ndFlrSF: Good, but referred to as "2ndFlrSF" in docs 
# LowQualFinSf: Good
# GrLivArea: Good
# BsmtFullBath: Good
# BsmtHalfBath: Good
# FullBath: Good
# HalfBath: Good
# BedroomAbvGr: Good
# KitchenAbvGr: Good
# KitchenQual: Change to ordered factor or numeric
# TotRmsAbvGrd: Good
# Functional: Change to ordered factor or numeric
# Fireplaces: Good
# FireplaceQu: False NAs, change to ordered factor or numeric
# GarageType: False Nas
# GarageYrBlt: False NAs
# GarageFinish: False NAs
# GarageCars: Good
# GarageArea: Good
# GarageQual: Change to ordered factor or numeric; false NAs
# GarageCond: Change to ordered factor or numeric; false NAs
# PavedDrive: Change to ordered factor or numeric
# WoodDeckSF: Good
# OpenPorchSF: Good
# EnclosedPorch: Good
# X3SSnPorch: Good, but referred to as "3SsnPorch" in docs
# ScreenPorch: Good
# PoolArea: Good
# PoolQC: Change to ordered factor or numeric
# Fence: False NAs
# MiscFeature: False NAs, some classes very rare
# MiscVal: Good
# MoSold: Good
# YearSold: Good
# SaleType: Some classes very rare
# SaleCondition: Some classes very rare
# SalePrice: Target; will not be in test data

clean_data <- function(my_data){
  
  fix_false_NA <- function(col){
    col[is.na(col)] <- 'None'
    return (col)
  }
  
  order_qual_codes <- function(col){
    # Ex	Excellent
    # Gd	Good
    # TA	Average/Typical
    # Fa	Fair
    # Po	Poor
    
    col[col=='Po'] <- 0
    col[col=='Fa'] <- 1
    col[col=='TA'] <- 2
    col[col=='Gd'] <- 3
    col[col=='Ex'] <- 4
    col[col=='None'] <- -1 #FIXME: ugly hack, only relevant to FireplaceQu
    
    return (col)
  }
  
  order_Functional <- function(col){
    # Typ	Typical Functionality
    # Min1	Minor Deductions 1
    # Min2	Minor Deductions 2
    # Mod	Moderate Deductions
    # Maj1	Major Deductions 1
    # Maj2	Major Deductions 2
    # Sev	Severely Damaged
    # Sal	Salvage only
    
    col[col=='Typ'] <- 0
    col[col=='Min1'] <- 1
    col[col=='Min2'] <- 2
    col[col=='Mod'] <- 3
    col[col=='Maj1'] <- 4
    col[col=='Maj2'] <- 5
    col[col=='Sev'] <- 6
    col[col=='Sal'] <- 7
    
    return(col)
  }
  
  order_PavedDrive <- function(col){
    # Y	Paved 
    # P	Partial Pavement
    # N	Dirt/Gravel
    
    col[col=='N'] <- 0
    col[col=='P'] <- 1
    col[col=='Y'] <- 2
    
    return(col)
  }
  
  two_hot_combine <- function(df){
    encoder <- onehot(df, stringsAsFactors = TRUE)
    wide_double_hot <- data.frame(predict(encoder, df))
    
    dh_names <- Reduce(union, lapply(df, unique))
    double_hot <- data.frame(matrix(nrow = nrow(df), ncol=length(dh_names), data=0))
    names(double_hot) <- dh_names
    
    for (i in 1:ncol(wide_double_hot)){
      col_id <- strsplit(colnames(wide_double_hot)[i], split="\\.")[[1]][2]
      double_hot[col_id] = double_hot[col_id] + wide_double_hot[i]
    }
     
    names(double_hot) <- paste0("Condition.", dh_names)
    
    return(double_hot)
      
  }
  
  false_NA_col <- c('Alley', 
                    'BsmtQual', 
                    'BsmtCond', 
                    'BsmtExposure', # FIXME: overwrites true missing
                    'BsmtFinType1',
                    'BsmtFinType2', #FIXME: overwrites true missing
                    'FireplaceQu',
                    'GarageType',
                    # 'GarageYrBlt', # Breaks column type to replace NA values
                    'GarageFinish',
                    'GarageQual',
                    'GarageCond',
                    'Fence')
  
  qual_col <- c('ExterQual', 'ExterCond',
                'KitchenQual',
                'GarageQual', 'GarageCond',
                'FireplaceQu', 
                'PoolQC')
  
  # Fix false NAs
  my_data[false_NA_col] <- apply(my_data[false_NA_col], 2, fix_false_NA)
  
  # Order factor variables
  my_data[qual_col] <- apply(my_data[qual_col], 2, order_qual_codes)
  my_data['Functional'] <- order_Functional(my_data['Functional'])
  my_data['PavedDrive'] <- order_PavedDrive(my_data['PavedDrive'])
  
  # Combine two-hot columns
  my_data <- cbind(my_data, two_hot_combine(my_data[c('Condition1', 'Condition2')]))
  within(my_data, rm(Condition1, Condition2))

  return(my_data)
  
}

clean_train <- clean_data(train)
clean_test <- clean_data(test)

write.csv(clean_train, "./Data/clean_train.csv")
write.csv(clean_train, "./Data/clean_test.csv")