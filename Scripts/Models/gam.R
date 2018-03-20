# Libraries ####
library(mgcv)

# Load data ####
training <- read.csv("./Data/engineered_train.csv")

# Model fitting ####
gam_model <- gam(SalePrice ~ MSSubClass +
                   MSZoning +
                   s(LotFrontage) +
                   s(LotArea) + 
                   Street +
                   Alley +
                   LotShape +
                   LandContour +
                   LotConfig +
                   LandSlope +
                   Neighborhood +
                   BldgType +
                   HouseStyle +
                   s(OverallQual, k=5) +
                   s(OverallCond, k=5) +
                   s(YearBuilt) +
                   s(YearRemodAdd) +
                   RoofStyle +
                   RoofMatl +
                   Foundation +
                   s(BsmtQual, k=5) +
                   s(BsmtCond, k=5) +
                   s(BsmtExposure, k=3) +
                   BsmtFinType1 +
                   BsmtFinType1 * BsmtFinSF1 +
                   BsmtFinType2 +
                   BsmtFinType2 * BsmtFinSF2 +
                   s(BsmtUnfSF) + 
                   Heating +
                   HeatingQC +
                   CentralAir +
                   Electrical +
                   s(X1stFlrSF) +
                   s(X2ndFlrSF) +
                   s(LowQualFinSF) +
                   BsmtFullBath +
                   BsmtHalfBath +
                   FullBath +
                   HalfBath +
                   BedroomAbvGr +
                   KitchenAbvGr +
                   s(KitchenQual, k=3) +
                   s(TotRmsAbvGrd, k=3) +
                   s(Functional, k=3) +
                   Fireplaces +
                   Fireplaces * FireplaceQu +
                   GarageType +
                   s(GarageYrBlt) +
                   s(GarageCars, k=3) +
                   s(GarageArea) +
                   s(GarageQual, k=3) +
                   s(GarageCond, k=3) +
                   PavedDrive +
                   s(WoodDeckSF) +
                   s(OpenPorchSF) +
                   s(EnclosedPorch) +
                   s(X3SsnPorch) +
                   s(ScreenPorch) +
                   s(PoolArea, k=5) +
                   s(PoolQC, k=3) +
                   Fence +
                   Fence * LotSide +
                   MiscVal +
                   s(MoSold, bs="cc", k=5) +
                   s(YrSold, k=5) +
                   SaleType +
                   SaleCondition +
                   Condition.Norm +
                   Condition.Feedr +
                   Condition.PosN +
                   Condition.Artery +
                   Condition.RRAe +
                   Condition.RRNn +
                   Condition.RRAn +
                   Condition.PosA +
                   Condition.RRNe +
                   s(LotSide) +
                   s(BsmtUnfPercent) +
                   LowQualFinPresence
                 , 
                 family=gaussian(link='log'),
                 data=training)

# Model analytics ####
summary(gam_model)
plot(gam_model)


# Saving model ####
saveRDS(gam_model, file="./Saved_Models/gam.rds")
