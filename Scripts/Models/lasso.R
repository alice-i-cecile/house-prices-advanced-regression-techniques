# Libraries ####
library(caret)

# Load data ####
training <- read.csv("./Data/engineered_train.csv")

# Model fitting ####
lasso_model <- train(SalePrice ~ ., 
                  training,
                  method = "lasso"
)

# Model analytics ####
print(lasso_model)
summary(lasso_model)

# Saving model ####
saveRDS(lasso_model, file="./Saved_Models/lasso.rds")
