# Libraries ####
library(caret)

# Load data ####
training <- read.csv("./Data/engineered_train.csv")

# Model fitting ####
lm_model <- train(SalePrice ~ .,
                  training,
                  method = "lm")

# Model analytics ####
print(lm_model)
summary(lm_model)

# Saving model ####
saveRDS(lm_model, file="./Saved_Models/lm.rds")
