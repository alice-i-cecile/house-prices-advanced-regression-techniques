# Libraries ####
library(caret)

# Load data ####
training <- read.csv("./Data/engineered_train.csv")

# Model fitting ####
rf_model <- train(SalePrice ~ ., 
                   training,
                   method = "rf"
)

# Model analytics ####
print(rf_model)
summary(rf_model)

# Saving model ####
save(rf_model, file="./Saved_Models/rf.RData")
