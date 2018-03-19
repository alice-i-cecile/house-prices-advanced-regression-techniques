# Libraries ####
library(caret)

# Load data ####
training <- read.csv("./Data/engineered_train.csv")

# Model fitting ####
ridge_model <- train(SalePrice ~ ., 
                     training,
                     method = "ridge"
)

# Model analytics ####
print(ridge_model)
summary(ridge_model)

# Saving model ####
save(ridge_model, file="./Saved_Models/ridge.RData")
