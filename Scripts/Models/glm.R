# Libraries ####
library(caret)

# Load data ####
training <- read.csv("./Data/engineered_train.csv")

# Model fitting ####
glm_model <- train(SalePrice ~ ., 
                  training,
                  method = "glm",
                  family=gaussian(link="log")
                  )

# Model analytics ####
print(glm_model)
summary(glm_model)

# Saving model ####
save(glm_model, file="./Saved_Models/glm.RData")