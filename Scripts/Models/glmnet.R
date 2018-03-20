# Libraries ####
library(caret)

# Load data ####
training <- read.csv("./Data/engineered_train.csv")

# Model fitting ####
# FIXME: does not run
glmnet_model <- train(SalePrice ~ ., 
                   training,
                   method = "glmnet",
                   family=gaussian(link="log")
)

# Model analytics ####
print(glmnet_model)
summary(glmnet_model)

# Saving model ####
saveRDS(glmnet_model, file="./Saved_Models/glmnet.rds")
