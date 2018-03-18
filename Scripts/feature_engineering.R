# Loading data ####
clean_train <- read.csv("./Data/clean_train.csv")
clean_test <- read.csv("./Data/clean_train.csv")

# Feature engineering ####

# Transformations that have no degrees of freedom
independent_engineer <- function(data){
  return (engineered_data)
}

engineered_train <- independent_engineer(clean_train)
engineered_test <- independent_engineer(clean_test)

# Transformations that are learned from the data
dependent_engineer <- function(train, test){
  return (list(engineered_train, engineered_test))
}

dependent_data <- dependent_engineer(engineered_train, engineered_test)
engineered_train <- dependent_data[[1]]
engineered_test  <- dependent_data[[2]]

# Saving data ####
write.csv(enigineered_train, "./Data/engineered_train.csv")
write.csv(engineered_test,  "./Data/engineered_test.csv")