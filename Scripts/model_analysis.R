# Libraries ####
library(ggplot2)
library(reshape2)
library(caret)

# Loading data ####
engineered_train <- read.csv("./Data/engineered_train.csv")
engineered_test <- read.csv("./Data/engineered_test.csv")

# Extracting metrics and predictions ####
model_names <- c("gam", "glm", "lasso", "lm", "rf", "ridge")

model_performance <- data.frame(matrix(nrow=length(model_names), ncol=7))
names(model_performance) <- c("Model", "RMSE", "RSquared", "MAE", "RMSESD", "RsquaredSD", "MAESD")
model_performance$Model <- model_names

train_pred <- data.frame(matrix(nrow=nrow(engineered_train), ncol=length(model_names)))
names(train_pred) <- model_names

test_pred <- data.frame(matrix(nrow=nrow(engineered_test), ncol=length(model_names)))
names(test_pred) <- model_names

for (i in 1:length(model_names)) {
  m_name <- model_names[i]
  m <- readRDS(paste0("./Saved_Models/", m_name, ".rds"))
  
  if (m_name == "gam") {
    model_performance[i, "RSquared"] <- summary(m)$r.sq
  } else {
    best_row <- which.min(m$results$RMSE)
    model_performance[i, 2:7] <- m$results[best_row, 2:7]
  }
  
  train_pred[[m_name]] <- predict(m, newdata = engineered_train)
  test_pred[[m_name]]  <- predict(m, newdata = engineered_test)
  
  if (m_name == "gam"){
    train_pred[[m_name]] <- exp(train_pred[[m_name]])
    test_pred[[m_name]] <- exp(test_pred[[m_name]])
  }
}

# Set NA and negative predictions to dataset mean
train_pred[is.na(train_pred)] <- mean(engineered_train$SalePrice)
test_pred[is.na(test_pred)]   <- mean(engineered_train$SalePrice)

train_pred[train_pred < 0] <- mean(engineered_train$SalePrice)
test_pred[test_pred < 0]   <- mean(engineered_train$SalePrice)


# Ensembling ####
# TODO: use exponential averaging
ensemble_names <- c("ensemble_all", "ensemble_ridge_rf", "stack_lasso")
ensemble_start <- length(model_names)+1
ensemble_end <- length(model_names)+length(ensemble_names)

model_performance[ensemble_start:ensemble_end,] <- NA
model_performance[ensemble_start:ensemble_end, "Model"] <- ensemble_names

train_pred$ensemble_all <- rowMeans(train_pred)
test_pred$ensemble_all <- rowMeans(test_pred)

train_pred$ensemble_ridge_rf <- rowMeans(train_pred[c('ridge','rf')])
test_pred$ensemble_ridge_rf <- rowMeans(test_pred[c('ridge','rf')])

# Stacking ##
stack_x_train <- data.frame(sapply(train_pred[model_names], log))
stack_x_test  <- data.frame(sapply(test_pred[model_names], log))
stack_y <- log(engineered_train$SalePrice)
stack_data <- cbind(stack_x_train, stack_y)

# Lasso stack
stack_lasso <- train(stack_y~.+0, data=stack_data, method='lasso')
train_pred$stack_lasso <- exp(predict(stack_lasso, newdata = stack_x_train))
test_pred$stack_lasso  <- exp(predict(stack_lasso, newdata = stack_x_test))

# Visualizations ####

# Prediction Correlations ##

# Wrapper for plots modified from:
# http://www.sthda.com/english/wiki/ggplot2-quick-correlation-matrix-heatmap-r-software-and-data-visualization

make_cor_plot <- function(dat, method="pearson"){
  
  if(is.data.frame(dat)){
    # Create the correlation matrix
    cormat <- cor(dat, method = method)
    
    # Reorder using hierarchical clustering
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <- cormat[hc$order, hc$order]
    
    # Remove the upper triangle of the matrix as it's redundant
    cormat[lower.tri(cormat)] <- 0
    
  } else {
    # Create the correlation matrix
    cormat <- cor(x=dat[[1]], y= dat[[2]], method = method)
    
    # Reorder
    # Reorder using hierarchical clustering
    # Sort each axis according to internal clustering
    # Use correlation between variables as distance
    cormat1 <- cor(dat[[1]], method = method)
    dd1 <- as.dist((1-cormat1)/2)
    hc1 <- hclust(dd1)
    
    cormat2 <- cor(dat[[2]], method = method)
    dd2 <- as.dist((1-cormat2)/2)
    hc2 <- hclust(dd2)
    
    cormat <- cormat[hc1$order, hc2$order]
  }
  
  # Create the plot
  melted_cormat <- melt(cormat)
  
  cor_plot <- ggplot(data = melted_cormat, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(low = "dodgerblue4", high = "red4", mid = "white", 
                         midpoint = 0.9, limit = c(0.8,1), space = "Lab", 
                         name="Correlation") +
    theme_minimal() + 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1),
          axis.title = element_blank()) +
    coord_fixed()
  
  return(cor_plot)
}

make_cor_plot(log(train_pred))
make_cor_plot(log(test_pred))

# Prediction error by sale price ####
error_df <- train_pred
error_df <- sapply(error_df, log)
error_df <- data.frame(error_df - log(engineered_train$SalePrice))
error_df$SalePrice <- engineered_train$SalePrice
melted_error <- melt(error_df, id.vars = "SalePrice")

ggplot(melted_error, aes(x=SalePrice, y=value, colour=variable)) + 
    geom_point(alpha=0.2) + facet_wrap(facets = "variable") + 
    geom_hline(yintercept = 0) + theme_bw()

# Evaluation ####
# Using RMSE of log SalePrice as per competition
check_score <- function(predictions) {
  pred <- log(predictions)
  actual <- log(engineered_train$SalePrice)
  
  error <- actual - pred
  square_error <- error*error
  mean_error <- mean(square_error, na.rm=TRUE)
  
  RMSE <- sqrt(mean_error)
  
  return(RMSE)
}

model_performance$score <- NA
for (i in 1:ncol(train_pred)) {
  model_performance[i, "score"] <- check_score(train_pred[[i]])
}

print(model_performance)

# Submission preparation ####
prepare_submission <- function(predictions, sub_name){
  sub_df <- data.frame(Id=engineered_test$Id, SalePrice=predictions)
  names(sub_df) <- c("Id", "SalePrice")
  
  write.csv(sub_df, file=paste0("./Submissions/", sub_name, ".csv"),
            quote=FALSE, row.names=FALSE)
}

for (i in 1:ncol(test_pred)){
  prepare_submission(test_pred[i], names(test_pred)[i])
}
