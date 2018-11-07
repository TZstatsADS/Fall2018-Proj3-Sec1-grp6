### Project 3


train <- function(dat_train, label_train, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  
  ### load libraries
  library("gbm")
  
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    depth <- 3
    n.trees <- 200
  } else {
    depth <- par$depth
    n.trees <- par$n.trees
  }
  library(foreach)
  library(doParallel)
  cl <- makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  modelList <- foreach(i = 1:12, .packages = "gbm") %dopar% {
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_gbm <- gbm.fit(x=featMat, y=labMat,
                           n.trees = n.trees,
                           distribution="gaussian",
                           interaction.depth=depth, 
                           bag.fraction = 0.5,
                           verbose=FALSE)
    best_iter <- gbm.perf(fit_gbm, method = "OOB", plot.it = FALSE)
    list(fit = fit_gbm, iter = best_iter)
  }
  stopCluster(cl)
  
  return(modelList)
}

