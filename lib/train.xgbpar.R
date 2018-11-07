#########################################################
### Train a classification model with training features ###
#########################################################

### Author: Chengliang Tang
### Project 3


train.xgbpar <- function(dat_train, label_train, par=NULL){
  
  ### Train a Gradient Boosting Model (GBM) using processed features from training images
  
  ### Input: 
  ###  -  features from LR images 
  ###  -  responses from HR images
  ### Output: a list for trained models
  
  ### load libraries
  library("xgboost")
  
  ### creat model list
  modelList <- list()
  
  ### Train with gradient boosting model
  if(is.null(par)){
    max_depth <- 2
    eta <- 1
  } else {
    max_depth <- par$max_depth
    eta <- par$eta
    #nrounds = par$nrounds
    subsample = par$subsample
    min_child_weight = par$min_child_weight
  }
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  library(foreach)
  library(doParallel)
  cl <- makePSOCKcluster(detectCores())
  
  registerDoParallel(cl)
  modelList <- foreach(i = 1:12, .packages = "xgboost") %dopar% {
    library("xgboost")
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_xgboost <- xgboost(data=featMat, label=labMat,
                           max_depth=max_depth,
                           eta=eta,
                           nthread=2,
                           nrounds=10,
                           objective="reg:linear", 
                           verbose=FALSE,
                           subsample=subsample,
                           min_child_weight = min_child_weight)
    modelList[[i]] <- list(fit=fit_xgboost)
  }
  stopCluster(cl)

  
  return(modelList)
}

