train_dir <- "/Users/jinpeiqi/Downloads/train_set/"
train_LR_dir <- paste(train_dir, "LR/", sep="")
train_HR_dir <- paste(train_dir, "HR/", sep="")
train_label_path <- paste(train_dir, "label.csv", sep="")

features <- feature(train_LR_dir, train_HR_dir)



train_xgboost <- function(dat_train, label_train, par=NULL){
  
  ### Train a Xgboost using processed features from training images
  
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
    max_depth <- 6
    gamma <- 0
  } else {
    max_depth <- par[1]
    gamma <- par[2]
  }
  
  ### the dimension of response arrat is * x 4 x 3, which requires 12 classifiers
  ### this part can be parallelized
  for (i in 1:12){
    ## calculate column and channel
    c1 <- (i-1) %% 4 + 1
    c2 <- (i-c1) %/% 4 + 1
    featMat <- dat_train[, , c2]
    labMat <- label_train[, c1, c2]
    fit_xgboost <- xgboost(data=featMat, label=labMat,
                           max_depth=max_depth,
                           gamma=gamma,
                           nrounds=10, 
                           subsample=0.5,
                           objective="reg:linear", 
                           verbose=FALSE)
    modelList[[i]] <- list(fit=fit_xgboost)
  }
  
  return(modelList)
}


test <- function(modelList, dat_test){
    
    ### Fit the classfication model with testing data
    
    ### Input: 
    ###  - the fitted classification model list using training data
    ###  - processed features from testing images 
    ### Output: training model specification
    
    ### load libraries
    library("xgboost")
    
    predArr <- array(NA, c(dim(dat_test)[1], 4, 3))
    
    for (i in 1:12){
      fit_train <- modelList[[i]]
      ### calculate column and channel
      c1 <- (i-1) %% 4 + 1
      c2 <- (i-c1) %/% 4 + 1
      featMat <- dat_test[, , c2]
      ### make predictions
      predArr[, c1, c2] <- predict(fit_train$fit, newdata=featMat, 
                                    type="response")
    }
    return(list(numericpred = as.numeric(predArr), arraypred = predArr))
  }
  
  
  


xgboost_models <- train_xgboost(features$feature, features$label)

#parameters of  of xgboost: max_depth and eta
c1 <- seq(4, 10, 2)
c2 <- seq(8, 14, 2)

model_par <- data.frame(c1, c2)
colnames(model_par) <- c("max_depth", "gamma")

feat_train <- features$feature
label_train <- features$label


#cross validation
cv.function <- function(X.train, y.train, modelvalues, K){
  
  n <- dim(y.train)[1]
  n.fold <- floor(n/K)
  s <- sample(rep(1:K, c(rep(n.fold, K-1), n-(K-1)*n.fold)))  
  cv.error <- rep(NA, K)
  
  for (i in 1:K){
    train.data <- X.train[s != i, ,]
    train.label <- y.train[s != i, ,]
    test.data <- X.train[s == i, ,]
    test.label <- y.train[s == i, ,]
    
    #par <- list(depth=d)
    fit <- train_xgboost(train.data, train.label, modelvalues)
    pred <- test(fit, test.data)  
    cv.error[i] <- mean((pred - test.label)^2)  
    
  }			
  return(c(mean(cv.error),sd(cv.error)))
}


err_cv <- array(dim=c(length(model_par), 2))

for(k in 1:nrow(model_par)){
  cat("k=", k, "\n")
  err_cv[k,] <- cv.function(feat_train, label_train, model_par[k, ], K=2)
}


#plot graphs
fit_train <- train(feat_train, label_train, model_par[2, ])
test_LR_dir <- "/Users/jinpeiqi/Downloads/Fall2018-Proj3-Sec1-grp6-master-2/data/test_sample/LR/"
test_HR_dir <- "/Users/jinpeiqi/Downloads/Fall2018-Proj3-Sec1-grp6-master-2/data/test_sample/HR/"
superResolution(LR_dir=test_LR_dir, HR_dir=test_HR_dir, modelList=fit_train)


