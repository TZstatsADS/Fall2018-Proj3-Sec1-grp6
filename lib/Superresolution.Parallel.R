########################
### Super-resolution ###
########################

### Author: Chengliang Tang
### Project 3

superResolution.par <- function(LR_dir, HR_dir, modelList){
  
  ### Construct high-resolution images from low-resolution images with trained predictor
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + a list for predictors
  
  ### load libraries
  library("EBImage")
  n_files <- length(list.files(LR_dir))
  library(doParallel)
  library(foreach)
  ### read LR/HR image pairs
  
  
  cl <- makePSOCKcluster(detectCores())
  registerDoParallel(cl)
  
  foreach (i = 1:n_files,.packages = "EBImage") %dopar% {
    library("EBImage")
    source("./lib/test.R")
    
    imgLR <- readImage(paste0(LR_dir,  "img", "_", sprintf("%04d", i), ".jpg"))
    pathHR <- paste0(HR_dir,  "img", "_", sprintf("%04d", i), ".jpg")
    featMat <- array(NA, c(dim(imgLR)[1] * dim(imgLR)[2], 8, 3))
    
    ### step 1. for each pixel and each channel in imgLR:
    ###           save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    ### step 1. sample n_points from imgLR
    
    dimLR <- dim(imgLR)
    pixels = seq(1,dimLR[1] * dimLR[2])
    pixels_row <- (pixels - 1)%%dimLR[1] + 1
    pixels_col <- (pixels - 1)%/%dimLR[1] + 1
    

    
    ##need to give values
     ### step 2. for each sampled point in imgLR
    
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    n_points <- dim(imgLR)[1] * dim(imgLR)[2] 
    
    for(j in 1:3){
      
      pad <- cbind(0, imgLR[,,j], 0)
      pad <- rbind(0, pad, 0)
      pad_central <- pad[cbind(pixels_row +1, pixels_col+1)]
      
      featMat[1:n_points, 1, j] <- pad[cbind(pixels_row, pixels_col)] -  pad_central
      featMat[1:n_points, 2, j] <- pad[cbind(pixels_row, pixels_col + 1)] - pad_central
      featMat[1:n_points, 3, j] <- pad[cbind(pixels_row, pixels_col+2)] - pad_central
      featMat[1:n_points, 4, j] <- pad[cbind(pixels_row +1, pixels_col+2)] - pad_central
      featMat[1:n_points, 5, j] <- pad[cbind(pixels_row +2, pixels_col+2)] - pad_central
      featMat[1:n_points, 6, j] <- pad[cbind(pixels_row+2, pixels_col+1)] - pad_central
      featMat[1:n_points, 7, j] <- pad[cbind(pixels_row+2, pixels_col)] - pad_central
      featMat[1:n_points, 8, j] <- pad[cbind(pixels_row+1, pixels_col)] - pad_central
    }
    
    
    ### step 2. apply the modelList over featMat
    predMat <- test(modelList, featMat)$arraypred
  
    ### step 3. recover high-resolution from predMat and save in HR_dir
   
    imagearray <- array(NA, c((dim(imgLR)[1]*2), (dim(imgLR)[2]*2), 3))
      
    for(k in 1:3){
      #doubleLR <- matrix(rep(pad[cbind(pixels_row +1, pixels_col+1)],4), ncol = 4)
      
      doubleLR <- matrix(rep(c(t(imgLR[, ,k])), each = 4), ncol = 4, byrow = TRUE)
      
      doubleLR[1:4,1:4]
      imagearray[seq(1,dim(imgLR)[1]*2, by =2 ),,k] <-  matrix(c(t(predMat[,1:2,k])), ncol = dim(imgLR)[2]*2, byrow = TRUE) + 
                                                        matrix(c(t(doubleLR[,1:2])), ncol = dim(imgLR)[2]*2, byrow = TRUE)
        
      imagearray[seq(2,dim(imgLR)[1]*2, by =2 ),,k] <-  matrix(c(t(predMat[,3:4,k])), ncol = dim(imgLR)[2]*2,byrow = TRUE) + 
                                                        matrix(c(t(doubleLR[,3:4])), ncol = dim(imgLR)[2]*2, byrow = TRUE)
      imagearray[1:4,1:4,1]
    }
    
    imagearray <- Image(imagearray)
    colorMode(imagearray) <- Color
    writeImage(imagearray, file = paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
  }
  stopCluster(cl)
}paste


