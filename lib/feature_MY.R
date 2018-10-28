#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3

##################################
#####Get index of matric #########
##################################

getmatindex <- function(vecindex, m){
  r <- ifelse(vecindex %% nrow(m) != 0, vecindex %% nrow(m), nrow(m))
  c <- ifelse(vecindex %% nrow(m) != 0, floor(vecindex/nrow(m)) +1, floor(vecindex/nrow(m)))
  return(c(r,c))
}


##################################
##### Ectract features for GBM ###
##################################


feature <- function(LR_dir, HR_dir, n_points=1000){
  
  ### Construct process features for training images (LR/HR pairs)
  
  ### Input: a path for low-resolution images + a path for high-resolution images 
  ###        + number of points sampled from each LR image
  ### Output: an .RData file contains processed features and responses for the images
  
  ### load libraries
  library("EBImage")
  
  
  n_files <- length(list.files(LR_dir))
  
  ### store feature and responses
  featMat <- array(NA, c(n_files * n_points, 8, 3))
  labMat <- array(NA, c(n_files * n_points, 4, 3))
  
  ### read LR/HR image pairs
  for(i in 1:n_files){
    imgLR <- readImage(paste0(LR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    imgHR <- readImage(paste0(HR_dir,  "img_", sprintf("%04d", i), ".jpg"))
    
    ### step 1. sample n_points from imgLR
    redmat <- imgLR[,,1]
    zero <- rep(0, nrow(redmat))
    zero1 <- rep(0, ncol(redmat)+2)
    mred <- cbind(zero, redmat, zero)
    mred<- rbind(zero1, mred, zero1)
    redvector <- c(mred)
    
    bluemat <- imgLR[,,2]
    zero <- rep(0, nrow(bluemat))
    zero1 <- rep(0, ncol(bluemat)+2)
    mblue <- cbind(zero, bluemat, zero)
    mblue<- rbind(zero1, mblue, zero1)
    bluevector <- c(mblue)
    
    greenmat <- imgLR[,,3]
    zero <- rep(0, nrow(greenmat))
    zero1 <- rep(0, ncol(greenmat)+2)
    mgreen <- cbind(zero, greenmat, zero)
    mgreen <- rbind(zero1, mgreen, zero1)
    greenvector <- c(mgreen)
    
    
    colno <- ncol(mred)
    rowno <- nrow(mred)
    a <- c(1:(rowno-1), 
           seq(from = rowno, to = (colno-1)*rowno, by = rowno), 
           seq(from = rowno+1, to = (colno-1)*rowno+1, by = rowno), 
           ((colno-1)*rowno+2):(colno*rowno))
    
    redvectorHR <- c(imgHR[,,1])
    greenvectorHR <- c(imgHR[,,2])
    bluevectorHR <- c(imgHR[,,3])
    
    allindex <- 1:length(redvector)
    index_no_bond <- allindex[!(1:length(redvector) %in% a)]
    
    index <- sample(index_no_bond, n_points)
    
    ### step 2. for each sampled point in imgLR
    
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    for(p in ((i-1)*n_points+1):(n_points*i)){
      
      if(p %% n_points == 0 ){modi <- n_points}else{modi <- p %% n_points }
      
      indexaround <- c(index[modi] -1, index[modi] +1, 
                       index[modi] - nrow(mred), index[modi] - nrow(mred) +1, index[modi] - nrow(mred) -1,
                       index[modi] + nrow(mred), index[modi] + nrow(mred) +1, index[modi] + nrow(mred) -1)
      
      featMat[,,1][p,] <- redvector[indexaround]
      featMat[,,2][p,] <- greenvector[indexaround]
      featMat[,,3][p,] <- bluevector[indexaround]
      
      orgindex <- index[modi] - nrow(mred) - 2 * floor(index[modi]/nrow(mred))-1
      indexfour <- c(getmatindex(orgindex, mred)[1] * 2 -1 
                     + (getmatindex(orgindex, mred)[2]*2-1) *2 * nrow(mred))
      
      labMat[,,1][p,] <- redvectorHR[indexfour]
      labMat[,,2][p,] <- greenvectorHR[indexfour]
      labMat[,,3][p,] <- bluevectorHR[indexfour]
      
    }
    
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
    
    ### step 3. repeat above for three channels
  }   
  
  return(list(feature = featMat, label =labMat ))
}


feature(train_LR_dir, train_HR_dir)
