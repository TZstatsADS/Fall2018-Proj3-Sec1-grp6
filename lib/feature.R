#############################################################
### Construct features and responses for training images###
#############################################################

### Authors: Chengliang Tang/Tian Zheng
### Project 3


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
    imgLR <- as.array(imgLR)
    imgHR <- as.array(imgHR)
   
    ### step 1. sample n_points from imgLR
    
    dimLR <- dim(imgLR)
    select = sample(dimLR[1] * dimLR[2], n_points)
    select_row <- (select - 1)%%dimLR[1] + 1
    select_col <- (select - 1)%/%dimLR[1] + 1
    
    ### step 2. for each sampled point in imgLR
    
    ### step 2.1. save (the neighbor 8 pixels - central pixel) in featMat
    ###           tips: padding zeros for boundary points
    
    for(j in 1:3){
      pad <- cbind(0, imgLR[,,j], 0)
      pad <- rbind(0, pad, 0)
      pad_central <- pad[cbind(select_row +1, select_col+1)]
      featMat[(i-1)*n_points + 1:n_points, 1, j] <- pad[cbind(select_row, select_col)]-pad_central
      featMat[(i-1)*n_points + 1:n_points, 2, j] <- pad[cbind(select_row, select_col + 1)]-pad_central
      featMat[(i-1)*n_points + 1:n_points, 3, j] <- pad[cbind(select_row, select_col+2)]-pad_central
      featMat[(i-1)*n_points + 1:n_points, 4, j] <- pad[cbind(select_row +1, select_col+2)]-pad_central
      featMat[(i-1)*n_points + 1:n_points, 5, j] <- pad[cbind(select_row +2, select_col+2)]-pad_central
      featMat[(i-1)*n_points + 1:n_points, 6, j] <- pad[cbind(select_row+2, select_col+1)]-pad_central
      featMat[(i-1)*n_points + 1:n_points, 7, j] <- pad[cbind(select_row+2, select_col)]-pad_central
      featMat[(i-1)*n_points + 1:n_points, 8, j] <- pad[cbind(select_row+1, select_col)]-pad_central
      
      channelHR <- imgHR[,,j]
      
      labMat[(i-1)*n_points + 1:n_points, 1, j] <- channelHR[cbind(select_row*2-1, select_col*2 -1)]-pad_central
      labMat[(i-1)*n_points + 1:n_points, 2, j] <- channelHR[cbind(select_row*2-1, select_col*2)]-pad_central
      labMat[(i-1)*n_points + 1:n_points, 3, j] <- channelHR[cbind(select_row*2, select_col*2)]-pad_central
      labMat[(i-1)*n_points + 1:n_points, 4, j] <- channelHR[cbind(select_row*2, select_col*2 -1)]-pad_central
      
      
    }
    
      ### step 2.2. save the corresponding 4 sub-pixels of imgHR in labMat
    
    ### step 3. repeat above for three channels
  }   
  
  return(list(feature = featMat, label =labMat ))
}



