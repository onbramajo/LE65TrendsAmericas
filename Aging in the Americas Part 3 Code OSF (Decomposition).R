
# part 3 of code: decomposition

options(scipen = 999, digits = 3)

# matrix to vec function for the horiuchi decomposition

mat_to_vec <- function(mat){
  dims <- dim(mat)
  vec <- c(mat)
  attr(vec,"dims to reapply") <- dims
  vec
}
# get back to matrix
vec_to_mat <- function(vec, dims = NULL){
  if (is.null(dims)){
    dims <- attr(vec, "dims to reapply")
  }
  dim(vec) <- dims
  vec
}

#  adapt the Mxc2e0abr function to decompose e50
LTFLEDecomp <- function (nmx, dims = NULL)
{
  # TR: new line
  nmx <- vec_to_mat(nmx, dims = dims)
  Mx  <- rowSums(nmx)
  LTFLE(Mx)
}


LTFLE <- function(nmx,Age=c(seq(65,95,5))){
  nax <- 2.5
  n <- c(diff(Age),nax[length(nax)])
  nqx <- n*nmx / (1+(n-nax)*nmx)
  nqx[7] <- 1
  npx <- 1 - nqx
  lx <- rep(NA,length(nqx))
  lx[1] <- 100000
  for(i in 1:length(n)){
    lx[i+1] <- lx[i]*npx[i]
  }
  
  lx <- lx[1:length(n)]
  
  ndx <- lx*nqx
  ndx[7] <- lx[7]
  nLx <- ndx/nmx
  Tx = rev(cumsum(rev(nLx)))
  ex <- Tx/lx
  return(ex[1])
}



# Combine the vectors into a matrix with 2 columns

combined_matrixARGMale2000 <- matrix(c(ARGMale2000$CVDShare*ARGMale2000$Deaths/ARGMale2000$Pop,
                                       ARGMale2000$CancerShare*ARGMale2000$Deaths/ARGMale2000$Pop,
                                       ARGMale2000$MetabolicShare*ARGMale2000$Deaths/ARGMale2000$Pop,
                                       ARGMale2000$RespShare*ARGMale2000$Deaths/ARGMale2000$Pop,
                                       ARGMale2000$ExternalShare*ARGMale2000$Deaths/ARGMale2000$Pop,
                                       ARGMale2000$NervShare*ARGMale2000$Deaths/ARGMale2000$Pop,
                                       ARGMale2000$RestShare*ARGMale2000$Deaths/ARGMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixARGFemale2000 <- matrix(c(ARGFemale2000$CVDShare*ARGFemale2000$Deaths/ARGFemale2000$Pop,
                                         ARGFemale2000$CancerShare*ARGFemale2000$Deaths/ARGFemale2000$Pop,
                                         ARGFemale2000$MetabolicShare*ARGFemale2000$Deaths/ARGFemale2000$Pop,
                                         ARGFemale2000$RespShare*ARGFemale2000$Deaths/ARGFemale2000$Pop,
                                         ARGFemale2000$ExternalShare*ARGFemale2000$Deaths/ARGFemale2000$Pop,
                                         ARGFemale2000$NervShare*ARGFemale2000$Deaths/ARGFemale2000$Pop,
                                         ARGFemale2000$RestShare*ARGFemale2000$Deaths/ARGFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixARGMale2010 <- matrix(c(ARGMale2010$CVDShare*ARGMale2010$Deaths/ARGMale2010$Pop,
                                       ARGMale2010$CancerShare*ARGMale2010$Deaths/ARGMale2010$Pop,
                                       ARGMale2010$MetabolicShare*ARGMale2010$Deaths/ARGMale2010$Pop,
                                       ARGMale2010$RespShare*ARGMale2010$Deaths/ARGMale2010$Pop,
                                       ARGMale2010$ExternalShare*ARGMale2010$Deaths/ARGMale2010$Pop,
                                       ARGMale2010$NervShare*ARGMale2010$Deaths/ARGMale2010$Pop,
                                       ARGMale2010$RestShare*ARGMale2010$Deaths/ARGMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixARGFemale2010 <- matrix(c(ARGFemale2010$CVDShare*ARGFemale2010$Deaths/ARGFemale2010$Pop,
                                         ARGFemale2010$CancerShare*ARGFemale2010$Deaths/ARGFemale2010$Pop,
                                         ARGFemale2010$MetabolicShare*ARGFemale2010$Deaths/ARGFemale2010$Pop,
                                         ARGFemale2010$RespShare*ARGFemale2010$Deaths/ARGFemale2010$Pop,
                                         ARGFemale2010$ExternalShare*ARGFemale2010$Deaths/ARGFemale2010$Pop,
                                         ARGFemale2000$NervShare*ARGFemale2000$Deaths/ARGFemale2000$Pop,
                                         ARGFemale2010$RestShare*ARGFemale2010$Deaths/ARGFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixARGMale2019 <- matrix(c(ARGMale2019$CVDShare*ARGMale2019$Deaths/ARGMale2019$Pop,
                                       ARGMale2019$CancerShare*ARGMale2019$Deaths/ARGMale2019$Pop,
                                       ARGMale2019$MetabolicShare*ARGMale2019$Deaths/ARGMale2019$Pop,
                                       ARGMale2019$RespShare*ARGMale2019$Deaths/ARGMale2019$Pop,
                                       ARGMale2019$ExternalShare*ARGMale2019$Deaths/ARGMale2019$Pop,
                                       ARGMale2019$NervShare*ARGMale2019$Deaths/ARGMale2019$Pop,
                                       ARGMale2019$RestShare*ARGMale2019$Deaths/ARGMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixARGFemale2019 <- matrix(c(ARGFemale2019$CVDShare*ARGFemale2019$Deaths/ARGFemale2019$Pop,
                                         ARGFemale2019$CancerShare*ARGFemale2019$Deaths/ARGFemale2019$Pop,
                                         ARGFemale2019$MetabolicShare*ARGFemale2019$Deaths/ARGFemale2019$Pop,
                                         ARGFemale2019$RespShare*ARGFemale2019$Deaths/ARGFemale2019$Pop,
                                         ARGFemale2019$ExternalShare*ARGFemale2019$Deaths/ARGFemale2019$Pop,
                                         ARGFemale2019$NervShare*ARGFemale2019$Deaths/ARGFemale2019$Pop,
                                         ARGFemale2019$RestShare*ARGFemale2019$Deaths/ARGFemale2019$Pop), nrow = 7, byrow = FALSE)



combined_matrixBRAMale2000 <- matrix(c(BRAMale2000$CVDShare*BRAMale2000$Deaths/BRAMale2000$Pop,
                                       BRAMale2000$CancerShare*BRAMale2000$Deaths/BRAMale2000$Pop,
                                       BRAMale2000$MetabolicShare*BRAMale2000$Deaths/BRAMale2000$Pop,
                                       BRAMale2000$RespShare*BRAMale2000$Deaths/BRAMale2000$Pop,
                                       BRAMale2000$ExternalShare*BRAMale2000$Deaths/BRAMale2000$Pop,
                                       BRAMale2000$NervShare*BRAMale2000$Deaths/BRAMale2000$Pop,
                                       BRAMale2000$RestShare*BRAMale2000$Deaths/BRAMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixBRAFemale2000 <- matrix(c(BRAFemale2000$CVDShare*BRAFemale2000$Deaths/BRAFemale2000$Pop,
                                         BRAFemale2000$CancerShare*BRAFemale2000$Deaths/BRAFemale2000$Pop,
                                         BRAFemale2000$MetabolicShare*BRAFemale2000$Deaths/BRAFemale2000$Pop,
                                         BRAFemale2000$RespShare*BRAFemale2000$Deaths/BRAFemale2000$Pop,
                                         BRAFemale2000$ExternalShare*BRAFemale2000$Deaths/BRAFemale2000$Pop,
                                         BRAFemale2000$NervShare*BRAFemale2000$Deaths/BRAFemale2000$Pop,
                                         BRAFemale2000$RestShare*BRAFemale2000$Deaths/BRAFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixBRAMale2010 <- matrix(c(BRAMale2010$CVDShare*BRAMale2010$Deaths/BRAMale2010$Pop,
                                       BRAMale2010$CancerShare*BRAMale2010$Deaths/BRAMale2010$Pop,
                                       BRAMale2010$MetabolicShare*BRAMale2010$Deaths/BRAMale2010$Pop,
                                       BRAMale2010$RespShare*BRAMale2010$Deaths/BRAMale2010$Pop,
                                       BRAMale2010$ExternalShare*BRAMale2010$Deaths/BRAMale2010$Pop,
                                       BRAMale2010$NervShare*BRAMale2010$Deaths/BRAMale2010$Pop,
                                       BRAMale2010$RestShare*BRAMale2010$Deaths/BRAMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixBRAFemale2010 <- matrix(c(BRAFemale2010$CVDShare*BRAFemale2010$Deaths/BRAFemale2010$Pop,
                                         BRAFemale2010$CancerShare*BRAFemale2010$Deaths/BRAFemale2010$Pop,
                                         BRAFemale2010$MetabolicShare*BRAFemale2010$Deaths/BRAFemale2010$Pop,
                                         BRAFemale2010$RespShare*BRAFemale2010$Deaths/BRAFemale2010$Pop,
                                         BRAFemale2010$ExternalShare*BRAFemale2010$Deaths/BRAFemale2010$Pop,
                                         BRAFemale2010$NervShare*BRAFemale2010$Deaths/BRAFemale2010$Pop,
                                         BRAFemale2010$RestShare*BRAFemale2010$Deaths/BRAFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixBRAMale2019 <- matrix(c(BRAMale2019$CVDShare*BRAMale2019$Deaths/BRAMale2019$Pop,
                                       BRAMale2019$CancerShare*BRAMale2019$Deaths/BRAMale2019$Pop,
                                       BRAMale2019$MetabolicShare*BRAMale2019$Deaths/BRAMale2019$Pop,
                                       BRAMale2019$RespShare*BRAMale2019$Deaths/BRAMale2019$Pop,
                                       BRAMale2019$ExternalShare*BRAMale2019$Deaths/BRAMale2019$Pop,
                                       BRAMale2019$NervShare*BRAMale2019$Deaths/BRAMale2019$Pop,
                                       BRAMale2019$RestShare*BRAMale2019$Deaths/BRAMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixBRAFemale2019 <- matrix(c(BRAFemale2019$CVDShare*BRAFemale2019$Deaths/BRAFemale2019$Pop,
                                         BRAFemale2019$CancerShare*BRAFemale2019$Deaths/BRAFemale2019$Pop,
                                         BRAFemale2019$MetabolicShare*BRAFemale2019$Deaths/BRAFemale2019$Pop,
                                         BRAFemale2019$RespShare*BRAFemale2019$Deaths/BRAFemale2019$Pop,
                                         BRAFemale2019$ExternalShare*BRAFemale2019$Deaths/BRAFemale2019$Pop,
                                         BRAFemale2019$NervShare*BRAFemale2019$Deaths/BRAFemale2019$Pop,
                                         BRAFemale2019$RestShare*BRAFemale2019$Deaths/BRAFemale2019$Pop), nrow = 7, byrow = FALSE)



combined_matrixCANMale2000 <- matrix(c(CANMale2000$CVDShare*CANMale2000$Deaths/CANMale2000$Pop,
                                       CANMale2000$CancerShare*CANMale2000$Deaths/CANMale2000$Pop,
                                       CANMale2000$MetabolicShare*CANMale2000$Deaths/CANMale2000$Pop,
                                       CANMale2000$RespShare*CANMale2000$Deaths/CANMale2000$Pop,
                                       CANMale2000$ExternalShare*CANMale2000$Deaths/CANMale2000$Pop,
                                       CANMale2000$NervShare*CANMale2000$Deaths/CANMale2000$Pop,
                                       CANMale2000$RestShare*CANMale2000$Deaths/CANMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixCANFemale2000 <- matrix(c(CANFemale2000$CVDShare*CANFemale2000$Deaths/CANFemale2000$Pop,
                                         CANFemale2000$CancerShare*CANFemale2000$Deaths/CANFemale2000$Pop,
                                         CANFemale2000$MetabolicShare*CANFemale2000$Deaths/CANFemale2000$Pop,
                                         CANFemale2000$RespShare*CANFemale2000$Deaths/CANFemale2000$Pop,
                                         CANFemale2000$ExternalShare*CANFemale2000$Deaths/CANFemale2000$Pop,
                                         CANFemale2000$NervShare*CANFemale2000$Deaths/CANFemale2000$Pop,
                                         CANFemale2000$RestShare*CANFemale2000$Deaths/CANFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixCANMale2010 <- matrix(c(CANMale2010$CVDShare*CANMale2010$Deaths/CANMale2010$Pop,
                                       CANMale2010$CancerShare*CANMale2010$Deaths/CANMale2010$Pop,
                                       CANMale2010$MetabolicShare*CANMale2010$Deaths/CANMale2010$Pop,
                                       CANMale2010$RespShare*CANMale2010$Deaths/CANMale2010$Pop,
                                       CANMale2010$ExternalShare*CANMale2010$Deaths/CANMale2010$Pop,
                                       CANMale2010$NervShare*CANMale2010$Deaths/CANMale2010$Pop,
                                       CANMale2010$RestShare*CANMale2010$Deaths/CANMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixCANFemale2010 <- matrix(c(CANFemale2010$CVDShare*CANFemale2010$Deaths/CANFemale2010$Pop,
                                         CANFemale2010$CancerShare*CANFemale2010$Deaths/CANFemale2010$Pop,
                                         CANFemale2010$MetabolicShare*CANFemale2010$Deaths/CANFemale2010$Pop,
                                         CANFemale2010$RespShare*CANFemale2010$Deaths/CANFemale2010$Pop,
                                         CANFemale2010$ExternalShare*CANFemale2010$Deaths/CANFemale2010$Pop,
                                         CANFemale2010$NervShare*CANFemale2010$Deaths/CANFemale2010$Pop,
                                         CANFemale2010$RestShare*CANFemale2010$Deaths/CANFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixCANMale2019 <- matrix(c(CANMale2019$CVDShare*CANMale2019$Deaths/CANMale2019$Pop,
                                       CANMale2019$CancerShare*CANMale2019$Deaths/CANMale2019$Pop,
                                       CANMale2019$MetabolicShare*CANMale2019$Deaths/CANMale2019$Pop,
                                       CANMale2019$RespShare*CANMale2019$Deaths/CANMale2019$Pop,
                                       CANMale2019$ExternalShare*CANMale2019$Deaths/CANMale2019$Pop,
                                       CANMale2019$NervShare*CANMale2019$Deaths/CANMale2019$Pop,
                                       CANMale2019$RestShare*CANMale2019$Deaths/CANMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixCANFemale2019 <- matrix(c(CANFemale2019$CVDShare*CANFemale2019$Deaths/CANFemale2019$Pop,
                                         CANFemale2019$CancerShare*CANFemale2019$Deaths/CANFemale2019$Pop,
                                         CANFemale2019$MetabolicShare*CANFemale2019$Deaths/CANFemale2019$Pop,
                                         CANFemale2019$RespShare*CANFemale2019$Deaths/CANFemale2019$Pop,
                                         CANFemale2019$ExternalShare*CANFemale2019$Deaths/CANFemale2019$Pop,
                                         CANFemale2019$NervShare*CANFemale2019$Deaths/CANFemale2019$Pop,
                                         CANFemale2019$RestShare*CANFemale2019$Deaths/CANFemale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixCHIMale2000 <- matrix(c(CHIMale2000$CVDShare*CHIMale2000$Deaths/CHIMale2000$Pop,
                                       CHIMale2000$CancerShare*CHIMale2000$Deaths/CHIMale2000$Pop,
                                       CHIMale2000$MetabolicShare*CHIMale2000$Deaths/CHIMale2000$Pop,
                                       CHIMale2000$RespShare*CHIMale2000$Deaths/CHIMale2000$Pop,
                                       CHIMale2000$ExternalShare*CHIMale2000$Deaths/CHIMale2000$Pop,
                                       CHIMale2000$NervShare*CHIMale2000$Deaths/CHIMale2000$Pop,
                                       CHIMale2000$RestShare*CHIMale2000$Deaths/CHIMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixCHIFemale2000 <- matrix(c(CHIFemale2000$CVDShare*CHIFemale2000$Deaths/CHIFemale2000$Pop,
                                         CHIFemale2000$CancerShare*CHIFemale2000$Deaths/CHIFemale2000$Pop,
                                         CHIFemale2000$MetabolicShare*CHIFemale2000$Deaths/CHIFemale2000$Pop,
                                         CHIFemale2000$RespShare*CHIFemale2000$Deaths/CHIFemale2000$Pop,
                                         CHIFemale2000$ExternalShare*CHIFemale2000$Deaths/CHIFemale2000$Pop,
                                         CHIFemale2000$NervShare*CHIFemale2000$Deaths/CHIFemale2000$Pop,
                                         CHIFemale2000$RestShare*CHIFemale2000$Deaths/CHIFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixCHIMale2010 <- matrix(c(CHIMale2010$CVDShare*CHIMale2010$Deaths/CHIMale2010$Pop,
                                       CHIMale2010$CancerShare*CHIMale2010$Deaths/CHIMale2010$Pop,
                                       CHIMale2010$MetabolicShare*CHIMale2010$Deaths/CHIMale2010$Pop,
                                       CHIMale2010$RespShare*CHIMale2010$Deaths/CHIMale2010$Pop,
                                       CHIMale2010$ExternalShare*CHIMale2010$Deaths/CHIMale2010$Pop,
                                       CHIMale2010$NervShare*CHIMale2010$Deaths/CHIMale2010$Pop,
                                       CHIMale2010$RestShare*CHIMale2010$Deaths/CHIMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixCHIFemale2010 <- matrix(c(CHIFemale2010$CVDShare*CHIFemale2010$Deaths/CHIFemale2010$Pop,
                                         CHIFemale2010$CancerShare*CHIFemale2010$Deaths/CHIFemale2010$Pop,
                                         CHIFemale2010$MetabolicShare*CHIFemale2010$Deaths/CHIFemale2010$Pop,
                                         CHIFemale2010$RespShare*CHIFemale2010$Deaths/CHIFemale2010$Pop,
                                         CHIFemale2010$ExternalShare*CHIFemale2010$Deaths/CHIFemale2010$Pop,
                                         CHIFemale2010$NervShare*CHIFemale2010$Deaths/CHIFemale2010$Pop,
                                         CHIFemale2010$RestShare*CHIFemale2010$Deaths/CHIFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixCHIMale2019 <- matrix(c(CHIMale2019$CVDShare*CHIMale2019$Deaths/CHIMale2019$Pop,
                                       CHIMale2019$CancerShare*CHIMale2019$Deaths/CHIMale2019$Pop,
                                       CHIMale2019$MetabolicShare*CHIMale2019$Deaths/CHIMale2019$Pop,
                                       CHIMale2019$RespShare*CHIMale2019$Deaths/CHIMale2019$Pop,
                                       CHIMale2019$ExternalShare*CHIMale2019$Deaths/CHIMale2019$Pop,
                                       CHIMale2019$NervShare*CHIMale2019$Deaths/CHIMale2019$Pop,
                                       CHIMale2019$RestShare*CHIMale2019$Deaths/CHIMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixCHIFemale2019 <- matrix(c(CHIFemale2019$CVDShare*CHIFemale2019$Deaths/CHIFemale2019$Pop,
                                         CHIFemale2019$CancerShare*CHIFemale2019$Deaths/CHIFemale2019$Pop,
                                         CHIFemale2019$MetabolicShare*CHIFemale2019$Deaths/CHIFemale2019$Pop,
                                         CHIFemale2019$RespShare*CHIFemale2019$Deaths/CHIFemale2019$Pop,
                                         CHIFemale2019$ExternalShare*CHIFemale2019$Deaths/CHIFemale2019$Pop,
                                         CHIFemale2019$NervShare*CHIFemale2019$Deaths/CHIFemale2019$Pop,
                                         CHIFemale2019$RestShare*CHIFemale2019$Deaths/CHIFemale2019$Pop), nrow = 7, byrow = FALSE)


combined_matrixCOLMale2000 <- matrix(c(COLMale2000$CVDShare*COLMale2000$Deaths/COLMale2000$Pop,
                                       COLMale2000$CancerShare*COLMale2000$Deaths/COLMale2000$Pop,
                                       COLMale2000$MetabolicShare*COLMale2000$Deaths/COLMale2000$Pop,
                                       COLMale2000$RespShare*COLMale2000$Deaths/COLMale2000$Pop,
                                       COLMale2000$ExternalShare*COLMale2000$Deaths/COLMale2000$Pop,
                                       COLMale2000$NervShare*COLMale2000$Deaths/COLMale2000$Pop,
                                       COLMale2000$RestShare*COLMale2000$Deaths/COLMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixCOLFemale2000 <- matrix(c(COLFemale2000$CVDShare*COLFemale2000$Deaths/COLFemale2000$Pop,
                                         COLFemale2000$CancerShare*COLFemale2000$Deaths/COLFemale2000$Pop,
                                         COLFemale2000$MetabolicShare*COLFemale2000$Deaths/COLFemale2000$Pop,
                                         COLFemale2000$RespShare*COLFemale2000$Deaths/COLFemale2000$Pop,
                                         COLFemale2000$ExternalShare*COLFemale2000$Deaths/COLFemale2000$Pop,
                                         COLFemale2000$NervShare*COLFemale2000$Deaths/COLFemale2000$Pop,
                                         COLFemale2000$RestShare*COLFemale2000$Deaths/COLFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixCOLMale2010 <- matrix(c(COLMale2010$CVDShare*COLMale2010$Deaths/COLMale2010$Pop,
                                       COLMale2010$CancerShare*COLMale2010$Deaths/COLMale2010$Pop,
                                       COLMale2010$MetabolicShare*COLMale2010$Deaths/COLMale2010$Pop,
                                       COLMale2010$RespShare*COLMale2010$Deaths/COLMale2010$Pop,
                                       COLMale2010$ExternalShare*COLMale2010$Deaths/COLMale2010$Pop,
                                       COLMale2010$NervShare*COLMale2010$Deaths/COLMale2010$Pop,
                                       COLMale2010$RestShare*COLMale2010$Deaths/COLMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixCOLFemale2010 <- matrix(c(COLFemale2010$CVDShare*COLFemale2010$Deaths/COLFemale2010$Pop,
                                         COLFemale2010$CancerShare*COLFemale2010$Deaths/COLFemale2010$Pop,
                                         COLFemale2010$MetabolicShare*COLFemale2010$Deaths/COLFemale2010$Pop,
                                         COLFemale2010$RespShare*COLFemale2010$Deaths/COLFemale2010$Pop,
                                         COLFemale2010$ExternalShare*COLFemale2010$Deaths/COLFemale2010$Pop,
                                         COLFemale2010$NervShare*COLFemale2010$Deaths/COLFemale2010$Pop,
                                         COLFemale2010$RestShare*COLFemale2010$Deaths/COLFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixCOLMale2019 <- matrix(c(COLMale2019$CVDShare*COLMale2019$Deaths/COLMale2019$Pop,
                                       COLMale2019$CancerShare*COLMale2019$Deaths/COLMale2019$Pop,
                                       COLMale2019$MetabolicShare*COLMale2019$Deaths/COLMale2019$Pop,
                                       COLMale2019$RespShare*COLMale2019$Deaths/COLMale2019$Pop,
                                       COLMale2019$ExternalShare*COLMale2019$Deaths/COLMale2019$Pop,
                                       COLMale2019$NervShare*COLMale2019$Deaths/COLMale2019$Pop,
                                       COLMale2019$RestShare*COLMale2019$Deaths/COLMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixCOLFemale2019 <- matrix(c(COLFemale2019$CVDShare*COLFemale2019$Deaths/COLFemale2019$Pop,
                                         COLFemale2019$CancerShare*COLFemale2019$Deaths/COLFemale2019$Pop,
                                         COLFemale2019$MetabolicShare*COLFemale2019$Deaths/COLFemale2019$Pop,
                                         COLFemale2019$RespShare*COLFemale2019$Deaths/COLFemale2019$Pop,
                                         COLFemale2019$ExternalShare*COLFemale2019$Deaths/COLFemale2019$Pop,
                                         COLFemale2019$NervShare*COLFemale2019$Deaths/COLFemale2019$Pop,
                                         COLFemale2019$RestShare*COLFemale2019$Deaths/COLFemale2019$Pop), nrow = 7, byrow = FALSE)


combined_matrixCRIMale2000 <- matrix(c(CRIMale2000$CVDShare*CRIMale2000$Deaths/CRIMale2000$Pop,
                                       CRIMale2000$CancerShare*CRIMale2000$Deaths/CRIMale2000$Pop,
                                       CRIMale2000$MetabolicShare*CRIMale2000$Deaths/CRIMale2000$Pop,
                                       CRIMale2000$RespShare*CRIMale2000$Deaths/CRIMale2000$Pop,
                                       CRIMale2000$ExternalShare*CRIMale2000$Deaths/CRIMale2000$Pop,
                                       CRIMale2000$NervShare*CRIMale2000$Deaths/CRIMale2000$Pop,
                                       CRIMale2000$RestShare*CRIMale2000$Deaths/CRIMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixCRIFemale2000 <- matrix(c(CRIFemale2000$CVDShare*CRIFemale2000$Deaths/CRIFemale2000$Pop,
                                         CRIFemale2000$CancerShare*CRIFemale2000$Deaths/CRIFemale2000$Pop,
                                         CRIFemale2000$MetabolicShare*CRIFemale2000$Deaths/CRIFemale2000$Pop,
                                         CRIFemale2000$RespShare*CRIFemale2000$Deaths/CRIFemale2000$Pop,
                                         CRIFemale2000$ExternalShare*CRIFemale2000$Deaths/CRIFemale2000$Pop,
                                         CRIFemale2000$NervShare*CRIFemale2000$Deaths/CRIFemale2000$Pop,
                                         CRIFemale2000$RestShare*CRIFemale2000$Deaths/CRIFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixCRIMale2010 <- matrix(c(CRIMale2010$CVDShare*CRIMale2010$Deaths/CRIMale2010$Pop,
                                       CRIMale2010$CancerShare*CRIMale2010$Deaths/CRIMale2010$Pop,
                                       CRIMale2010$MetabolicShare*CRIMale2010$Deaths/CRIMale2010$Pop,
                                       CRIMale2010$RespShare*CRIMale2010$Deaths/CRIMale2010$Pop,
                                       CRIMale2010$ExternalShare*CRIMale2010$Deaths/CRIMale2010$Pop,
                                       CRIMale2010$NervShare*CRIMale2010$Deaths/CRIMale2010$Pop,
                                       CRIMale2010$RestShare*CRIMale2010$Deaths/CRIMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixCRIFemale2010 <- matrix(c(CRIFemale2010$CVDShare*CRIFemale2010$Deaths/CRIFemale2010$Pop,
                                         CRIFemale2010$CancerShare*CRIFemale2010$Deaths/CRIFemale2010$Pop,
                                         CRIFemale2010$MetabolicShare*CRIFemale2010$Deaths/CRIFemale2010$Pop,
                                         CRIFemale2010$RespShare*CRIFemale2010$Deaths/CRIFemale2010$Pop,
                                         CRIFemale2010$ExternalShare*CRIFemale2010$Deaths/CRIFemale2010$Pop,
                                         CRIFemale2010$NervShare*CRIFemale2010$Deaths/CRIFemale2010$Pop,
                                         CRIFemale2010$RestShare*CRIFemale2010$Deaths/CRIFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixCRIMale2019 <- matrix(c(CRIMale2019$CVDShare*CRIMale2019$Deaths/CRIMale2019$Pop,
                                       CRIMale2019$CancerShare*CRIMale2019$Deaths/CRIMale2019$Pop,
                                       CRIMale2019$MetabolicShare*CRIMale2019$Deaths/CRIMale2019$Pop,
                                       CRIMale2019$RespShare*CRIMale2019$Deaths/CRIMale2019$Pop,
                                       CRIMale2019$ExternalShare*CRIMale2019$Deaths/CRIMale2019$Pop,
                                       CRIMale2019$NervShare*CRIMale2019$Deaths/CRIMale2019$Pop,
                                       CRIMale2019$RestShare*CRIMale2019$Deaths/CRIMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixCRIFemale2019 <- matrix(c(CRIFemale2019$CVDShare*CRIFemale2019$Deaths/CRIFemale2019$Pop,
                                         CRIFemale2019$CancerShare*CRIFemale2019$Deaths/CRIFemale2019$Pop,
                                         CRIFemale2019$MetabolicShare*CRIFemale2019$Deaths/CRIFemale2019$Pop,
                                         CRIFemale2019$RespShare*CRIFemale2019$Deaths/CRIFemale2019$Pop,
                                         CRIFemale2019$ExternalShare*CRIFemale2019$Deaths/CRIFemale2019$Pop,
                                         CRIFemale2019$NervShare*CRIFemale2019$Deaths/CRIFemale2019$Pop,
                                         CRIFemale2019$RestShare*CRIFemale2019$Deaths/CRIFemale2019$Pop), nrow = 7, byrow = FALSE)


combined_matrixMEXMale2000 <- matrix(c(MEXMale2000$CVDShare*MEXMale2000$Deaths/MEXMale2000$Pop,
                                       MEXMale2000$CancerShare*MEXMale2000$Deaths/MEXMale2000$Pop,
                                       MEXMale2000$MetabolicShare*MEXMale2000$Deaths/MEXMale2000$Pop,
                                       MEXMale2000$RespShare*MEXMale2000$Deaths/MEXMale2000$Pop,
                                       MEXMale2000$ExternalShare*MEXMale2000$Deaths/MEXMale2000$Pop,
                                       MEXMale2000$NervShare*MEXMale2000$Deaths/MEXMale2000$Pop,
                                       MEXMale2000$RestShare*MEXMale2000$Deaths/MEXMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixMEXFemale2000 <- matrix(c(MEXFemale2000$CVDShare*MEXFemale2000$Deaths/MEXFemale2000$Pop,
                                         MEXFemale2000$CancerShare*MEXFemale2000$Deaths/MEXFemale2000$Pop,
                                         MEXFemale2000$MetabolicShare*MEXFemale2000$Deaths/MEXFemale2000$Pop,
                                         MEXFemale2000$RespShare*MEXFemale2000$Deaths/MEXFemale2000$Pop,
                                         MEXFemale2000$ExternalShare*MEXFemale2000$Deaths/MEXFemale2000$Pop,
                                         MEXFemale2000$NervShare*MEXFemale2000$Deaths/MEXFemale2000$Pop,
                                         MEXFemale2000$RestShare*MEXFemale2000$Deaths/MEXFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixMEXMale2010 <- matrix(c(MEXMale2010$CVDShare*MEXMale2010$Deaths/MEXMale2010$Pop,
                                       MEXMale2010$CancerShare*MEXMale2010$Deaths/MEXMale2010$Pop,
                                       MEXMale2010$MetabolicShare*MEXMale2010$Deaths/MEXMale2010$Pop,
                                       MEXMale2010$RespShare*MEXMale2010$Deaths/MEXMale2010$Pop,
                                       MEXMale2010$ExternalShare*MEXMale2010$Deaths/MEXMale2010$Pop,
                                       MEXMale2010$NervShare*MEXMale2010$Deaths/MEXMale2010$Pop,
                                       MEXMale2010$RestShare*MEXMale2010$Deaths/MEXMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixMEXFemale2010 <- matrix(c(MEXFemale2010$CVDShare*MEXFemale2010$Deaths/MEXFemale2010$Pop,
                                         MEXFemale2010$CancerShare*MEXFemale2010$Deaths/MEXFemale2010$Pop,
                                         MEXFemale2010$MetabolicShare*MEXFemale2010$Deaths/MEXFemale2010$Pop,
                                         MEXFemale2010$RespShare*MEXFemale2010$Deaths/MEXFemale2010$Pop,
                                         MEXFemale2010$ExternalShare*MEXFemale2010$Deaths/MEXFemale2010$Pop,
                                         MEXFemale2010$NervShare*MEXFemale2010$Deaths/MEXFemale2010$Pop,
                                         MEXFemale2010$RestShare*MEXFemale2010$Deaths/MEXFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixMEXMale2019 <- matrix(c(MEXMale2019$CVDShare*MEXMale2019$Deaths/MEXMale2019$Pop,
                                       MEXMale2019$CancerShare*MEXMale2019$Deaths/MEXMale2019$Pop,
                                       MEXMale2019$MetabolicShare*MEXMale2019$Deaths/MEXMale2019$Pop,
                                       MEXMale2019$RespShare*MEXMale2019$Deaths/MEXMale2019$Pop,
                                       MEXMale2019$ExternalShare*MEXMale2019$Deaths/MEXMale2019$Pop,
                                       MEXMale2019$NervShare*MEXMale2019$Deaths/MEXMale2019$Pop,
                                       MEXMale2019$RestShare*MEXMale2019$Deaths/MEXMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixMEXFemale2019 <- matrix(c(MEXFemale2019$CVDShare*MEXFemale2019$Deaths/MEXFemale2019$Pop,
                                         MEXFemale2019$CancerShare*MEXFemale2019$Deaths/MEXFemale2019$Pop,
                                         MEXFemale2019$MetabolicShare*MEXFemale2019$Deaths/MEXFemale2019$Pop,
                                         MEXFemale2019$RespShare*MEXFemale2019$Deaths/MEXFemale2019$Pop,
                                         MEXFemale2019$ExternalShare*MEXFemale2019$Deaths/MEXFemale2019$Pop,
                                         MEXFemale2019$NervShare*MEXFemale2019$Deaths/MEXFemale2019$Pop,
                                         MEXFemale2019$RestShare*MEXFemale2019$Deaths/MEXFemale2019$Pop), nrow = 7, byrow = FALSE)


combined_matrixPERMale2000 <- matrix(c(PERMale2000$CVDShare*PERMale2000$Deaths/PERMale2000$Pop,
                                       PERMale2000$CancerShare*PERMale2000$Deaths/PERMale2000$Pop,
                                       PERMale2000$MetabolicShare*PERMale2000$Deaths/PERMale2000$Pop,
                                       PERMale2000$RespShare*PERMale2000$Deaths/PERMale2000$Pop,
                                       PERMale2000$ExternalShare*PERMale2000$Deaths/PERMale2000$Pop,
                                       PERMale2000$NervShare*PERMale2000$Deaths/PERMale2000$Pop,
                                       PERMale2000$RestShare*PERMale2000$Deaths/PERMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixPERFemale2000 <- matrix(c(PERFemale2000$CVDShare*PERFemale2000$Deaths/PERFemale2000$Pop,
                                         PERFemale2000$CancerShare*PERFemale2000$Deaths/PERFemale2000$Pop,
                                         PERFemale2000$MetabolicShare*PERFemale2000$Deaths/PERFemale2000$Pop,
                                         PERFemale2000$RespShare*PERFemale2000$Deaths/PERFemale2000$Pop,
                                         PERFemale2000$ExternalShare*PERFemale2000$Deaths/PERFemale2000$Pop,
                                         PERFemale2000$NervShare*PERFemale2000$Deaths/PERFemale2000$Pop,
                                         PERFemale2000$RestShare*PERFemale2000$Deaths/PERFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixPERMale2010 <- matrix(c(PERMale2010$CVDShare*PERMale2010$Deaths/PERMale2010$Pop,
                                       PERMale2010$CancerShare*PERMale2010$Deaths/PERMale2010$Pop,
                                       PERMale2010$MetabolicShare*PERMale2010$Deaths/PERMale2010$Pop,
                                       PERMale2010$RespShare*PERMale2010$Deaths/PERMale2010$Pop,
                                       PERMale2010$ExternalShare*PERMale2010$Deaths/PERMale2010$Pop,
                                       PERMale2010$NervShare*PERMale2010$Deaths/PERMale2010$Pop,
                                       PERMale2010$RestShare*PERMale2010$Deaths/PERMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixPERFemale2010 <- matrix(c(PERFemale2010$CVDShare*PERFemale2010$Deaths/PERFemale2010$Pop,
                                         PERFemale2010$CancerShare*PERFemale2010$Deaths/PERFemale2010$Pop,
                                         PERFemale2010$MetabolicShare*PERFemale2010$Deaths/PERFemale2010$Pop,
                                         PERFemale2010$RespShare*PERFemale2010$Deaths/PERFemale2010$Pop,
                                         PERFemale2010$ExternalShare*PERFemale2010$Deaths/PERFemale2010$Pop,
                                         PERFemale2010$NervShare*PERFemale2010$Deaths/PERFemale2010$Pop,
                                         PERFemale2010$RestShare*PERFemale2010$Deaths/PERFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixPERMale2019 <- matrix(c(PERMale2019$CVDShare*PERMale2019$Deaths/PERMale2019$Pop,
                                       PERMale2019$CancerShare*PERMale2019$Deaths/PERMale2019$Pop,
                                       PERMale2019$MetabolicShare*PERMale2019$Deaths/PERMale2019$Pop,
                                       PERMale2019$RespShare*PERMale2019$Deaths/PERMale2019$Pop,
                                       PERMale2019$ExternalShare*PERMale2019$Deaths/PERMale2019$Pop,
                                       PERMale2019$NervShare*PERMale2019$Deaths/PERMale2019$Pop,
                                       PERMale2019$RestShare*PERMale2019$Deaths/PERMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixPERFemale2019 <- matrix(c(PERFemale2019$CVDShare*PERFemale2019$Deaths/PERFemale2019$Pop,
                                         PERFemale2019$CancerShare*PERFemale2019$Deaths/PERFemale2019$Pop,
                                         PERFemale2019$MetabolicShare*PERFemale2019$Deaths/PERFemale2019$Pop,
                                         PERFemale2019$RespShare*PERFemale2019$Deaths/PERFemale2019$Pop,
                                         PERFemale2019$ExternalShare*PERFemale2019$Deaths/PERFemale2019$Pop,
                                         PERFemale2019$NervShare*PERFemale2019$Deaths/PERFemale2019$Pop,
                                         PERFemale2019$RestShare*PERFemale2019$Deaths/PERFemale2019$Pop), nrow = 7, byrow = FALSE)


combined_matrixAVGMale2000 <- matrix(c(AVGMale2000$CVDShare*AVGMale2000$Deaths/AVGMale2000$Pop,
                                       AVGMale2000$CancerShare*AVGMale2000$Deaths/AVGMale2000$Pop,
                                       AVGMale2000$MetabolicShare*AVGMale2000$Deaths/AVGMale2000$Pop,
                                       AVGMale2000$RespShare*AVGMale2000$Deaths/AVGMale2000$Pop,
                                       AVGMale2000$ExternalShare*AVGMale2000$Deaths/AVGMale2000$Pop,
                                       AVGMale2000$NervShare*AVGMale2000$Deaths/AVGMale2000$Pop,
                                       AVGMale2000$RestShare*AVGMale2000$Deaths/AVGMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGFemale2000 <- matrix(c(AVGFemale2000$CVDShare*AVGFemale2000$Deaths/AVGFemale2000$Pop,
                                         AVGFemale2000$CancerShare*AVGFemale2000$Deaths/AVGFemale2000$Pop,
                                         AVGFemale2000$MetabolicShare*AVGFemale2000$Deaths/AVGFemale2000$Pop,
                                         AVGFemale2000$RespShare*AVGFemale2000$Deaths/AVGFemale2000$Pop,
                                         AVGFemale2000$ExternalShare*AVGFemale2000$Deaths/AVGFemale2000$Pop,
                                         AVGFemale2000$NervShare*AVGFemale2000$Deaths/AVGFemale2000$Pop,
                                         AVGFemale2000$RestShare*AVGFemale2000$Deaths/AVGFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGMale2010 <- matrix(c(AVGMale2010$CVDShare*AVGMale2010$Deaths/AVGMale2010$Pop,
                                       AVGMale2010$CancerShare*AVGMale2010$Deaths/AVGMale2010$Pop,
                                       AVGMale2010$MetabolicShare*AVGMale2010$Deaths/AVGMale2010$Pop,
                                       AVGMale2010$RespShare*AVGMale2010$Deaths/AVGMale2010$Pop,
                                       AVGMale2010$ExternalShare*AVGMale2010$Deaths/AVGMale2010$Pop,
                                       AVGMale2010$NervShare*AVGMale2010$Deaths/AVGMale2010$Pop,
                                       AVGMale2010$RestShare*AVGMale2010$Deaths/AVGMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGFemale2010 <- matrix(c(AVGFemale2010$CVDShare*AVGFemale2010$Deaths/AVGFemale2010$Pop,
                                         AVGFemale2010$CancerShare*AVGFemale2010$Deaths/AVGFemale2010$Pop,
                                         AVGFemale2010$MetabolicShare*AVGFemale2010$Deaths/AVGFemale2010$Pop,
                                         AVGFemale2010$RespShare*AVGFemale2010$Deaths/AVGFemale2010$Pop,
                                         AVGFemale2010$ExternalShare*AVGFemale2010$Deaths/AVGFemale2010$Pop,
                                         AVGFemale2010$NervShare*AVGFemale2010$Deaths/AVGFemale2010$Pop,
                                         AVGFemale2010$RestShare*AVGFemale2010$Deaths/AVGFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGMale2019 <- matrix(c(AVGMale2019$CVDShare*AVGMale2019$Deaths/AVGMale2019$Pop,
                                       AVGMale2019$CancerShare*AVGMale2019$Deaths/AVGMale2019$Pop,
                                       AVGMale2019$MetabolicShare*AVGMale2019$Deaths/AVGMale2019$Pop,
                                       AVGMale2019$RespShare*AVGMale2019$Deaths/AVGMale2019$Pop,
                                       AVGMale2019$ExternalShare*AVGMale2019$Deaths/AVGMale2019$Pop,
                                       AVGMale2019$NervShare*AVGMale2019$Deaths/AVGMale2019$Pop,
                                       AVGMale2019$RestShare*AVGMale2019$Deaths/AVGMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGFemale2019 <- matrix(c(AVGFemale2019$CVDShare*AVGFemale2019$Deaths/AVGFemale2019$Pop,
                                         AVGFemale2019$CancerShare*AVGFemale2019$Deaths/AVGFemale2019$Pop,
                                         AVGFemale2019$MetabolicShare*AVGFemale2019$Deaths/AVGFemale2019$Pop,
                                         AVGFemale2019$RespShare*AVGFemale2019$Deaths/AVGFemale2019$Pop,
                                         AVGFemale2019$ExternalShare*AVGFemale2019$Deaths/AVGFemale2019$Pop,
                                         AVGFemale2019$NervShare*AVGFemale2019$Deaths/AVGFemale2019$Pop,
                                         AVGFemale2019$RestShare*AVGFemale2019$Deaths/AVGFemale2019$Pop), nrow = 7, byrow = FALSE)

# this to add the average without Mexico


combined_matrixAVGnoMexMale2000 <- matrix(c(AVGnoMexMale2000$CVDShare*AVGnoMexMale2000$Deaths/AVGnoMexMale2000$Pop,
                                            AVGnoMexMale2000$CancerShare*AVGnoMexMale2000$Deaths/AVGnoMexMale2000$Pop,
                                            AVGnoMexMale2000$MetabolicShare*AVGnoMexMale2000$Deaths/AVGnoMexMale2000$Pop,
                                            AVGnoMexMale2000$RespShare*AVGnoMexMale2000$Deaths/AVGnoMexMale2000$Pop,
                                            AVGnoMexMale2000$ExternalShare*AVGnoMexMale2000$Deaths/AVGnoMexMale2000$Pop,
                                            AVGnoMexMale2000$NervShare*AVGnoMexMale2000$Deaths/AVGnoMexMale2000$Pop,
                                            AVGnoMexMale2000$RestShare*AVGnoMexMale2000$Deaths/AVGnoMexMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGnoMexFemale2000 <- matrix(c(AVGnoMexFemale2000$CVDShare*AVGnoMexFemale2000$Deaths/AVGnoMexFemale2000$Pop,
                                              AVGnoMexFemale2000$CancerShare*AVGnoMexFemale2000$Deaths/AVGnoMexFemale2000$Pop,
                                              AVGnoMexFemale2000$MetabolicShare*AVGnoMexFemale2000$Deaths/AVGnoMexFemale2000$Pop,
                                              AVGnoMexFemale2000$RespShare*AVGnoMexFemale2000$Deaths/AVGnoMexFemale2000$Pop,
                                              AVGnoMexFemale2000$ExternalShare*AVGnoMexFemale2000$Deaths/AVGnoMexFemale2000$Pop,
                                              AVGnoMexFemale2000$NervShare*AVGnoMexFemale2000$Deaths/AVGnoMexFemale2000$Pop,
                                              AVGnoMexFemale2000$RestShare*AVGnoMexFemale2000$Deaths/AVGnoMexFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGnoMexMale2010 <- matrix(c(AVGnoMexMale2010$CVDShare*AVGnoMexMale2010$Deaths/AVGnoMexMale2010$Pop,
                                            AVGnoMexMale2010$CancerShare*AVGnoMexMale2010$Deaths/AVGnoMexMale2010$Pop,
                                            AVGnoMexMale2010$MetabolicShare*AVGnoMexMale2010$Deaths/AVGnoMexMale2010$Pop,
                                            AVGnoMexMale2010$RespShare*AVGnoMexMale2010$Deaths/AVGnoMexMale2010$Pop,
                                            AVGnoMexMale2010$ExternalShare*AVGnoMexMale2010$Deaths/AVGnoMexMale2010$Pop,
                                            AVGnoMexMale2010$NervShare*AVGnoMexMale2010$Deaths/AVGnoMexMale2010$Pop,
                                            AVGnoMexMale2010$RestShare*AVGnoMexMale2010$Deaths/AVGnoMexMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGnoMexFemale2010 <- matrix(c(AVGnoMexFemale2010$CVDShare*AVGnoMexFemale2010$Deaths/AVGnoMexFemale2010$Pop,
                                              AVGnoMexFemale2010$CancerShare*AVGnoMexFemale2010$Deaths/AVGnoMexFemale2010$Pop,
                                              AVGnoMexFemale2010$MetabolicShare*AVGnoMexFemale2010$Deaths/AVGnoMexFemale2010$Pop,
                                              AVGnoMexFemale2010$RespShare*AVGnoMexFemale2010$Deaths/AVGnoMexFemale2010$Pop,
                                              AVGnoMexFemale2010$ExternalShare*AVGnoMexFemale2010$Deaths/AVGnoMexFemale2010$Pop,
                                              AVGnoMexFemale2010$NervShare*AVGnoMexFemale2010$Deaths/AVGnoMexFemale2010$Pop,
                                              AVGnoMexFemale2010$RestShare*AVGnoMexFemale2010$Deaths/AVGnoMexFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGnoMexMale2019 <- matrix(c(AVGnoMexMale2019$CVDShare*AVGnoMexMale2019$Deaths/AVGnoMexMale2019$Pop,
                                            AVGnoMexMale2019$CancerShare*AVGnoMexMale2019$Deaths/AVGnoMexMale2019$Pop,
                                            AVGnoMexMale2019$MetabolicShare*AVGnoMexMale2019$Deaths/AVGnoMexMale2019$Pop,
                                            AVGnoMexMale2019$RespShare*AVGnoMexMale2019$Deaths/AVGnoMexMale2019$Pop,
                                            AVGnoMexMale2019$ExternalShare*AVGnoMexMale2019$Deaths/AVGnoMexMale2019$Pop,
                                            AVGnoMexMale2019$NervShare*AVGnoMexMale2019$Deaths/AVGnoMexMale2019$Pop,
                                            AVGnoMexMale2019$RestShare*AVGnoMexMale2019$Deaths/AVGnoMexMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixAVGnoMexFemale2019 <- matrix(c(AVGnoMexFemale2019$CVDShare*AVGnoMexFemale2019$Deaths/AVGnoMexFemale2019$Pop,
                                              AVGnoMexFemale2019$CancerShare*AVGnoMexFemale2019$Deaths/AVGnoMexFemale2019$Pop,
                                              AVGnoMexFemale2019$MetabolicShare*AVGnoMexFemale2019$Deaths/AVGnoMexFemale2019$Pop,
                                              AVGnoMexFemale2019$RespShare*AVGnoMexFemale2019$Deaths/AVGnoMexFemale2019$Pop,
                                              AVGnoMexFemale2019$ExternalShare*AVGnoMexFemale2019$Deaths/AVGnoMexFemale2019$Pop,
                                              AVGnoMexFemale2019$NervShare*AVGnoMexFemale2019$Deaths/AVGnoMexFemale2019$Pop,
                                              AVGnoMexFemale2019$RestShare*AVGnoMexFemale2019$Deaths/AVGnoMexFemale2019$Pop), nrow = 7, byrow = FALSE)



combined_matrixUSAMale2000 <- matrix(c(USAMale2000$CVDShare*USAMale2000$Deaths/USAMale2000$Pop,
                                       USAMale2000$CancerShare*USAMale2000$Deaths/USAMale2000$Pop,
                                       USAMale2000$MetabolicShare*USAMale2000$Deaths/USAMale2000$Pop,
                                       USAMale2000$RespShare*USAMale2000$Deaths/USAMale2000$Pop,
                                       USAMale2000$ExternalShare*USAMale2000$Deaths/USAMale2000$Pop,
                                       USAMale2000$NervShare*USAMale2000$Deaths/USAMale2000$Pop,
                                       USAMale2000$RestShare*USAMale2000$Deaths/USAMale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixUSAFemale2000 <- matrix(c(USAFemale2000$CVDShare*USAFemale2000$Deaths/USAFemale2000$Pop,
                                         USAFemale2000$CancerShare*USAFemale2000$Deaths/USAFemale2000$Pop,
                                         USAFemale2000$MetabolicShare*USAFemale2000$Deaths/USAFemale2000$Pop,
                                         USAFemale2000$RespShare*USAFemale2000$Deaths/USAFemale2000$Pop,
                                         USAFemale2000$ExternalShare*USAFemale2000$Deaths/USAFemale2000$Pop,
                                         USAFemale2000$NervShare*USAFemale2000$Deaths/USAFemale2000$Pop,
                                         USAFemale2000$RestShare*USAFemale2000$Deaths/USAFemale2000$Pop), nrow = 7, byrow = FALSE)

combined_matrixUSAMale2010 <- matrix(c(USAMale2010$CVDShare*USAMale2010$Deaths/USAMale2010$Pop,
                                       USAMale2010$CancerShare*USAMale2010$Deaths/USAMale2010$Pop,
                                       USAMale2010$MetabolicShare*USAMale2010$Deaths/USAMale2010$Pop,
                                       USAMale2010$RespShare*USAMale2010$Deaths/USAMale2010$Pop,
                                       USAMale2010$ExternalShare*USAMale2010$Deaths/USAMale2010$Pop,
                                       USAMale2010$NervShare*USAMale2010$Deaths/USAMale2010$Pop,
                                       USAMale2010$RestShare*USAMale2010$Deaths/USAMale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixUSAFemale2010 <- matrix(c(USAFemale2010$CVDShare*USAFemale2010$Deaths/USAFemale2010$Pop,
                                         USAFemale2010$CancerShare*USAFemale2010$Deaths/USAFemale2010$Pop,
                                         USAFemale2010$MetabolicShare*USAFemale2010$Deaths/USAFemale2010$Pop,
                                         USAFemale2010$RespShare*USAFemale2010$Deaths/USAFemale2010$Pop,
                                         USAFemale2010$ExternalShare*USAFemale2010$Deaths/USAFemale2010$Pop,
                                         USAFemale2010$NervShare*USAFemale2010$Deaths/USAFemale2010$Pop,
                                         USAFemale2010$RestShare*USAFemale2010$Deaths/USAFemale2010$Pop), nrow = 7, byrow = FALSE)

combined_matrixUSAMale2019 <- matrix(c(USAMale2019$CVDShare*USAMale2019$Deaths/USAMale2019$Pop,
                                       USAMale2019$CancerShare*USAMale2019$Deaths/USAMale2019$Pop,
                                       USAMale2019$MetabolicShare*USAMale2019$Deaths/USAMale2019$Pop,
                                       USAMale2019$RespShare*USAMale2019$Deaths/USAMale2019$Pop,
                                       USAMale2019$ExternalShare*USAMale2019$Deaths/USAMale2019$Pop,
                                       USAMale2019$NervShare*USAMale2019$Deaths/USAMale2019$Pop,
                                       USAMale2019$RestShare*USAMale2019$Deaths/USAMale2019$Pop), nrow = 7, byrow = FALSE)

combined_matrixUSAFemale2019 <- matrix(c(USAFemale2019$CVDShare*USAFemale2019$Deaths/USAFemale2019$Pop,
                                         USAFemale2019$CancerShare*USAFemale2019$Deaths/USAFemale2019$Pop,
                                         USAFemale2019$MetabolicShare*USAFemale2019$Deaths/USAFemale2019$Pop,
                                         USAFemale2019$RespShare*USAFemale2019$Deaths/USAFemale2019$Pop,
                                         USAFemale2019$ExternalShare*USAFemale2019$Deaths/USAFemale2019$Pop,
                                         USAFemale2019$NervShare*USAFemale2019$Deaths/USAFemale2019$Pop,
                                         USAFemale2019$RestShare*USAFemale2019$Deaths/USAFemale2019$Pop), nrow = 7, byrow = FALSE)



# coNvert to vec 
combined_vecARGMale2000<- mat_to_vec(combined_matrixARGMale2000)
combined_vecARGMale2010<- mat_to_vec(combined_matrixARGMale2010)
combined_vecARGMale2019 <- mat_to_vec(combined_matrixARGMale2019)

combined_vecARGFemale2000<- mat_to_vec(combined_matrixARGFemale2000)
combined_vecARGFemale2010<- mat_to_vec(combined_matrixARGFemale2010)
combined_vecARGFemale2019 <- mat_to_vec(combined_matrixARGFemale2019)

combined_vecBRAMale2000<- mat_to_vec(combined_matrixBRAMale2000)
combined_vecBRAMale2010<- mat_to_vec(combined_matrixBRAMale2010)
combined_vecBRAMale2019 <- mat_to_vec(combined_matrixBRAMale2019)

combined_vecBRAFemale2000<- mat_to_vec(combined_matrixBRAFemale2000)
combined_vecBRAFemale2010<- mat_to_vec(combined_matrixBRAFemale2010)
combined_vecBRAFemale2019 <- mat_to_vec(combined_matrixBRAFemale2019)

combined_vecCANMale2000<- mat_to_vec(combined_matrixCANMale2000)
combined_vecCANMale2010<- mat_to_vec(combined_matrixCANMale2010)
combined_vecCANMale2019 <- mat_to_vec(combined_matrixCANMale2019)

combined_vecCANFemale2000<- mat_to_vec(combined_matrixCANFemale2000)
combined_vecCANFemale2010<- mat_to_vec(combined_matrixCANFemale2010)
combined_vecCANFemale2019 <- mat_to_vec(combined_matrixCANFemale2019)

combined_vecCHIMale2000<- mat_to_vec(combined_matrixCHIMale2000)
combined_vecCHIMale2010<- mat_to_vec(combined_matrixCHIMale2010)
combined_vecCHIMale2019 <- mat_to_vec(combined_matrixCHIMale2019)

combined_vecCHIFemale2000<- mat_to_vec(combined_matrixCHIFemale2000)
combined_vecCHIFemale2010<- mat_to_vec(combined_matrixCHIFemale2010)
combined_vecCHIFemale2019 <- mat_to_vec(combined_matrixCHIFemale2019)

combined_vecCOLMale2000<- mat_to_vec(combined_matrixCOLMale2000)
combined_vecCOLMale2010<- mat_to_vec(combined_matrixCOLMale2010)
combined_vecCOLMale2019 <- mat_to_vec(combined_matrixCOLMale2019)

combined_vecCOLFemale2000<- mat_to_vec(combined_matrixCOLFemale2000)
combined_vecCOLFemale2010<- mat_to_vec(combined_matrixCOLFemale2010)
combined_vecCOLFemale2019 <- mat_to_vec(combined_matrixCOLFemale2019)

combined_vecCRIMale2000<- mat_to_vec(combined_matrixCRIMale2000)
combined_vecCRIMale2010<- mat_to_vec(combined_matrixCRIMale2010)
combined_vecCRIMale2019 <- mat_to_vec(combined_matrixCRIMale2019)

combined_vecCRIFemale2000<- mat_to_vec(combined_matrixCRIFemale2000)
combined_vecCRIFemale2010<- mat_to_vec(combined_matrixCRIFemale2010)
combined_vecCRIFemale2019 <- mat_to_vec(combined_matrixCRIFemale2019)

combined_vecMEXMale2000<- mat_to_vec(combined_matrixMEXMale2000)
combined_vecMEXMale2010<- mat_to_vec(combined_matrixMEXMale2010)
combined_vecMEXMale2019 <- mat_to_vec(combined_matrixMEXMale2019)

combined_vecMEXFemale2000<- mat_to_vec(combined_matrixMEXFemale2000)
combined_vecMEXFemale2010<- mat_to_vec(combined_matrixMEXFemale2010)
combined_vecMEXFemale2019 <- mat_to_vec(combined_matrixMEXFemale2019)

combined_vecPERMale2000<- mat_to_vec(combined_matrixPERMale2000)
combined_vecPERMale2010<- mat_to_vec(combined_matrixPERMale2010)
combined_vecPERMale2019 <- mat_to_vec(combined_matrixPERMale2019)

combined_vecPERFemale2000<- mat_to_vec(combined_matrixPERFemale2000)
combined_vecPERFemale2010<- mat_to_vec(combined_matrixPERFemale2010)
combined_vecPERFemale2019 <- mat_to_vec(combined_matrixPERFemale2019)

combined_vecAVGMale2000<- mat_to_vec(combined_matrixAVGMale2000)
combined_vecAVGMale2010<- mat_to_vec(combined_matrixAVGMale2010)
combined_vecAVGMale2019 <- mat_to_vec(combined_matrixAVGMale2019)

combined_vecAVGFemale2000<- mat_to_vec(combined_matrixAVGFemale2000)
combined_vecAVGFemale2010<- mat_to_vec(combined_matrixAVGFemale2010)
combined_vecAVGFemale2019 <- mat_to_vec(combined_matrixAVGFemale2019)

combined_vecAVGnoMexMale2000<- mat_to_vec(combined_matrixAVGnoMexMale2000)
combined_vecAVGnoMexMale2010<- mat_to_vec(combined_matrixAVGnoMexMale2010)
combined_vecAVGnoMexMale2019 <- mat_to_vec(combined_matrixAVGnoMexMale2019)

combined_vecAVGnoMexFemale2000<- mat_to_vec(combined_matrixAVGnoMexFemale2000)
combined_vecAVGnoMexFemale2010<- mat_to_vec(combined_matrixAVGnoMexFemale2010)
combined_vecAVGnoMexFemale2019 <- mat_to_vec(combined_matrixAVGnoMexFemale2019)


combined_vecUSAMale2000<- mat_to_vec(combined_matrixUSAMale2000)
combined_vecUSAMale2010<- mat_to_vec(combined_matrixUSAMale2010)
combined_vecUSAMale2019 <- mat_to_vec(combined_matrixUSAMale2019)

combined_vecUSAFemale2000<- mat_to_vec(combined_matrixUSAFemale2000)
combined_vecUSAFemale2010<- mat_to_vec(combined_matrixUSAFemale2010)
combined_vecUSAFemale2019 <- mat_to_vec(combined_matrixUSAFemale2019)

# this works separately 



ARGARG_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecARGMale2000,
                                            pars2=combined_vecARGMale2010,
                                            N=20,
                                            dims = c(7,7))
ARGARG_horiuchiMaleAll20002010mat <- vec_to_mat(ARGARG_horiuchiMaleAll20002010 , c(7,7))
dimnames(ARGARG_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


ARGARG_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecARGMale2010,
                                            pars2=combined_vecARGMale2019,
                                            N=20,
                                            dims = c(7,7))
ARGARG_horiuchiMaleAll20102019mat <- vec_to_mat(ARGARG_horiuchiMaleAll20102019 , c(7,7))
dimnames(ARGARG_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



ARGARG_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecARGFemale2000,
                                              pars2=combined_vecARGFemale2010,
                                              N=20,
                                              dims = c(7,7))
ARGARG_horiuchiFemaleAll20002010mat <- vec_to_mat(ARGARG_horiuchiFemaleAll20002010 , c(7,7))
dimnames(ARGARG_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


ARGARG_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecARGFemale2010,
                                              pars2=combined_vecARGFemale2019,
                                              N=20,
                                              dims = c(7,7))
ARGARG_horiuchiFemaleAll20102019mat <- vec_to_mat(ARGARG_horiuchiFemaleAll20102019 , c(7,7))
dimnames(ARGARG_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


BRABRA_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecBRAMale2000,
                                            pars2=combined_vecBRAMale2010,
                                            N=20,
                                            dims = c(7,7))
BRABRA_horiuchiMaleAll20002010mat <- vec_to_mat(BRABRA_horiuchiMaleAll20002010 , c(7,7))
dimnames(BRABRA_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


BRABRA_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecBRAMale2010,
                                            pars2=combined_vecBRAMale2019,
                                            N=20,
                                            dims = c(7,7))
BRABRA_horiuchiMaleAll20102019mat <- vec_to_mat(BRABRA_horiuchiMaleAll20102019 , c(7,7))
dimnames(BRABRA_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



BRABRA_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecBRAFemale2000,
                                              pars2=combined_vecBRAFemale2010,
                                              N=20,
                                              dims = c(7,7))
BRABRA_horiuchiFemaleAll20002010mat <- vec_to_mat(BRABRA_horiuchiFemaleAll20002010 , c(7,7))
dimnames(BRABRA_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


BRABRA_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecBRAFemale2010,
                                              pars2=combined_vecBRAFemale2019,
                                              N=20,
                                              dims = c(7,7))
BRABRA_horiuchiFemaleAll20102019mat <- vec_to_mat(BRABRA_horiuchiFemaleAll20102019 , c(7,7))
dimnames(BRABRA_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


CANCAN_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecCANMale2000,
                                            pars2=combined_vecCANMale2010,
                                            N=20,
                                            dims = c(7,7))
CANCAN_horiuchiMaleAll20002010mat <- vec_to_mat(CANCAN_horiuchiMaleAll20002010 , c(7,7))
dimnames(CANCAN_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


CANCAN_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecCANMale2010,
                                            pars2=combined_vecCANMale2019,
                                            N=20,
                                            dims = c(7,7))
CANCAN_horiuchiMaleAll20102019mat <- vec_to_mat(CANCAN_horiuchiMaleAll20102019 , c(7,7))
dimnames(CANCAN_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



CANCAN_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecCANFemale2000,
                                              pars2=combined_vecCANFemale2010,
                                              N=20,
                                              dims = c(7,7))
CANCAN_horiuchiFemaleAll20002010mat <- vec_to_mat(CANCAN_horiuchiFemaleAll20002010 , c(7,7))
dimnames(CANCAN_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


CANCAN_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecCANFemale2010,
                                              pars2=combined_vecCANFemale2019,
                                              N=20,
                                              dims = c(7,7))
CANCAN_horiuchiFemaleAll20102019mat <- vec_to_mat(CANCAN_horiuchiFemaleAll20102019 , c(7,7))
dimnames(CANCAN_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


CHICHI_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecCHIMale2000,
                                            pars2=combined_vecCHIMale2010,
                                            N=20,
                                            dims = c(7,7))
CHICHI_horiuchiMaleAll20002010mat <- vec_to_mat(CHICHI_horiuchiMaleAll20002010 , c(7,7))
dimnames(CHICHI_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


CHICHI_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecCHIMale2010,
                                            pars2=combined_vecCHIMale2019,
                                            N=20,
                                            dims = c(7,7))
CHICHI_horiuchiMaleAll20102019mat <- vec_to_mat(CHICHI_horiuchiMaleAll20102019 , c(7,7))
dimnames(CHICHI_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



CHICHI_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecCHIFemale2000,
                                              pars2=combined_vecCHIFemale2010,
                                              N=20,
                                              dims = c(7,7))
CHICHI_horiuchiFemaleAll20002010mat <- vec_to_mat(CHICHI_horiuchiFemaleAll20002010 , c(7,7))
dimnames(CHICHI_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


CHICHI_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecCHIFemale2010,
                                              pars2=combined_vecCHIFemale2019,
                                              N=20,
                                              dims = c(7,7))
CHICHI_horiuchiFemaleAll20102019mat <- vec_to_mat(CHICHI_horiuchiFemaleAll20102019 , c(7,7))
dimnames(CHICHI_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


CRICRI_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecCRIMale2000,
                                            pars2=combined_vecCRIMale2010,
                                            N=20,
                                            dims = c(7,7))
CRICRI_horiuchiMaleAll20002010mat <- vec_to_mat(CRICRI_horiuchiMaleAll20002010 , c(7,7))
dimnames(CRICRI_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


CRICRI_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecCRIMale2010,
                                            pars2=combined_vecCRIMale2019,
                                            N=20,
                                            dims = c(7,7))
CRICRI_horiuchiMaleAll20102019mat <- vec_to_mat(CRICRI_horiuchiMaleAll20102019 , c(7,7))
dimnames(CRICRI_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



CRICRI_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecCRIFemale2000,
                                              pars2=combined_vecCRIFemale2010,
                                              N=20,
                                              dims = c(7,7))
CRICRI_horiuchiFemaleAll20002010mat <- vec_to_mat(CRICRI_horiuchiFemaleAll20002010 , c(7,7))
dimnames(CRICRI_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


CRICRI_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecCRIFemale2010,
                                              pars2=combined_vecCRIFemale2019,
                                              N=20,
                                              dims = c(7,7))
CRICRI_horiuchiFemaleAll20102019mat <- vec_to_mat(CRICRI_horiuchiFemaleAll20102019 , c(7,7))
dimnames(CRICRI_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


COLCOL_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecCOLMale2000,
                                            pars2=combined_vecCOLMale2010,
                                            N=20,
                                            dims = c(7,7))
COLCOL_horiuchiMaleAll20002010mat <- vec_to_mat(COLCOL_horiuchiMaleAll20002010 , c(7,7))
dimnames(COLCOL_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


COLCOL_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecCOLMale2010,
                                            pars2=combined_vecCOLMale2019,
                                            N=20,
                                            dims = c(7,7))
COLCOL_horiuchiMaleAll20102019mat <- vec_to_mat(COLCOL_horiuchiMaleAll20102019 , c(7,7))
dimnames(COLCOL_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



COLCOL_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecCOLFemale2000,
                                              pars2=combined_vecCOLFemale2010,
                                              N=20,
                                              dims = c(7,7))
COLCOL_horiuchiFemaleAll20002010mat <- vec_to_mat(COLCOL_horiuchiFemaleAll20002010 , c(7,7))
dimnames(COLCOL_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


COLCOL_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecCOLFemale2010,
                                              pars2=combined_vecCOLFemale2019,
                                              N=20,
                                              dims = c(7,7))
COLCOL_horiuchiFemaleAll20102019mat <- vec_to_mat(COLCOL_horiuchiFemaleAll20102019 , c(7,7))
dimnames(COLCOL_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


MEXMEX_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecMEXMale2000,
                                            pars2=combined_vecMEXMale2010,
                                            N=20,
                                            dims = c(7,7))
MEXMEX_horiuchiMaleAll20002010mat <- vec_to_mat(MEXMEX_horiuchiMaleAll20002010 , c(7,7))
dimnames(MEXMEX_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


MEXMEX_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecMEXMale2010,
                                            pars2=combined_vecMEXMale2019,
                                            N=20,
                                            dims = c(7,7))
MEXMEX_horiuchiMaleAll20102019mat <- vec_to_mat(MEXMEX_horiuchiMaleAll20102019 , c(7,7))
dimnames(MEXMEX_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



MEXMEX_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecMEXFemale2000,
                                              pars2=combined_vecMEXFemale2010,
                                              N=20,
                                              dims = c(7,7))
MEXMEX_horiuchiFemaleAll20002010mat <- vec_to_mat(MEXMEX_horiuchiFemaleAll20002010 , c(7,7))
dimnames(MEXMEX_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


MEXMEX_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecMEXFemale2010,
                                              pars2=combined_vecMEXFemale2019,
                                              N=20,
                                              dims = c(7,7))
MEXMEX_horiuchiFemaleAll20102019mat <- vec_to_mat(MEXMEX_horiuchiFemaleAll20102019 , c(7,7))
dimnames(MEXMEX_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


PERPER_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecPERMale2000,
                                            pars2=combined_vecPERMale2010,
                                            N=20,
                                            dims = c(7,7))
PERPER_horiuchiMaleAll20002010mat <- vec_to_mat(PERPER_horiuchiMaleAll20002010 , c(7,7))
dimnames(PERPER_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


PERPER_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecPERMale2010,
                                            pars2=combined_vecPERMale2019,
                                            N=20,
                                            dims = c(7,7))
PERPER_horiuchiMaleAll20102019mat <- vec_to_mat(PERPER_horiuchiMaleAll20102019 , c(7,7))
dimnames(PERPER_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



PERPER_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecPERFemale2000,
                                              pars2=combined_vecPERFemale2010,
                                              N=20,
                                              dims = c(7,7))
PERPER_horiuchiFemaleAll20002010mat <- vec_to_mat(PERPER_horiuchiFemaleAll20002010 , c(7,7))
dimnames(PERPER_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


PERPER_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecPERFemale2010,
                                              pars2=combined_vecPERFemale2019,
                                              N=20,
                                              dims = c(7,7))
PERPER_horiuchiFemaleAll20102019mat <- vec_to_mat(PERPER_horiuchiFemaleAll20102019 , c(7,7))
dimnames(PERPER_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


AVGAVG_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecAVGMale2000,
                                            pars2=combined_vecAVGMale2010,
                                            N=20,
                                            dims = c(7,7))
AVGAVG_horiuchiMaleAll20002010mat <- vec_to_mat(AVGAVG_horiuchiMaleAll20002010 , c(7,7))
dimnames(AVGAVG_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


AVGAVG_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecAVGMale2010,
                                            pars2=combined_vecAVGMale2019,
                                            N=20,
                                            dims = c(7,7))
AVGAVG_horiuchiMaleAll20102019mat <- vec_to_mat(AVGAVG_horiuchiMaleAll20102019 , c(7,7))
dimnames(AVGAVG_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



AVGAVG_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecAVGFemale2000,
                                              pars2=combined_vecAVGFemale2010,
                                              N=20,
                                              dims = c(7,7))
AVGAVG_horiuchiFemaleAll20002010mat <- vec_to_mat(AVGAVG_horiuchiFemaleAll20002010 , c(7,7))
dimnames(AVGAVG_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


AVGAVG_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecAVGFemale2010,
                                              pars2=combined_vecAVGFemale2019,
                                              N=20,
                                              dims = c(7,7))
AVGAVG_horiuchiFemaleAll20102019mat <- vec_to_mat(AVGAVG_horiuchiFemaleAll20102019 , c(7,7))
dimnames(AVGAVG_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



AVGnoMexAVGnoMex_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                                      pars1=combined_vecAVGnoMexMale2000,
                                                      pars2=combined_vecAVGnoMexMale2010,
                                                      N=20,
                                                      dims = c(7,7))
AVGnoMexAVGnoMex_horiuchiMaleAll20002010mat <- vec_to_mat(AVGnoMexAVGnoMex_horiuchiMaleAll20002010 , c(7,7))
dimnames(AVGnoMexAVGnoMex_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


AVGnoMexAVGnoMex_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                                      pars1=combined_vecAVGnoMexMale2010,
                                                      pars2=combined_vecAVGnoMexMale2019,
                                                      N=20,
                                                      dims = c(7,7))
AVGnoMexAVGnoMex_horiuchiMaleAll20102019mat <- vec_to_mat(AVGnoMexAVGnoMex_horiuchiMaleAll20102019 , c(7,7))
dimnames(AVGnoMexAVGnoMex_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



AVGnoMexAVGnoMex_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                                        pars1=combined_vecAVGnoMexFemale2000,
                                                        pars2=combined_vecAVGnoMexFemale2010,
                                                        N=20,
                                                        dims = c(7,7))
AVGnoMexAVGnoMex_horiuchiFemaleAll20002010mat <- vec_to_mat(AVGnoMexAVGnoMex_horiuchiFemaleAll20002010 , c(7,7))
dimnames(AVGnoMexAVGnoMex_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


AVGnoMexAVGnoMex_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                                        pars1=combined_vecAVGnoMexFemale2010,
                                                        pars2=combined_vecAVGnoMexFemale2019,
                                                        N=20,
                                                        dims = c(7,7))
AVGnoMexAVGnoMex_horiuchiFemaleAll20102019mat <- vec_to_mat(AVGnoMexAVGnoMex_horiuchiFemaleAll20102019 , c(7,7))
dimnames(AVGnoMexAVGnoMex_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



USAUSA_horiuchiMaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecUSAMale2000,
                                            pars2=combined_vecUSAMale2010,
                                            N=20,
                                            dims = c(7,7))
USAUSA_horiuchiMaleAll20002010mat <- vec_to_mat(USAUSA_horiuchiMaleAll20002010 , c(7,7))
dimnames(USAUSA_horiuchiMaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


USAUSA_horiuchiMaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                            pars1=combined_vecUSAMale2010,
                                            pars2=combined_vecUSAMale2019,
                                            N=20,
                                            dims = c(7,7))
USAUSA_horiuchiMaleAll20102019mat <- vec_to_mat(USAUSA_horiuchiMaleAll20102019 , c(7,7))
dimnames(USAUSA_horiuchiMaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))



USAUSA_horiuchiFemaleAll20002010  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecUSAFemale2000,
                                              pars2=combined_vecUSAFemale2010,
                                              N=20,
                                              dims = c(7,7))
USAUSA_horiuchiFemaleAll20002010mat <- vec_to_mat(USAUSA_horiuchiFemaleAll20002010 , c(7,7))
dimnames(USAUSA_horiuchiFemaleAll20002010mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))


USAUSA_horiuchiFemaleAll20102019  <- horiuchi(func=LTFLEDecomp,
                                              pars1=combined_vecUSAFemale2010,
                                              pars2=combined_vecUSAFemale2019,
                                              N=20,
                                              dims = c(7,7))
USAUSA_horiuchiFemaleAll20102019mat <- vec_to_mat(USAUSA_horiuchiFemaleAll20102019 , c(7,7))
dimnames(USAUSA_horiuchiFemaleAll20102019mat) <- list(c(seq(65,95,5)),c("Cardiovascular","Cancer","Metabolic","Respiratory","External","Nervous","Rest"))
# 

# 


#Decompositions. GAP  REP Cardiovascular mortality 08-19

# US decomps


fiveagevec <- c(seq(65,95,5))

# DF 2000 TO 2019 WITHIN COUNTRY


AllCountriesdecomp_res20002019df <- data.frame(Age=fiveagevec,
                                                ARGARG_horiuchiMaleAll20002010mat=ARGARG_horiuchiMaleAll20002010mat,
                                                ARGARG_horiuchiMaleAll20102019mat=ARGARG_horiuchiMaleAll20102019mat,
                                                ARGARG_horiuchiFemaleAll20002010mat=ARGARG_horiuchiFemaleAll20002010mat,
                                                ARGARG_horiuchiFemaleAll20102019mat=ARGARG_horiuchiFemaleAll20102019mat,
                                                BRABRA_horiuchiMaleAll20002010mat=BRABRA_horiuchiMaleAll20002010mat,
                                                BRABRA_horiuchiMaleAll20102019mat=BRABRA_horiuchiMaleAll20102019mat,
                                                BRABRA_horiuchiFemaleAll20002010mat=BRABRA_horiuchiFemaleAll20002010mat,
                                                BRABRA_horiuchiFemaleAll20102019mat=BRABRA_horiuchiFemaleAll20102019mat,
                                                CANCAN_horiuchiMaleAll20002010mat=CANCAN_horiuchiMaleAll20002010mat,
                                                CANCAN_horiuchiMaleAll20102019mat=CANCAN_horiuchiMaleAll20102019mat,
                                                CANCAN_horiuchiFemaleAll20002010mat=CANCAN_horiuchiFemaleAll20002010mat,
                                                CANCAN_horiuchiFemaleAll20102019mat=CANCAN_horiuchiFemaleAll20102019mat,
                                                CHICHI_horiuchiMaleAll20002010mat=CHICHI_horiuchiMaleAll20002010mat,
                                                CHICHI_horiuchiMaleAll20102019mat=CHICHI_horiuchiMaleAll20102019mat,
                                                CHICHI_horiuchiFemaleAll20002010mat=CHICHI_horiuchiFemaleAll20002010mat,
                                                CHICHI_horiuchiFemaleAll20102019mat=CHICHI_horiuchiFemaleAll20102019mat,
                                                COLCOL_horiuchiMaleAll20002010mat=COLCOL_horiuchiMaleAll20002010mat,
                                                COLCOL_horiuchiMaleAll20102019mat=COLCOL_horiuchiMaleAll20102019mat,
                                                COLCOL_horiuchiFemaleAll20002010mat=COLCOL_horiuchiFemaleAll20002010mat,
                                                COLCOL_horiuchiFemaleAll20102019mat=COLCOL_horiuchiFemaleAll20102019mat,
                                                CRICRI_horiuchiMaleAll20002010mat=CRICRI_horiuchiMaleAll20002010mat,
                                                CRICRI_horiuchiMaleAll20102019mat=CRICRI_horiuchiMaleAll20102019mat,
                                                CRICRI_horiuchiFemaleAll20002010mat=CRICRI_horiuchiFemaleAll20002010mat,
                                                CRICRI_horiuchiFemaleAll20102019mat=CRICRI_horiuchiFemaleAll20102019mat,
                                                MEXMEX_horiuchiMaleAll20002010mat=MEXMEX_horiuchiMaleAll20002010mat,
                                                MEXMEX_horiuchiMaleAll20102019mat=MEXMEX_horiuchiMaleAll20102019mat,
                                                MEXMEX_horiuchiFemaleAll20002010mat=MEXMEX_horiuchiFemaleAll20002010mat,
                                                MEXMEX_horiuchiFemaleAll20102019mat=MEXMEX_horiuchiFemaleAll20102019mat,
                                                PERPER_horiuchiMaleAll20002010mat=PERPER_horiuchiMaleAll20002010mat,
                                                PERPER_horiuchiMaleAll20102019mat=PERPER_horiuchiMaleAll20102019mat,
                                                PERPER_horiuchiFemaleAll20002010mat=PERPER_horiuchiFemaleAll20002010mat,
                                                PERPER_horiuchiFemaleAll20102019mat=PERPER_horiuchiFemaleAll20102019mat,
                                                USAUSA_horiuchiMaleAll20002010mat=USAUSA_horiuchiMaleAll20002010mat,
                                                USAUSA_horiuchiMaleAll20102019mat=USAUSA_horiuchiMaleAll20102019mat,
                                                USAUSA_horiuchiFemaleAll20002010mat=USAUSA_horiuchiFemaleAll20002010mat,
                                                USAUSA_horiuchiFemaleAll20102019mat=USAUSA_horiuchiFemaleAll20102019mat,
                                                AVGAVG_horiuchiMaleAll20002010mat=AVGAVG_horiuchiMaleAll20002010mat,
                                                AVGAVG_horiuchiMaleAll20102019mat=AVGAVG_horiuchiMaleAll20102019mat,
                                                AVGAVG_horiuchiFemaleAll20002010mat=AVGAVG_horiuchiFemaleAll20002010mat,
                                                AVGAVG_horiuchiFemaleAll20102019mat=AVGAVG_horiuchiFemaleAll20102019mat,
                                               AVGnoMexAVGnoMex_horiuchiMaleAll20002010mat=AVGnoMexAVGnoMex_horiuchiMaleAll20002010mat,
                                               AVGnoMexAVGnoMex_horiuchiMaleAll20102019mat=AVGnoMexAVGnoMex_horiuchiMaleAll20102019mat,
                                               AVGnoMexAVGnoMex_horiuchiFemaleAll20002010mat=AVGnoMexAVGnoMex_horiuchiFemaleAll20002010mat,
                                               AVGnoMexAVGnoMex_horiuchiFemaleAll20102019mat=AVGnoMexAVGnoMex_horiuchiFemaleAll20102019mat)

# # DF 2000 TO 2019 WITh US (so no USAUSA)

# 

AllCountriesdecomp_res20002019dflong  <- AllCountriesdecomp_res20002019df  %>%  
  group_by(Age) %>% 
  pivot_longer(c(2:309),
               names_to="Decomposition",
               values_to="Value")


# do it just for the main decomp between periods

CountryDecompVec2156 <- rep(c(rep("Argentina",28),
                              rep("Brazil",28),
                              rep("Canada",28),
                              rep("Chile",28),
                              rep("Colombia",28),
                              rep("Costa Rica",28),
                              rep("Mexico",28),
                              rep("Peru",28),
                              rep("U.S.",28),
                              rep("Latin America",28),
                              rep("Latin America w/o Mexico",28)), length.out=2156)


SexVec2156 <- rep(c(rep("Male",14),
                    rep("Female",14)), length.out=2156)


TypeVec2156 <- rep(c(rep("Cardiovascular",1),
                     rep("Cancer",1),
                     rep("Metabolic",1),
                     rep("Respiratory",1),
                     rep("External",1),
                     rep("Nervous",1),
                     rep("Rest",1)), length.out=2156)

PeriodVec2156 <- rep(c(rep("2000-2009",7),
                     rep("2010-2019",7)), length.out=2156)



# and the Rest countries

AllCountriesDecomp20002019dffull<- as.data.frame(cbind(AllCountriesdecomp_res20002019dflong,SexVec2156,TypeVec2156,CountryDecompVec2156,PeriodVec2156))

colnames(AllCountriesDecomp20002019dffull) <- c("Age","Decomposition","Value","Sex","Type","Country","Period")


# now summarize for analysis for all countries change over time (2000-10,2010-19)

AllCountriesDecomp20002019dffullsummary <- AllCountriesDecomp20002019dffull %>% 
  group_by(Sex,Country,Type,Period) %>% 
  summarize(Value=sum(Value)) %>% 
  mutate(Contribution = (Value / sum(Value)) * 100)

AllCountriesDecomp20002019dffullsummary <- AllCountriesDecomp20002019dffullsummary %>%
  mutate(Cause = paste(Type, Period, sep = ","))


# only cardio

AllCountriesDecomp20002019onlyCardio <- AllCountriesDecomp20002019dffullsummary 

AllCountriesDecomp20002019dffullsummary$Type[AllCountriesDecomp20002019dffullsummary$Type=="External"] <- "Rest"

AllCountriesDecomp20002019dffullsummary <- AllCountriesDecomp20002019dffull %>% 
  group_by(Sex,Country,Type,Period) %>% 
  summarize(Value=sum(Value)) %>% 
  mutate(Contribution = (Value / sum(Value)) * 100)


# Calculate the total sum of values for each period
total_sum20002019 <- AllCountriesDecomp20002019dffullsummary %>%
  group_by(Sex,Country,Period) %>%
  summarise(total_sum20002019 = sum(Value))

# Calculate the contribution as the relative sum of each value by type on the total sum of each period

RelContributionbyCause20002019 <- left_join(AllCountriesDecomp20002019dffullsummary,total_sum20002019,by=c("Sex","Country","Period"))

RelContributionbyCause20002019 <- RelContributionbyCause20002019 %>% 
  mutate(Contribution=Value/total_sum20002019)
RelContributionbyCause20002019 <- RelContributionbyCause20002019 %>%
  mutate(normalizedcontrib = ifelse(Contribution < -1, -1, ifelse(Contribution > 1, 1, Contribution)))

# 
# Now the Contribution column represents the proportion of each cause to the sum of the value for each period
# this is the main plot. we only focus in three main causes 


AllCountriesDecomp20002019df3causes <- AllCountriesDecomp20002019dffullsummary

AllCountriesDecomp20002019df3causes$Type[AllCountriesDecomp20002019df3causes$Type=="External"] <- "Other"
AllCountriesDecomp20002019df3causes$Type[AllCountriesDecomp20002019df3causes$Type=="Metabolic"] <- "Other"
AllCountriesDecomp20002019df3causes$Type[AllCountriesDecomp20002019df3causes$Type=="Respiratory"] <- "Other"
AllCountriesDecomp20002019df3causes$Type[AllCountriesDecomp20002019df3causes$Type=="Nervous"] <- "Other"
AllCountriesDecomp20002019df3causes$Type[AllCountriesDecomp20002019df3causes$Type=="Rest"] <- "Other"


AllCountriesDecomp20002019df3causes <- AllCountriesDecomp20002019df3causes %>% 
  group_by(Sex,Country,Type,Period,Value,Contribution) %>% 
  summarize(Value=sum(Value))

library(dplyr)

# to put Rest altogether i must do this, separating 

# Separate data for Cancer and Cardiovascular
cancer_Cardiovascular_data <- AllCountriesDecomp20002019df3causes %>%
  filter(Type %in% c("Cancer", "Cardiovascular"))

# Aggregate data for Other types
Other_data <- AllCountriesDecomp20002019df3causes %>%
  filter(Type == "Other") %>%
  group_by(Sex, Country, Type, Period) %>%
  summarize(
    Value = sum(Value),
    Contribution = mean(Contribution),  
    .groups = 'drop'
  )

# Combine the data back
AllCountriesDecomp20002019df3causes <- bind_rows(cancer_Cardiovascular_data, Other_data)

 

# Define the new color palette

# Define a custom color palette
color_palette <- c("Cancer" = "coral", "Cardiovascular" = "green", "Other" = "blue")

# Load RColorBrewer if you haven't already
library(RColorBrewer)

# Define a color palette using RColorBrewer's Dark2
color_palette <- brewer.pal(3, "Dark2")

# Assign names to the colors if needed
names(color_palette) <- c("Cancer", "Cardiovascular", "Other")



AllCountriesDecomp20002019df3causes$Sex <- factor(AllCountriesDecomp20002019df3causes$Sex,
                                                  levels = c("Male", "Female"))


PivotAllCountriesDecomp20002019df3causes <- AllCountriesDecomp20002019df3causes %>%
  select(c(1:5)) %>% 
  pivot_wider(names_from=Type,values_from=Value) %>% 
  mutate(Total=Cancer+Cardiovascular+Other,
         CardioRel=Cardiovascular/Total)
print(PivotAllCountriesDecomp20002019df3causes,n=44)

# 

# # # Other plot
# # 


AllCountriesDecomp20002019df3causes$Country <- factor(AllCountriesDecomp20002019df3causes$Country, 
                                                      levels = c( "Argentina", "Brazil","Chile","Colombia","Costa Rica","Peru","Latin America","Latin America w/o Mexico","Mexico", "Canada", "U.S."))
# #
 ggplot(AllCountriesDecomp20002019df3causes, aes(y = Country, x = Value, fill = Type)) +
 geom_bar(stat = "identity", position = "stack", size = 2) +
   geom_text(data = subset(AllCountriesDecomp20002019df3causes, Type == "Cancer"),
             aes(x = -1, y = Country, label = round(Value, 2)), size = 5, hjust = 1, color = "limegreen") +
   geom_text(data = subset(AllCountriesDecomp20002019df3causes, Type == "Cardiovascular"),
             aes(x = -1.5, y = Country, label = round(Value, 2)), size = 5, hjust = 1, color = "darkorange") +
   geom_text(data = subset(AllCountriesDecomp20002019df3causes, Type == "Other"),
             aes(x = -2, y = Country, label = round(Value, 2)), size = 5, hjust = 1, color = "steelblue") +
   theme_bw() +
   xlab("Contribution to LE65 change (years)") +
   ylab("Country") +
   geom_vline(xintercept = 0, linetype = "dotted", size = 1.5) +
   theme(
   axis.title.x = element_text(size = 18),
   axis.text.x = element_text(size = 18, angle = 90, vjust = 0.2),
   axis.title.y = element_text(size = 18),
   axis.text.y = element_text(size = 18),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.text = element_text(size = 18),
     legend.title = element_text(size = 18),
     strip.text = element_text(size = 18),
     strip.background = element_rect(fill = "lightgrey", color = "black", size = 1)
   ) +
   scale_fill_manual(values = color_palette) +
   scale_x_continuous(limits = c(-3, 3)) +
   facet_grid(Sex ~ Period)  # This will create a single column layout
 
# 
# 

AllCountriesDecomp20002019dffullsummarynotbycause <- AllCountriesDecomp20002019dffull %>% 
  group_by(Sex,Country,Type,Period) %>% 
  summarize(Value=sum(Value)) %>% 
  mutate(Contribution = (Value / sum(Value)) * 100)



ggplot(AllCountriesDecomp20002019dffull, aes(x = as.factor(Age), y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack", size = 2) +
  theme_bw()+
  xlab("Age") +
  ylab("Contribution to LE50 Gap (years)") +
  ggtitle("Contribution to LE50 gap in selected countries and US in 2008") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.2)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  ylim(-1,1)+
  facet_wrap(~Sex+Country,nrow=2)


AllCountriesDecomp20002019dffullsummarynotbycause <- AllCountriesDecomp20002019dffull %>% 
  group_by(Sex,Country,Type,Period) %>% 
  summarize(Value=sum(Value)) %>% 
  mutate(Contribution = (Value / sum(Value)) * 100)

ggplot(AllCountriesDecomp20002019dffull, aes(x = as.factor(Age), y = Value, fill = Type)) +
  geom_bar(stat = "identity", position = "stack", size = 2) +
  theme_bw()+
  xlab("Age") +
  ylab("Contribution to LE65 change (years)") +
  ggtitle("Contribution to LE65 growth in 2000-2019") +
  theme(axis.title.x = element_text(size = 14)) +
  theme(axis.text.x = element_text(size = 8, angle = 90, vjust = 0.2)) +
  theme(axis.title.y = element_text(size = 14)) +
  theme(axis.text.y = element_text(size = 12)) +
  ylim(-1,1)+
  facet_grid(Sex~Country)