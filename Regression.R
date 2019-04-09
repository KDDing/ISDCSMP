W_mp2 <- function (SDC, SDS, SDCC, SDSS, SDGD) {
  VSDC <- as.vector(SDC)
  VSDS <- as.vector(SDS)
  VSDCC <- as.vector(SDCC)
  VSDSS <- as.vector(SDSS)
  VSDGD <- as.vector(SDGD)
  rigdgeLm <- lm.ridge(VSDC~VSDS+VSDCC+VSDSS+VSDGD,lambda = 0.1)
  output <- rigdgeLm$coef
  return (output)
}

W_mp3 <- function (SDC, SDS, SDCC, SDSS, SDGD, SDCCC, SDSSS, SDGGD, SDCSC, SDSCS) {
  VSDC <- as.vector(SDC)
  VSDS <- as.vector(SDS)
  VSDCC <- as.vector(SDCC)
  VSDSS <- as.vector(SDSS)
  VSDGD <- as.vector(SDGD)
  VSDCCC <- as.vector(SDCCC)
  VSDSSS <- as.vector(SDSSS)
  VSDGGD <- as.vector(SDGGD)
  VSDCSC <- as.vector(SDCSC)
  VSDSCS <- as.vector(SDSCS)
  rigdgeLm <- lm.ridge(VSDC~VSDS+VSDCC+VSDSS+VSDGD+VSDCCC+VSDSSS+VSDGGD+VSDCSC+VSDSCS,lambda = 0.1)
  output <- rigdgeLm$coef
  return (output)
}



W_mp4 <- function (SDC, SDS, SDCC, SDSS, SDGD, SDCCC, SDSSS, SDGGD, SDCSC, SDSCS, SDCCCC, SDSSSS,SDGDGD,SDGGGD,SDCGDC,SDSGDS,SDCSSC,SDSCCS) {
  VSDC <- as.vector(SDC)
  VSDS <- as.vector(SDS)
  VSDCC <- as.vector(SDCC)
  VSDSS <- as.vector(SDSS)
  VSDGD <- as.vector(SDGD)
  VSDCCC <- as.vector(SDCCC)
  VSDSSS <- as.vector(SDSSS)
  VSDGGD <- as.vector(SDGGD)
  VSDCSC <- as.vector(SDCSC)
  VSDSCS <- as.vector(SDSCS)
  VSDCCCC <- as.vector(SDCCCC)
  VSDSSSS <- as.vector(SDSSSS)
  VSDGDGD <- as.vector(SDGDGD)
  VSDGGGD <- as.vector(SDGGGD)
  VSDCGDC <- as.vector(SDCGDC)
  VSDSGDS <- as.vector(SDSGDS)
  VSDCSSC <- as.vector(SDCSSC)
  VSDSCCS <- as.vector(SDSCCS)
  
  rigdgeLm <- lm.ridge(VSDC~VSDS+VSDCC+VSDSS+VSDGD+VSDCCC+VSDSSS+VSDGGD+VSDCSC+VSDSCS+VSDCCCC+VSDSSSS+VSDGDGD+VSDGGGD+VSDCGDC+VSDSGDS+VSDCSSC+VSDSCCS,lambda = 0.1)
  output <- rigdgeLm$coef
  return (output)
}

W_lasso3 <- function (SDC, SDS, SDCC, SDSS, SDGD, SDCCC, SDSSS, SDGGD, SDCSC, SDSCS) {
  VSDC <- as.vector(SDC)
  VSDS <- as.vector(SDS)
  VSDCC <- as.vector(SDCC)
  VSDSS <- as.vector(SDSS)
  VSDGD <- as.vector(SDGD)
  VSDCCC <- as.vector(SDCCC)
  VSDSSS <- as.vector(SDSSS)
  VSDGGD <- as.vector(SDGGD)
  VSDCSC <- as.vector(SDCSC)
  VSDSCS <- as.vector(SDSCS)
  shuju.x <- cbind(VSDS,VSDCC,VSDSS,VSDGD,VSDCCC,VSDSSS,VSDGGD,VSDCSC,VSDSCS)
  la<-seq(0,1,0.05)
  #shuju.lasso <- glmnet(shuju.x,VSDC,alpha=1,lambda=la)
  shuju.lasso <- glmnet(shuju.x,VSDC,alpha=1,lambda=0)
  output <- shuju.lasso$coef
  return (output)
}


adjust_w <- function(w_mp) {
  for (i in 1:dim(w_mp)[1]) {
    if (w_mp[i,1] < 0) {
      w_mp[i,1] <- 0
    }
  }
  return (w_mp)
}
