rm(list=ls())

setwd("E://research//doc//Papers//drug combinations//ISDCSMP//ISDCSMP")
#source("T_LEARN.R")
source("CrossValidation.R")
source("PathSim.R")
source("Regression.R")
library("caTools")
library(MASS)
library(glmnet)

setwd("E://research//doc//Papers//drug combinations//ISDCSMP//drug_in")
RDDC <- read.table("DD_MAT.txt")
RDDS1 <- read.table("DD_sim_ATC1.txt")
RDDS2 <- read.table("DD_sim_ATC2.txt")
RDDS3 <- read.table("DD_sim_ATC3.txt")
RDG <- read.table("DG_MAT.txt")
RGG <- read.table("GG_MAT.txt")

DDC_mat <- as.matrix(RDDC)
DDS1_mat <- as.matrix(RDDS1)
DDS2_mat <- as.matrix(RDDS2)
DDS3_mat <- as.matrix(RDDS3)
DG_mat <- as.matrix(RDG)
GG_mat <- as.matrix(RGG)

DDS_mat <- (DDS1_mat + DDS2_mat + DDS3_mat)/3

DD_mat <- DDC_mat
DD_mat_O <- DD_mat

GD_mat <- t(DG_mat)
    
DDC_mat <- DD_mat
DDSC <- DDC_mat %*% DDS_mat
    
#¸Ä³Édata.frame´æ´¢
DC <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '1')
DS <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '2')
DCC <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '11')
DSS <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '22')
DGD <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '33')
DCCC <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '111')
DSSS <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '222')
DGGD <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '343')
DCSC <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '121')
DSCS <- PathCal(DDC_mat, DDS_mat, DG_mat, GG_mat, DDJ_mat, '212')

    
SDC <- PathSim(DC)
SDS <- PathSim(DS)
SDCC <- PathSim(DCC)
SDSS <- PathSim(DSS)
SDGD <- PathSim(DGD)
SDCCC <- PathSim(DCCC)
SDSSS <- PathSim(DSSS)
SDGGD <- PathSim(DGGD)
SDCSC <- PathSim(DCSC)
SDSCS <- PathSim(DSCS)
    
w_m <- W_mp3(SDC,SDS,SDCC,SDSS,SDGD,SDCCC,SDSSS,SDGGD,SDCSC,SDSCS)
w_m <- data.frame(w_m)
w_m <- adjust_w(w_m)
NS3W <- SDC+w_m[1,1]*SDS+w_m[2,1]*SDCC+w_m[3,1]*SDSS+w_m[4,1]*SDGD+w_m[5,1]*SDCCC+w_m[6,1]*SDSSS+w_m[7,1]*SDGGD+w_m[8,1]*SDCSC+w_m[9,1]*SDSCS 
NS <- NS3W
    
data_ROC_n <- Get_Test_Score_List(NS,DD_mat,DD_mat_O)
    
    