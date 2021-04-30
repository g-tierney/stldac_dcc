rm(list=ls())

print(parallel::detectCores())

library(tidyverse)
library(quanteda)
library(stopwords)

source("scripts/setup.R")
source("scripts/helper_functions.R")
source("scripts/vb_stldac_functions.R")
source("scripts/vb_implementation.R")

load("temp/dw_train.RData")

if(Sys.getenv('SLURM_ARRAY_TASK_ID') != ""){
  slurm_id <- as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))
} else slurm_id <- 1

#set parameters
maxiter <- 1000; nC <- 2*slurm_id; nT <- 30; seed <- 196
nCores <- min(1,round(parallel::detectCores()/1),length(unique(senators)))

x <- stldac_vb(users=senators[train],dw=dw_mat[train,],nT = nT,nC = nC,tol = .01,seed = seed,maxiter = 2,n.cores=nCores)
gc()

saveRDS(x,file = str_c("output/vb_",nC,"C_",nT,"T_",maxiter,"Max_",nCores,"Core_",seed,"seed_train.rds"))
