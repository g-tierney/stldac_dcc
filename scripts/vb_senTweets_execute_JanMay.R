rm(list=ls())

print(parallel::detectCores())

library(tidyverse)
library(quanteda)
library(stopwords)

source("scripts/setup.R")
source("scripts/helper_functions.R")
source("scripts/vb_stldac_functions.R")
source("scripts/vb_implementation.R")

load('senatorTweet_data/rinputs.Rdata')
dw_mat <- convert(senTweets116_2020.dfm_trimmed,to="matrix")
senators <- senTweets116_2020.dfm_trimmed@docvars$screen_name

#seed set for creating a small dataset for testing (x from each senator)
set.seed(196)
small_n <- senTweets116_2020.dfm_trimmed@docvars %>% 
  group_by(screen_name) %>% 
  sample_n(pmin(4,n())) %>% 
  select(docname_)

#switch n values to choose sample or full data
n <- 1:nrow(dw_mat) # which(senTweets116_2020.dfm_trimmed@docvars$docname_ %in% small_n$docname_) #
dw_mat <- dw_mat[n,]
senators <- senators[n]
nCores <- min(round(parallel::detectCores()/1),length(unique(senators))/4)

print(str_c(length(n)," Tweets"))
print(str_c("Using ",nCores," cores."))

rm(senTweets116_2020.dfm_trimmed,small_n)

#set parameters
nC_vec <- c(2,4,6,8)
nT_vec <- 50 #c(20,30,40,50)

param_mat <- expand.grid(nC_vec,nT_vec)

#get slurm id 
if(Sys.getenv('SLURM_ARRAY_TASK_ID') != ""){
  slurm_id <- as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))
} else slurm_id <- 1

nC <- param_mat[slurm_id,1]; nT <- param_mat[slurm_id,2];
maxiter <- 1000;  seed <- 196
print(str_c("Using ",nC," clusters and ",nT," topics"))

x <- stldac_vb(users=senators,dw=dw_mat,nT = nT,nC = nC,tol = .01,seed = seed,maxiter = maxiter,n.cores=nCores)
gc()

saveRDS(x,file = str_c("/work/gt83/stldac_dcc/output/vb_",nC,"C_",nT,"T_",maxiter,"Max_",nCores,"Core_",seed,"seed_JanMay.rds"))
