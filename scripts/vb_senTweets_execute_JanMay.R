rm(list=ls())

print(parallel::detectCores())

library(tidyverse)
library(quanteda)
library(stopwords)

source("scripts/setup.R")
source("scripts/helper_functions.R")
source("scripts/vb_stldac_functions.R")
source("scripts/vb_implementation.R")

load('senatorTweet_data/senatorTweet_dfms.Rdata')
senTweets_trimmed.dfm <- senTweets_trimmed.dfm %>% 
  dfm_subset(created_at < "2020-05-31")
dw_mat <- convert(senTweets_trimmed.dfm,to="matrix")
senators <- senTweets_trimmed.dfm@docvars$screen_name

#seed set for creating a small dataset for testing (x from each senator)
set.seed(196)
small_n <- senTweets_trimmed.dfm@docvars %>% 
  group_by(screen_name) %>% 
  sample_n(pmin(4,n())) %>% 
  select(docname_)

#switch n values to choose sample or full data
n <-   1:nrow(dw_mat) # which(senTweets_trimmed.dfm@docvars$docname_ %in% small_n$docname_) #
dw_mat <- dw_mat[n,]
senators <- senators[n]
nCores <- min(round(parallel::detectCores()/1),length(unique(senators))/4)

print(str_c(length(n)," Tweets"))
print(str_c("Using ",nCores," cores."))

#rm(senTweets116_2020.dfm_trimmed,small_n)

#set parameters
nC_vec <- c(2,4,6,8)
nT_vec <- c(20,30,40,60)

param_mat <- expand.grid(nC_vec,nT_vec)

#get slurm id 
if(Sys.getenv('SLURM_ARRAY_TASK_ID') != ""){
  slurm_id <- as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))
} else slurm_id <- 1

nC <- param_mat[slurm_id,1]; nT <- param_mat[slurm_id,2];
maxiter <- 1000;  seed <- 196

x <- stldac_vb(users=senators,dw=dw_mat,nT = nT,nC = nC,tol = .01,seed = seed,maxiter = 1000,n.cores=nCores)
gc()

saveRDS(x,file = str_c("output/vb_",nC,"C_",nT,"T_",maxiter,"Max_",nCores,"Core_",seed,"seed_JanMay.rds"))
