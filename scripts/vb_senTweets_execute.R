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
nCores <- min(1,round(parallel::detectCores()/1),length(unique(senators)))

print(str_c(length(n)," Tweets"))
print(str_c("Using ",nCores," cores."))

#rm(senTweets116_2020.dfm_trimmed,small_n)

#set parameters
maxiter <- 1000; nC <- 4; nT <- 30; seed <- 196

x <- stldac_vb(users=senators,dw=dw_mat,nT = nT,nC = nC,tol = .01,seed = seed,maxiter = 1000,n.cores=nCores)
gc()

saveRDS(x,file = str_c("output/vb_",nC,"C_",nT,"T_",maxiter,"Max_",nCores,"Core_",seed,"seed.rds"))
