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


set.seed(196)
small_n <- senTweets116_2020.dfm_trimmed@docvars %>% 
  group_by(screen_name) %>% 
  sample_n(4) %>% 
  select(docname_)

n <-   1:nrow(dw_mat) # which(senTweets116_2020.dfm_trimmed@docvars$docname_ %in% small_n$docname_) #
dw_mat <- dw_mat[n,]
senators <- senators[n]
nCores <- min(4,round(parallel::detectCores()/1),length(unique(senators)))

print(str_c(length(n)," Tweets"))
print(str_c("Using ",nCores," cores."))

#rm(senTweets116_2020.dfm_trimmed)

x <- stldac_vb(users=senators,dw=dw_mat,nT = 10,nC = 4,tol = .01,seed = 1,maxiter = 2,n.cores=nCores)

gc()

#saveRDS(x,file = "senatorTweet_data/vb_4C_30T_200Max_MaxCore.rds")
