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
rm(senTweets116_2020.dfm_trimmed)

n <- nrow(dw_mat) #9000 
dw_mat <- dw_mat[1:n,]
senators <- senators[1:n]
nCores <- min(1,parallel::detectCores(),length(unique(senators)))

print(n)
print(nCores)

x <- stldac_vb(users=senators,dw=dw_mat,nT = 10,nC = 4,tol = .01,seed = 1,maxiter = 2,n.cores=nCores)
saveRDS(x,file = "senatorTweet_data/vb_4C_30T_test.rds")
