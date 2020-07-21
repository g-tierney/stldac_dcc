rm(list=ls())

print(parallel::detectCores())

library(tidyverse)
library(quanteda)
library(stopwords)

source("scripts/setup.R")
source("scripts/helper_functions.R")
source("scripts/gibbs_functions.R")


load('senatorTweet_data/rinputs.Rdata')
dw_mat <- convert(senTweets116_2020.dfm_trimmed,to="matrix")

if(!dir.exists("output")){
  dir.create("output")
}

if(!dir.exists("temp")){
  dir.create("temp")
}


set.seed(196)
small_n <- senTweets116_2020.dfm_trimmed@docvars %>% 
  group_by(screen_name) %>% 
  sample_n(4) %>% 
  select(docname_)

n <- 1:nrow(dw_mat) #which(senTweets116_2020.dfm_trimmed@docvars$docname_ %in% small_n$docname_) # 
dw_mat <- dw_mat[n,]
senators <- senators[n]

print(str_c(length(n)," Tweets"))

rm(senTweets116_2020.dfm_trimmed,small_n)


x <- collapsed_gibbs_1topic_clusters(alpha = 1,eta = .1,nu = 1,
                                      users = senators,dw = dw_mat,
                                      nT = 10,nC = 4,
                                      niter = 1000,
                                      seed = 196,mcmc_update = T,
                                      nalphag_steps = 10,
                                      mu_scale = 0,sigma_scale = 100,
                                      prop_scale_center = 100,alphag_sample_method = "componentwise",
                                      print_clusters = T,
                                      partial_output = T)

saveRDS(x,"output/gibbs_4C_10T_mu0_sigma100_componentwise.rds")
gc()
