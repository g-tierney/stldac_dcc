
library(tidyverse)

# see tutorial here: https://tutorials.quanteda.io/machine-learning/topicmodel/

source("scripts/setup.R")
source("scripts/helper_functions.R")
source("scripts/gibbs_functions.R")

seed.sim <- 515

#make data
alphag <- matrix(c(1,1,1,1,1,1,1,1,1,1,
                   1,1,1,1,1,0,0,0,0,0,
                   0,0,0,0,0,1,1,1,1,1,
                   0,1,1,1,1,0,0,0,0,0
),ncol = 10,byrow = T)
dat <- make_data(nW = 4000,nT = 10,topic_low = 1,topic_high = 10,nC = 3,alpha_true = alphag,seed = seed.sim,
                 nUC = 10,nDperU = 100)

#run full gibbs
fgibbs <- full_gibbs_1topic(alpha = 1,eta = .1,nu = 1,
                            users = dat$users,dw = dat$dw,
                            nT = dat$nT,nC = dat$nC,
                            niter = 2000,nClusterIter = 50,
                            seed = seed.sim,
                            mu_scale = 0,sigma_scale = 100)
#run collapsed gibbs
cgibbs <- collapsed_gibbs_1topic_clusters(alpha = 1,eta = .1,nu = 1,
                                          users = dat$users,dw = dat$dw,
                                          nT = dat$nT,nC = dat$nC,
                                          niter = 5,
                                          seed = seed.sim,mcmc_update = T,
                                          nClusterIter = 500,
                                          mu_scale = 0,sigma_scale = 100,
                                          prop_scale_center = 100,alphag_sample_method = "componentwise",
                                          print_clusters = T)

print("full gibbs topic recovery")
fgibbs$ta %>% results_freq_table() %>% table(dat$ta_true)
fgibbs$ta %>% results_freq_table() %>% table(dat$ta_true) %>% nmi

print("collapsed gibbs topic recovery")
cgibbs$ta %>% results_freq_table() %>% table(dat$ta_true)
cgibbs$ta %>% results_freq_table() %>% table(dat$ta_true) %>% nmi



if(!dir.exists("output/cgibbs_fgibbs_comparison")) dir.create("output/cgibbs_fgibbs_comparison")

saveRDS(list(fgibbs = fgibbs,cgibbs = cgibbs),file = "output/cgibbs_fgibbs_comparison/gibbs_results.rds")