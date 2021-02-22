
library(topicmodels)
library(BTM)
library(tidytext)
library(tidyverse)
#require(quanteda)

# see tutorial here: https://tutorials.quanteda.io/machine-learning/topicmodel/

source("scripts/setup.R")
source("scripts/helper_functions.R")
source("scripts/gibbs_functions.R")
source("scripts/vb_stldac_functions.R")
source("scripts/vb_implementation.R")


sim_vb_gibs_comp <- function(nUC.sim,nDperU.sim,seed.sim = 1){
  
  #make data
  alphag <- matrix(c(1,1,1,1,1,1,1,1,1,1,
                     1,1,1,1,1,0,0,0,0,0,
                     0,0,0,0,0,1,1,1,1,1,
                     0,1,1,1,1,0,0,0,0,0
  ),ncol = 10,byrow = T)
  dat <- make_data(nW = 4000,nT = 10,topic_low = 1,topic_high = 10,nC = 3,alpha_true = alphag,seed = seed.sim,
                   nUC = nUC.sim,nDperU = nDperU.sim)
  
  #run full gibbs
  fullg <- NULL
  # fullg <- full_gibbs_1topic(alpha = 1,eta = .1,nu = 1,
  #                            users = dat$users,dw = dat$dw,
  #                            nT = dat$nT,nC = dat$nC,
  #                            niter = 500,nClusterIter = 50,
  #                            seed = seed.sim,
  #                            mu_scale = 0,sigma_scale = 100)
  
  #run VB and get output
  vb_output.list <- lapply(1:5,function(s) stldac_vb(alpha_start = 00,users = dat$users,dw = dat$dw,nT = dat$nT,nC = dat$nC,maxiter = 300,seed = s))
  vb_output1 <- lapply(vb_output.list,function(l) l$log_like) %>% which.max %>% vb_output.list[[.]]
  
  #run collapsed gibbs
  groundtruth_estimate <- collapsed_gibbs_1topic_clusters(alpha = 1,eta = .1,nu = 1,
                                                          users = dat$users,dw = dat$dw,
                                                          nT = dat$nT,nC = dat$nC,
                                                          niter = 500,
                                                          seed = seed.sim,mcmc_update = T,
                                                          nClusterIter = 50,
                                                          mu_scale = 0,sigma_scale = 100,
                                                          prop_scale_center = 100,alphag_sample_method = "componentwise",
                                                          print_clusters = T)
  

  
  #stLDA without clustering
  stlda_noc <- NULL
  # stlda_noc <- collapsed_gibbs_1topic(alpha = 1,eta = .1,
  #                                     users = dat$users,dw = dat$dw,
  #                                     nT = dat$nT,
  #                                     niter = 500,
  #                                     seed = seed.sim)
  
  #adding LDA, cLDA, and BTM is pretty cheap I guess
  lda_results <- LDA(x = dat$dw,method = "VEM",k = 10,control = list(seed = seed.sim))
  
  dat$dw_colapsed <- sapply(1:length(unique(dat$users)), function(u) colSums(dat$dw[dat$users==u,])) %>% t
  clda_results <- LDA(x = dat$dw_colapsed,method = "VEM",k = 10,control = list(seed = seed.sim))
  
  btm.data <- apply(dat$dw,1,function(d) rep(1:ncol(dat$dw),d) %>% str_c(collapse = " ")) %>% 
    {data.frame(doc_id = 1:length(.),text=.,stringsAsFactors = F)} %>% 
    tidytext::unnest_tokens(output=token,input=text)
  
  btm_results <- BTM::BTM(data = btm.data,k = 10)
  
  return(list(dat=dat,vb = vb_output1,vb_list = vb_output.list,cgibbs = groundtruth_estimate,fgibbs = fullg,
              stlda_noc = stlda_noc,
              lda = lda_results,clda = clda_results,btm = btm_results))
}



#sim_vb_gibs_comp(nUC.sim = 2,nDperU.sim = 5)

nUC.values <- c(5,10,20,50)
nDperU.values <- c(50,100,200,500)
seeds <- 196:200

params <- expand.grid(nUC.values=nUC.values[1:2],
                      nDperU.values=nDperU.values[1],
                      seeds=seeds[1]) %>% as_tibble %>% 
  arrange(-nUC.values*nDperU.values) %>% 
  #filter(row_number()>=5) %>% 
  identity() 

# array verrsion
start <- Sys.time()

slurm_id <- as.integer(Sys.getenv('SLURM_ARRAY_TASK_ID'))

seed <- params$seeds[slurm_id]
nUC <- params$nUC.values[slurm_id]
nD <- params$nDperU.values[slurm_id]

set.seed(slurm_id)
sim_results <- sim_vb_gibs_comp(nUC.sim = nUC,nDperU.sim = nD,seed.sim = seed)
saveRDS(sim_results,str_c("output/vb-gibbs-comp-sims/nUC",nUC,"_nDU",nD,"_seed",seed,".rds"))

end <- Sys.time()
end - start
