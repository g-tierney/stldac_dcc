
#pull into data frame
methods <- c("vb","cgibbs","lda","clda","btm")
results <- tibble()

for(nD in nDperU.values){
  for(nUC in nUC.values){
    if(!file.exists(str_c("output/vb-gibbs-comp-sims/nUC",nUC,"_nDU",nD,"_seed3.rds"))) next
    
    sim_results <- readRDS(str_c("output/vb-gibbs-comp-sims/nUC",nUC,"_nDU",nD,"_seed3.rds"))
    for(method in methods){
      method.result <- sim_results[[method]]
      
      #collect nmi for topics and clusters 
      if(method == "vb"){
        nmi.topics <- method.result$phi_mat %>% apply(1,which.max) %>% table(sim_results$dat$ta_true) %>% nmi
        nmi.clusters <- method.result$lambda_mat %>% apply(1,which.max) %>% table(sim_results$dat$ca_true) %>% nmi
      } else if(method == "cgibbs"){
        nmi.topics <- method.result$ta %>% results_freq_table() %>% apply(1,which.max) %>% table(sim_results$dat$ta_true) %>% nmi
        nmi.clusters <- method.result$ca %>% results_freq_table() %>% apply(1,which.max) %>% table(sim_results$dat$ca_true) %>% nmi
      } else if(method == "lda"){
        pred.topic.dists <- posterior(method.result)$topics
        pred.cluster <- sapply(sim_results$dat$users %>% unique,
                               function(u) colMeans(pred.topic.dists[u == sim_results$dat$users,])) %>% t %>% 
          cluster::pam(k = 3) %>% {.$clustering}
        
        nmi.topics <- topics(method.result) %>% table(sim_results$dat$ta_true) %>% nmi
        nmi.clusters <- pred.cluster %>% table(sim_results$dat$ca_true) %>% nmi
      } else if(method == "clda") {
        pred.usertopic.dists <- posterior(method.result)$topics
        pred.cluster <- cluster::pam(pred.usertopic.dists,k = 3)$clustering
        
        nmi.topics <- posterior(method.result,newdata = sim_results$dat$dw)$topics %>% apply(1,which.max) %>% table(sim_results$dat$ta_true) %>% nmi
        nmi.clusters <- pred.cluster %>% table(sim_results$dat$ca_true) %>% nmi
      } else if(method == "btm"){
        btm.data <- apply(sim_results$dat$dw,1,function(d) rep(1:ncol(sim_results$dat$dw),d) %>% str_c(collapse = " ")) %>% 
        {data.frame(doc_id = 1:length(.),text=.,stringsAsFactors = F)} %>% 
          tidytext::unnest_tokens(output=token,input=text)
        
        pred.topic.dists <- predict(method.result,btm.data)
        pred.cluster <- sapply(sim_results$dat$users %>% unique,
                               function(u) colMeans(pred.topic.dists[u == sim_results$dat$users,])) %>% t %>% 
          cluster::pam(k = 3) %>% {.$clustering}
        
        nmi.topics <- pred.topic.dists %>% apply(1,which.max) %>% table(sim_results$dat$ta_true) %>% nmi
        nmi.clusters <- pred.cluster %>% table(sim_results$dat$ca_true) %>% nmi
      } else if(method == "stlda_noc"){
        nmi.topics <- method.result$ta %>% results_freq_table() %>% apply(1,which.max) %>% table(sim_results$dat$ta_true) %>% nmi
        nmi.clusters <- method.result$ut %>% results_array_mean() %>% 
          cluster::pam(k = 3) %>% {.$clustering} %>% table(sim_results$dat$ca_true) %>% nmi
      } else{
        nmi.topics <- NA
        nmi.clusters <- NA
      }
      
      #add to results table
      results.row <- tibble(method = method,nD = nD,nUC = nUC,nmi.topics = nmi.topics,nmi.clusters = nmi.clusters)
      results <- bind_rows(results,results.row)
    }
  }
}
#write_csv(results,file = "output/vb-gibbs-comp-sims/results_seed3.csv")

results_wider <- results %>% pivot_wider(names_from = "method",values_from = starts_with("nmi"),id_cols = c("nD","nUC"))

results <- bind_rows(read_csv("output/vb-gibbs-comp-sims/results_seed2.csv") %>% mutate(seed = 2),
                     read_csv("output/vb-gibbs-comp-sims/results_seed3.csv") %>% mutate(seed = 3)) %>% 
  group_by(method,nD,nUC) %>% 
  summarise(nmi.topics = mean(nmi.topics),)
results %>% 
  group_by(method,nD,nUC) %>% 
  summarise(across(starts_with("nmi"),mean)) %>% 
  ggplot(aes(x = nD %>% as.factor,y = nUC %>% as.factor)) + 
  geom_tile(aes(fill = nmi.clusters)) + 
  labs(x = "Documents per User",y = "Users per Cluster",fill = "Cluster Recovery\n(NMI)") + 
  theme(legend.position = "bottom") + scale_fill_gradient(limits = c(0,1)) + 
  facet_wrap(~ method)
ggsave("output/vb-gibbs-comp-sims/cluster_nmi.png")

results %>% 
  group_by(method,nD,nUC) %>% 
  summarise(across(starts_with("nmi"),mean)) %>% 
  ggplot(aes(x = nD %>% as.factor,y = nUC %>% as.factor)) + 
  geom_tile(aes(fill = nmi.topics)) + 
  labs(x = "Documents per User",y = "Users per Cluster",fill = "Topic Recovery\n(NMI)") + 
  theme(legend.position = "bottom") + scale_fill_gradient(limits = c(0,1)) + 
  labs(x = "Documents per User",y = "Users per Cluster") + 
  facet_wrap(~ method)
ggsave("output/vb-gibbs-comp-sims/topic_nmi.png")
