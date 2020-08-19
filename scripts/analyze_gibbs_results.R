
source("scripts/setup.R")
source("scripts/helper_functions.R")
#new comment

results <- readRDS("output/gibbs_4C_10T_mu0_sigma100_componentwise.rds")
load("senatorTweet_data/rinputs.Rdata")

names(results)

cluster_assignments <- results$ca %>% results_freq_table() %>% apply(1,which.max) 
topic_assignments <- results$ta %>% results_freq_table() %>% apply(1,which.max)

top_topic_words(results$tw,results$words,n=10) %>% t


senatorDemos <- senTweets116_2020.dfm_trimmed@docvars %>% select(screen_name,party,state) %>% unique()
senatorDemos$cluster <- cluster_assignments
senatorDemos %>% arrange(cluster)
table(cluster_assignments,senatorDemos$party)

senTweets116_2020.dfm_trimmed@docvars$topic <- topic_assignments
