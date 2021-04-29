
library(tidyverse)
library(quanteda)
library(stopwords)

senTweets116.full <- rtweet::read_twitter_csv("senatorTweet_data/senators116_3200RecentTweets_2020-06-01.csv") 
senTweets116 <- senTweets116.full %>% 
  group_by(screen_name,status_id) %>% #added after run, comment to return to original documents 
  filter(row_number() == 1) %>% 
  mutate(date = as.Date(created_at))
senDemos <- openxlsx::read.xlsx("senatorTweet_data/116_Senator_Twitter_Handles.xlsx") %>% 
  filter(!str_detect(senator_name,"Isakson"))

# senTweets116 %>% 
#   filter(date >= as.Date("2020-01-01") & date <= as.Date("2020-05-31"),!str_detect(screen_name,"isakson")) %>% 
#   group_by(screen_name) %>% 
#   summarise(n=n(),
#             first=min(date),
#             last=max(date)) %>% View

senTweets116_2020 <- senTweets116 %>% 
  filter(date >= as.Date("2020-01-01") & date <= as.Date("2020-05-31"),!str_detect(screen_name,"isakson")) 

senTweets116_2020$text_clean <- 
  str_replace_all(senTweets116_2020$text,"https?://([a-z\\.]*)/([:alnum:]*)","hyperlink") #remove URLs


senTweets116_2020.dfm <- dfm(senTweets116_2020$text_clean,tolower = T,remove = c(stopwords(source = "smart"),"hyperlink","amp"),remove_punct = T)
docvars(senTweets116_2020.dfm) <- senTweets116_2020 %>% ungroup()

senTweets116_2020.dfm %>% colSums() %>% sort(decreasing = T) %>% {.[1:10]}

senTweets116_2020.dfm_trimmed <- senTweets116_2020.dfm %>% dfm_trim(min_docfreq = 0.001,docfreq_type = "prop") %>% 
{dfm_subset(.,subset = ntoken(.)>1)} %>% 
  dfm_trim(min_docfreq = 1,docfreq_type = "count")

senTweets116_2020.dfm_trimmed@docvars %>% group_by(screen_name) %>% summarise(n=n()) %>% arrange(n)
senTweets116_2020.dfm_trimmed

###############################
### Now convert to a matrix ###
###############################

dw_mat <- senTweets116_2020.dfm_trimmed %>% 
  convert(to="matrix")
senators <- senTweets_trimmed.dfm@docvars$screen_name

#seed set for creating a small dataset for testing (x from each senator)
set.seed(196)
small_n <- senTweets_trimmed.dfm@docvars %>% 
  group_by(screen_name) %>% 
  sample_n(pmin(4,n())) %>% 
  select(docname_)
set.seed(NULL)

#switch n values to choose small sample or full data
n <-   1:nrow(dw_mat) # which(senTweets_trimmed.dfm@docvars$docname_ %in% small_n$docname_) #
dw_mat <- dw_mat[n,]
senators <- senators[n]

print(str_c(length(n)," Tweets"))
print(str_c("Using ",nCores," cores."))

#rm(senTweets116_2020.dfm_trimmed,small_n)

#extract test set
set.seed(196)
test <- senTweets_trimmed.dfm@docvars %>% 
  group_by(screen_name) %>% 
  slice_sample(prop = 0.1) %>% 
  {senTweets_trimmed.dfm@docvars$docname_ %in% .$docname_} %>% 
  which
train <- c(1:nrow(dw_mat))[!(1:nrow(dw_mat)) %in% test]
set.seed(NULL)

dw_train <- dw_mat[train,]
dw_test <- dw_mat[test,]

save(dw_mat,test,train,senators,file = "temp/dw_train.RData")
