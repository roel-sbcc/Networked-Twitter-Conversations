
# R-scripts to retrieve and analyze networked Twitter conversations

The code chuncks below comprise a set of R-scripts to…

1.  Retrieve and process tweets and associated ego networks
2.  Analyze the ego networks (community detection and centrality
    measures)
3.  Mine profile descriptions and tweets and transform them into word
    clouds

The code chuncks outline the retrieval and analysis process and call
functions defined in `helpers.r` and `rate_limit.r` in the helpers
folder.

## 1\. Retrieval of Tweets

The Twitter API comes with rate limitations. We have written scripts to
stay within these rate limitations. Retrieving large sets of data
therefore requires a lot of time.

### 1.1 R setup

First, we load the required packages and Twitter-token.

``` r
## Loading libraries
require(rtweet)
require(tidyverse)
require(stringr)
require(readr)
require(httpuv)
require(lubridate)
require(igraph)
require(pattern.nlp)
require(wordcloud)
require(tm)
require(SnowballC)
require(tidytext)
require(wordcloud2)
require(longurl)
require(rvest)
require(webshot)
require(htmltools)
require(htmlwidgets)
require(imager)
require(kableExtra)
require(rmdformats)

## Thousands seperator
knitr::knit_hooks$set(inline = function(x) { if(!is.numeric(x)){ x }else{ prettyNum(round(x,2), big.mark=",") } })

## Loading twitter token and scripts
twitter_token <- read_rds(paste(path.expand("~/"),"twitter_token.rds",sep="")) ## load your twitter token here; for more info, consult the rtweet documentation

## Loading scripts
source("helpers/helpers.r")
source("helpers/rate_limit.r")
```

### 1.2 Tweet retrieval

Next, we retrieve the tweets meeting our query, also including all
associated original tweets, retweets, quotes, and replies.

``` r
## Define the query to be sent to Twitter. 
  
  ## More info: https://dev.twitter.com/rest/public/search
  query <- "'vaccinatie' OR 'vaccineren' OR 'vaccinaties' OR 'inenten' OR 'inenting' OR 'inentingen'"
  
  ## Retrieve tweets
  tweets_raw <- search_tweets2(query,
                              n=18000, ## Returns min 100 tweets, max 18000. n is multiple of 100. Rounds up (n = 210 results in 300 tweets)
                              lang="nl",
                              type="recent", ## "popular",
                              include_rts=TRUE,
                              parse=TRUE,
                              retryonratelimit = TRUE) %>% ## If n > 18000, set to TRUE, otherwise FALSE
    mutate(original=TRUE) ## specifically add this to initial query to distinguish tweets and associated accounts resulting from this query
  
  ### Process the data in the desired format
  tweets_processed <- process_tweets(tweets_raw)
  
  ### Retrieve the retweeted and quoted originals before retrieving all the user data
  rts_quotes_processed <- retrieve_rts_quotes(tweets_processed[[1]])
  rts_quotes_processed[[1]] <- rts_quotes_processed[[1]] %>%
    mutate(original=ifelse(status_id %in% tweets_processed[[1]]$status_id,TRUE,FALSE)) %>%
    unique
  
  ### Retrieve the statuses that replies are replying to, and continue to follow these chains until first tweet is reached
  conversations_processed <- retrieve_conversations(tweets_processed[[1]])
  conversations_processed[[1]] <- conversations_processed[[1]] %>%
    mutate(original=ifelse(status_id %in% tweets_processed[[1]]$status_id,TRUE,FALSE)) %>%
    unique
  
  ## Retrieving retweet and quote originals as well as conversations results in doubles. The code below addresses this.
  
  ### Mark rts or quotes that were part of a conversation as such
  rts_quotes_processed[[1]] <- rts_quotes_processed[[1]] %>% 
    select(-conversation) %>%
    left_join(.,conversations_processed[[1]] %>% 
                select(status_id,conversation),by=c("status_id"="status_id"))
  
  ### Remove rts and quotes that were part of a conversation from the conversation set
  conversations_processed[[1]] <- conversations_processed[[1]] %>%
    filter(!status_id %in% (rts_quotes_processed[[1]] %>% 
                              select(status_id,conversation) %>% 
                              filter(!is.na(conversation)) %>% 
                              .$status_id))
  
  ### Removing tweets from the original set that are part of replies, rts or quotes
  tweets_processed[[1]] <- tweets_processed[[1]] %>% 
    filter(!status_id %in% rts_quotes_processed[[1]]$status_id) %>%
    filter(!status_id %in% conversations_processed[[1]]$status_id) %>%
    mutate(conversation=NA,retweet_user_id=NA,quote_user_id=NA)
  
  ## Finishing up the data
  tweets_complete <- rbind(tweets_processed[[1]],rts_quotes_processed[[1]],conversations_processed[[1]]) %>% 
    unique
  
  ### Checking for doubles; doubles are and have ambiguous characteristics, such as 
  ### being part of two conversations or being a quote as well as a normal tweet
  double <- tweets_complete %>% 
    group_by(status_id) %>% 
    dplyr::summarize(count=n()) %>% 
    filter(count>1) %>% 
    .$status_id
  
  ### Isolating the non-double tweets as basis for conversations table
  without_double <- tweets_complete %>%
    filter(!status_id %in% double) %>%
    distinct %>%
    select(status_id,conversation)
  
  ### Only the double cases, without conversation column (cleaning its characteristics)
  double_cases_without_conv <- tweets_complete %>%
    filter(status_id %in% double) %>%
    select(-conversation) %>%
    distinct
  
  ## Retrieves the conversation number for the double cases
  clean_doubles <- double_cases_without_conv %>% 
    group_by(status_id) %>%
    do(conversation=retrieve_con(.$status_id)) %>%
    unnest
  
  ## Create tweets data frame
  tweets_complete <- tweets_complete %>% select(-conversation) %>% distinct
      ## %>% filter(!status_id %in% double) in case of repeated errors, delete ambiguous cases
  
  ## Create conversation data frame (conversation is not stored as a tweet attribute, but in a seperate table)
  conversations <- clean_doubles %>% 
    rbind(.,without_double) %>%
    unique %>%
    filter(!is.na(conversation))
  
  ## Creates mentions data frame (including the mentions in the rts, quotes, and tweets-and-reply-chains.)
  mentions_users <- rbind(tweets_processed[[2]],rts_quotes_processed[[2]],conversations_processed[[2]]) %>% 
    unique
```

### 1.3 Retrieve connections and process network file

We retrieve all the connections of the authors of our retrieved tweets.

#### 1.3.1 Retrieving networks

First we retrieve the connections with accounts that are following our
author. Next, we retrieve the conections with accounts that our authors
are
following.

``` r
user_stats <- retrieve_user_stats(tweets_complete$user_id %>% unique %>% as.character) %>% as.data.frame
  
  ## Preprocess data
  users_followers <- user_stats %>%
    select(user_id,followers) %>%
    filter(followers>0) %>%
    unique %>%
    mutate(calls_needed=((followers/5000)%>%ceiling))
  
  ## Retrieve followers
  
  if(nrow(users_followers>0)) {
    
    ## Smaller accounts to retrieve in one time frame
    smaller_accounts <- users_followers %>% 
      filter(followers<=75000)
    
    ## Big accounts to retrieve in more time frames
    are_there_big_accounts <- ifelse(nrow(users_followers %>%filter(followers>75000))!=0,TRUE,FALSE)
    
    ## Initiating results df
    followers <- NULL
    
    ## Loops through given data frame row by row
    for(i in 1:nrow(smaller_accounts)) {
      
      ## Subset batch, announcing batch start
      batch <- smaller_accounts[i,]
      calls_needed <- batch$calls_needed
      
      ## Limit check
      limit_check(batch$calls_needed,TRUE,"followers/ids")
      
      print(paste("###### Retrieving user",i,"of",nrow(smaller_accounts)))
      
      ## Retrieve followers. Returning NA when no results given
      tmp <- batch %>%
        group_by(user_id) %>%  ##=user_id) %>%
        do(
          data.frame(follower=(pos_get_followers(.$user_id,calls_needed)))) %>%
        as.data.frame
      
      tmp$follower <- tmp$follower %>% as.character
      
      followers <- rbind(followers,tmp)
      
    } ## einde retrieval small accounts
    
    if(are_there_big_accounts==TRUE) {
      
      ## create set
      big_accounts <- users_followers %>% 
        filter(followers>75000)
      
      ## Loops through given data frame row by row
      for(i in 1:nrow(big_accounts)) {
        
        ## Subset batch, announcing batch start
        batch <- big_accounts[i,]
        
        ## Retrieve followers. Returning NA when no results given
        tmp <- batch %>%
          group_by(user_id) %>%
          do(
            data.frame(follower=get_followers_for_large_account(.$user_id,.$calls_needed))) %>%
          as.data.frame
        
        tmp$follower <- tmp$follower %>% as.character
        
        followers <- rbind(followers,tmp)
        
      } ## for loop big_accounts
      
    } ## einde retrieval large 
    
    followers$user_id <- followers$user_id %>% as.character
    followers$follower <- followers$follower %>% as.character
    
  } ## einde retrieval followers

  ## Retrieve following
  
  ## Preprocess data
  users_following <- user_stats %>%
    select(user_id,following) %>%
    filter(following>0) %>%
    unique %>%
    mutate(calls_needed=((following/5000)%>%ceiling))
  
  if(nrow(users_following>0)) {
    
    ## Smaller accounts to retrieve in one time frame
    smaller_accounts <- users_following %>% 
      filter(following<=75000)
    
    ## Big accounts to retrieve in more time frames
    are_there_big_accounts <- ifelse(nrow(users_following %>%filter(following>75000))!=0,TRUE,FALSE)
    
    ## Initiating results df
    following <- NULL
    
    ## Loops through given data frame row by row
    for(i in 1:nrow(smaller_accounts)) {
      
      ## Subset batch, announcing batch start
      batch <- smaller_accounts[i,]
      calls_needed <- batch$calls_needed
      
      ## Limit check
      limit_check(batch$calls_needed,TRUE,"friends/ids")
      
      print(paste("###### Retrieving user",i,"of",nrow(smaller_accounts)))
      
      ## Retrieve followers. Returning NA when no results given
      tmp <- batch %>%
        group_by(user_id) %>%  ##=user_id) %>%
        do(
          data.frame(following=(pos_get_following(.$user_id,calls_needed)))) %>%
        as.data.frame
      
      tmp$following <- tmp$following %>% as.character
      
      following <- rbind(following,tmp)
      
    } ## einde retrieval small accounts
    
    if(are_there_big_accounts==TRUE) {
      
      ## create set
      big_accounts <- users_following %>% 
        filter(following>75000)
      
      ## Loops through given data frame row by row
      for(i in 1:nrow(big_accounts)) {
        
        ## Subset batch, announcing batch start
        batch <- big_accounts[i,]
        
        ## Retrieve followers. Returning NA when no results given
        tmp <- batch %>%
          group_by(user_id) %>%
          do(
            data.frame(following=get_following_for_large_account(.$user_id,.$calls_needed))) %>%
          as.data.frame
        
        tmp$following <- tmp$following %>% as.character
        
        following <- rbind(following,tmp)
        
      } ## einde for loop big_accounts
      
    } ## einde retrieval large 
    
    following$user_id <- following$user_id %>% as.character
    following$following <- following$following %>% as.character
    
  } ## einde retrieval following
```

#### 1.3.2 Creating network file

We filter the network to only include connections with users that are
connected with multiple of our authors. Thereby, the network represents
shared following- and follower-patterns. The cut-off point is determined
by inspecting the distribution of connections and aiming for a network
size that is computable by our technical
setup.

``` r
  # Simply creating a network file from the retrieved data will most likely not work,
  # as the file will be too large to visualize or compute. Therefore, we will inspect
  # how often each account is connected (following, followed, or both) with an author 
  # which is an account that is present in tweets_complete.
  
  ## Cleaning colnames
  colnames(followers) <- c("source","target")
  colnames(following) <- c("target","source")
  
  ## Calculate author connections...
  
  ### ...for user_id_x_follower
  followers_count <- followers %>%
    .$target %>%
    table %>%
    as.data.frame
  
  ### ...for user_id_x_following
  following_count <- following %>%
    .$source %>%
    table %>%
    as.data.frame
  
  ### ...and combining both
  connections_with_authors <- data.frame(user_id=c(followers_count$. %>% as.character,following_count$. %>% as.character),
             count=c(followers_count$Freq,following_count$Freq)) %>%
    group_by(user_id) %>%
    summarize(count=sum(count))
  
  ## Cleaning RAM
  rm(followers_count,following_count)
  
  ## Dealing with factors
  connections_with_authors$user_id <- connections_with_authors$user_id %>% as.character
  
  ## Inspecting the distribution to determine the cut-off point
  
  ### Summary
  summ <- connections_with_authors$count %>% summary
  
  ### Distribution
  dist <- connections_with_authors %>%
      group_by(count) %>%
      summarize(occurrence=n()) %>%
      mutate(percentage=occurrence/sum(occurrence)*100) %>% arrange(desc(percentage))
  
  print(summ)
  plot(dist)
  
  #### Based on this distribution above, we decide to retain accounts with 15 authors or more
  
  ## Filtering the data
  
  ### Creating a vector of the author id's
  author_ids <- tweets_complete$user_id %>% unique
  
  ### Filtering followers
  followers_flt <- followers %>% 
    filter(target %in% c(author_ids,connections_with_authors %>% filter(count>=15) %>% .$user_id %>% as.character)) %>%
    select(source,target)
  
  ### Filtering following
  following_flt <- following %>% 
    filter(source %in% c(author_ids,(connections_with_authors %>% filter(count>=15) %>% .$user_id %>% as.character))) %>%
    select(source,target)
  
  ## Clearing RAM
  rm(followers,following)
  
  ## Creating edgelist
  
  ## Retrieve mentions for weight
  mentions <- mentions_users %>% 
    group_by(user_id,mentions_user_id) %>% 
    summarize(weight=n()) %>% 
    ungroup %>% 
    mutate(weight=dense_rank(weight)+1)
  
  ## Create edgelist including weight column
  edgelist_data_frame <- rbind(data.frame(source=followers_flt$source %>% as.character,
                                          target=followers_flt$target %>% as.character),
                               data.frame(source=following_flt$source %>% as.character,
                                          target=following_flt$target %>% as.character)) %>%
    distinct %>%
    left_join(.,mentions,by=c("source"="user_id","target"="mentions_user_id")) %>%
    mutate(weight=ifelse(is.na(weight),1,weight))
  
  ## Clearing RAM
  rm(followers_flt,following_flt)
  
}
## retrieve user_stats for all nodes in the network and create the initial nodelist

  ## Retrieve user_stats
  user_data_to_retrieve <- c(edgelist_data_frame$source,edgelist_data_frame$target) %>% unique %>% as.character
  user_stats <- retrieve_user_stats(user_data_to_retrieve)
  
  # Create nodelist
  nodelist_data_frame <- data.frame(Id=user_stats$user_id %>% as.character,
                                  Label=user_stats$screen_name %>% as.character,
                                  real_name=user_stats$real_name %>% as.character,
                                  description=user_stats$description %>% as.character,
                                  location=user_stats$location %>% as.character,
                                  language=user_stats$language %>% as.character,
                                  followers=user_stats$followers,
                                  following=user_stats$following) %>%
  as.data.frame %>%
  group_by(Id) %>%
  mutate(statuses=get_statuses(Id) %>% as.integer) %>% ## gets the ammount of statuses for every node
  as.data.frame
  
  ## Workaround for R making factors from characters
  nodelist_data_frame$Id <- nodelist_data_frame$Id %>% as.character
  nodelist_data_frame$Label <- nodelist_data_frame$Label %>% as.character 
  nodelist_data_frame$real_name <- nodelist_data_frame$real_name %>% as.character 
  nodelist_data_frame$description <- nodelist_data_frame$description %>% as.character
  nodelist_data_frame$location <- nodelist_data_frame$location %>% as.character 
  nodelist_data_frame$language <- nodelist_data_frame$language %>% as.character 
  
  user_ids <- c(edgelist_data_frame$source,edgelist_data_frame$target) %>% unique
  
  ## Creating NA's for user_ids in the edgelist that could not be retrieved (deleted?)
  na <- data.frame(Id=c(user_ids[!user_ids %in% nodelist_data_frame$Id])) %>%
    mutate(Label=NA,
           real_name=NA,
           description=NA,
           location=NA,
           language=NA,
           followers=NA,
           following=NA,
           statuses=NA,
           original=NA) %>%
    as.data.frame
  
  ## Combining the data
  nodelist_data_frame <- rbind(nodelist_data_frame,na)
  
  ## Determine links to other platforms
  associated_urls <- nodelist_data_frame %>% 
    select(user_id=Id,screen_name=Label,description) %>%
    urls_from_description_and_link_field %>% 
    filter(url_filtered!="character(0)")
  
  ## Detect wether node counts as influencer
  influencers <- detect_influencer(associated_urls)
  
  ## Add data to nodelist
  nodelist_data_frame <- nodelist_data_frame %>%
    left_join(.,influencers,by=c("Id"="user_id"))
```

## 2\. Data analysis

After retrieveal of the tweets, we detect communities and perform text
mining by community.

### 2.1 Community detection and influence computation

We perform community detection on the network and compute influence
statistics for every
user.

``` r
## First, we make sure the same user_ids are in both the nodelist and edgelist
  Ids_in_nodelist <- nodelist_data_frame %>% filter(!is.na(Label)) %>% .$Id %>% unique  ## All ids in nodelist 
  Ids_in_edgelist <- c(edgelist_data_frame$source,edgelist_data_frame$target) %>% unique ## all ids in edgelist
  Edge_ids_not_in_nodelist <- Ids_in_edgelist[!Ids_in_edgelist %in% Ids_in_nodelist] ## Ids in edgelist not in nodelist
  Node_ids_not_in_edgelist <- Ids_in_nodelist[!Ids_in_nodelist %in% Ids_in_edgelist] ## Ids in de nodelist not in de edgelist
  
  ## Filter the nodelist and edgelist
  edge_tmp <- edgelist_data_frame %>%
    filter(!target %in% Edge_ids_not_in_nodelist, !source %in% Edge_ids_not_in_nodelist)
  node_tmp <- nodelist_data_frame[!nodelist_data_frame$Id %in% Node_ids_not_in_edgelist,]
  
  ## Deals with doubles caused by multi influencers
  doubles <- node_tmp %>%
    group_by(Id) %>%
    summarize(count=n()) %>%
    filter(count>1) %>%
    .$Id
  
  node_influencer_is_multi <- node_tmp %>%
    filter(Id %in% doubles) %>%
    mutate(influencer="Multi") %>%
    distinct
  
  node_tmp <- node_tmp %>%
    filter(!Id %in% doubles) %>%
    rbind(.,node_influencer_is_multi) %>%
    distinct
  
  ## create graph
  graph <- graph_from_data_frame(edge_tmp,directed=TRUE,vertices=(node_tmp))
  
  ## Cluster louvain
  louvain <- cluster_louvain(as.undirected(graph))
  
  ## Calc modularity score
  modularity <- modularity(as.undirected(graph), membership(louvain))
  
  ## create df for louvain
  Ids <- membership(louvain) %>% names
  Communities <- membership(louvain) %>% as.numeric
  membership <- data.frame(Id=Ids,Community=Communities)
  
  ## Betweenness
  betweenness <- betweenness(graph,directed=TRUE)
  Ids_btw <- betweenness %>% names
  btw <- betweenness %>% as.numeric
  betweenness <- data.frame(Id=Ids_btw,betweenness=btw)
  
  ## PageRank
  pagerank <- page_rank(graph,directed=TRUE)
  Ids_pr <- pagerank$vector %>% names
  pr <- pagerank$vector %>% as.numeric
  pagerank <- data.frame(Id=Ids_pr,pagerank=pr)

  ## update nodelist
  nodelist_data_frame <- node_tmp %>%
    left_join(.,membership,by=c("Id"="Id")) %>%
    left_join(.,pagerank,by=c("Id"="Id")) %>% 
    left_join(.,betweenness,by=c("Id"="Id"))
  
  ## compute stats (inline insnt legible)
  no_o_nodes <- ifelse(exists("graph"),
                       V(graph)%>%length,
                       nodelist_data_frame %>% nrow)
  no_o_edges <- ifelse(exists("graph"),
                       E(graph)%>%length,
                       edgelist_data_frame %>% nrow)
```

### 2.2 Text mining

We analyze the profile descriptions and tweets by community. Note: the
scripts below are aimed at processing Dutch language and requires extra
packages. See the code for
details.

``` r
  ## First, we define the communities we want to compare the profile texts and tweets by
  ## We choose to retain the communities that make up more than 1 per cent of the total network
  
  keepers <- nodelist_data_frame %>% 
    group_by(Community) %>% 
    summarize(count=n()) %>% 
    ungroup %>% 
    mutate(perc=count/sum(count)) %>% 
    arrange(desc(perc )) %>%
    filter(perc>0.01) %>%
    .$Community

  ## joining data to analyze tweets
  tweets_for_analysis <- tweets_complete %>%
    left_join(.,nodelist_data_frame %>% select(Id,Community),by=c("user_id"="Id")) %>%
    filter(Community %in% keepers)
    
  ## The code below loads four lists of stopwords, combines them, and retains words that occur twice or more.
  
  ### Stop words
  stopwords_dutch_1 <- read_html("http://www.damienvanholten.com/blog/dutch-stop-words/") %>% 
    html_nodes("[id='word_list'] li") %>% 
    html_text %>%
    as.data.frame
  colnames(stopwords_dutch_1) <- "word"
  
  stopwords_dutch_2 <- read_delim("https://raw.githubusercontent.com/stopwords-iso/stopwords-nl/master/stopwords-nl.txt",delim="/n",col_names = F)
  colnames(stopwords_dutch_2) <- "word"
  
  stopwords_dutch_3 <- read_delim("https://sites.google.com/site/kevinbouge/stopwords-lists/stopwords_nl.txt",delim="/n",col_names = F) 
  colnames(stopwords_dutch_3) <- "word"
  
  stopwords_CLiPS <- read_delim("https://raw.githubusercontent.com/clips/pattern/master/pattern/vector/stopwords-nl.txt",delim="/n",col_names=F) %>%
    paste %>% 
    str_split(",") %>% 
    unlist %>% 
    as.data.frame
  colnames(stopwords_CLiPS) <- "word"
  
  ### Creating final list to include words occurring 2 or more times
  stopwords <- rbind(stopwords_dutch_1,stopwords_dutch_2,stopwords_dutch_3,stopwords_CLiPS) %>% 
    group_by(word) %>%
    summarize(count=n()) %>%
    filter(count>=2) %>%
    .$word %>%
    as.character
  
  ## Adding Twitter noise such as screen_names RT to the list of stopwords
  stopwords_and_noise <- data.frame(word=c(stopwords,"rt","RT","PT","pt","twitter","tweets","tweeten","tweet",tweets_complete$screen_name,stopwords("en"))
                                    %>% as.character %>% 
                                      unique)
  
  # CLEANING THE CORPUS

  ## Cleaning corpus
  corpus_description <- nodelist_data_frame %>% 
    select(Id,Label,description,Community,original,statuses) %>%
    distinct %>%
    unnest_tokens(.,word,description,token="words") %>%
    anti_join(stopwords_and_noise) %>% ## stopwords
    filter(!str_detect(word,"[0-9]")) %>% ## getallen
    filter(!str_detect(word,"http|www|.nl|.com|t.co")) %>% ## url noise
    filter(!str_detect(word,"^[A-z]$|^[A-z][A-z]$")) ## one and two character words
  
  ## Cleaning corpus
  corpus_tweets <- tweets_for_analysis %>% 
    filter(is_retweet==FALSE, is_quote==FALSE) %>%
    select(status_id,user_id,text,Community) %>%
    distinct %>%
    unnest_tokens(.,word,text,token="words") %>%
    anti_join(stopwords_and_noise) %>% ## stopwords
    filter(!str_detect(word,"[0-9]")) %>% ## getallen
    filter(!str_detect(word,"http|www|.nl|.com|t.co")) %>% ## url noise
    filter(!str_detect(word,"^[A-z]$|^[A-z][A-z]$")) ## one and two character words
  
  ## POS mapping
  ## This step stems the Dutch words and determines what kind of word it is for easier filtering
  ### Note that this uses the pattern.nlp function, that requires Python and the MBSP-package to run on the machine that executes these scripts
  ### More info: https://www.clips.uantwerpen.be/pages/mbsp-tags
  corpus_pos_description <- corpus_description %>%
    select(word) %>%
    unique %>%
    group_by(word) %>%
    do(pattern_pos(.$word,language="dutch"))
  
  corpus_description <- corpus_description %>%
    left_join(.,corpus_pos_description %>% select(word,word.type,word.lemma),by=c("word"="word"))
  
  ## Re-using words that were retrieved already
  ## Making a list of words to retrieve in order to re-use words that were retrieved already
  corpus_pos_tweets_to_retrieve <- corpus_tweets %>%
    left_join(.,corpus_pos_description %>% select(word,word.type,word.lemma),by=c("word"="word")) %>%
    filter(is.na(word.type))
  
  corpus_pos_tweets <- corpus_pos_tweets_to_retrieve %>%
    select(word) %>%
    unique %>%
    group_by(word) %>%
    do(pattern_pos(.$word,language="dutch")) %>%
    rbind(.,corpus_pos_description)
  
  corpus_tweets <- corpus_tweets %>%
    left_join(.,corpus_pos_tweets %>% select(word,word.type,word.lemma),by=c("word"="word"))
  
  ## TEXT MINING
  
  ## Most occurring words by community
  wordcount_description <- corpus_description %>%
    filter(Community %in% keepers) %>%
    group_by(Community,word.lemma) %>% 
    summarize(count=n()) %>% 
    arrange(desc(count))
  
  ## Most occurring words by community
  tf_idf_description <- corpus_description %>%
    filter(Community %in% keepers) %>%
    group_by(Community,word.lemma) %>% 
    summarize(count=n()) %>%
    filter(count!=1) %>%
    ungroup %>%
    bind_tf_idf(word.lemma,Community,count) %>%
    arrange(Community,desc(tf_idf))
  
  ## Most occurring words by community
  wordcount_tweets <- corpus_tweets %>%
    filter(Community %in% keepers) %>%
    group_by(Community,word.lemma) %>% 
    summarize(count=n()) %>% 
    arrange(desc(count))
  
  ## Most occurring words by community
  tf_idf_tweets <- corpus_tweets %>%
    filter(Community %in% keepers) %>%
    group_by(Community,word.lemma) %>% 
    summarize(count=n()) %>%
    filter(count!=1) %>%
    ungroup %>%
    bind_tf_idf(word.lemma,Community,count) %>%
    arrange(Community,desc(tf_idf))
  
  ## computes the wordcloud data
  
  wordclouds_description <- data.frame(community=c(keepers)) %>%
    group_by(community) %>%
    do(create_wordcloud(wordcount_description,tf_idf_description,.$community)) %>%
    ungroup %>%
    select(-community)
  
  wordclouds_tweets <- data.frame(community=c(keepers)) %>%
    group_by(community) %>%
    do(create_wordcloud(wordcount_tweets,tf_idf_tweets,.$community)) %>%
    ungroup %>%
    select(-community)
```

### 2.3 Wordclouds by community

We process wordclouds, representing semantic patterns in the profile
texts and tweets for every community. These wordclouds can guide
researchers in subsequent qualitative
analyses.

``` r
## Creates a df with community labels, size, wordclouds (description and tweets), and file names of word clouds
plots <- data.frame(Community=keepers) %>%
    group_by(Community) %>%
    do(Label=create_label(.$Community),
       Size=retrieve_size(.$Community),
       Authors=retrieve_authors(.$Community),
       Tweets_n=retrieve_tweets(.$Community),
       Retweets_quotes=retrieve_retweets_quotes(.$Community),
       Reply=retrieve_reply(.$Community)) %>%
    unnest %>%
    ungroup %>%
    mutate(Size_perc=(Size/sum(Size) * 100)%>% round(1)) %>%
    mutate(Authors_perc=(Authors/sum(Authors) * 100)%>% round(1)) %>%
    mutate(Tweets_perc=(Tweets_n/sum(Tweets_n) * 100)%>% round(1)) %>%
    mutate(Retweets_quotes_perc=(Retweets_quotes/sum(Retweets_quotes) * 100)%>% round(1)) %>%
    mutate(Reply_perc=(Reply/sum(Reply) * 100)%>% round(1)) %>%
    group_by(Community,Label,Size,Size_perc,Authors,Authors_perc,Tweets_n,Tweets_perc,Retweets_quotes,Retweets_quotes_perc,Reply,Reply_perc) %>%
    do(Description=plot_cloud(wordclouds_description,.$Community),
       Tweets=plot_cloud(wordclouds_tweets,.$Community),
       file_tw=NA,
       file_ds=NA) %>%
    arrange(desc(Size))

  ## Wordclouds are htmlwidgets by default. We use webshot and imager to flatten and crop these images
  for(i in 1:length(plots$Community)) {
    
    ## Save widgets as html files
    htmlwidgets::saveWidget(plots$Tweets[[i]],"tw.html",selfcontained = F)  
    htmlwidgets::saveWidget(plots$Description[[i]],"ds.html",selfcontained = F)
   
    ## Make webshots of the html files
    tw_raw <- webshot::webshot("tw.html","figures/tw_raw.png",
                     vwidth = 1600, vheight = 1200, delay = 2)
    ds_raw <- webshot::webshot("ds.html","figures/ds_raw.png",
                     vwidth = 1600, vheight = 1200, delay = 2)
    
    ## Load, crop and save images
    load.image("figures/tw_raw.png") %>%
      crop.borders(nx=12,ny=12) %>%
      autocrop(axes = "xy") %>%
      save.image(paste0("figures/tw_",plots$Label[i],".png",collapse=""),quality=0.9)
    
    load.image("figures/ds_raw.png") %>%
      crop.borders(nx=12,ny=12) %>%
      autocrop(axes = "xy") %>%
      save.image(paste0("figures/ds_",plots$Label[i],".png",collapse=""),quality=0.9)
    
    # Write the filenames to the df with plots
    plots$file_tw[i] <- paste0("figures/tw_",plots$Label[i],".png",collapse="")
    plots$file_ds[i] <- paste0("figures/ds_",plots$Label[i],".png",collapse="")
    
    ## Delete temporary files
    file.remove(c("tw.html","ds.html","figures/tw_raw.png","figures/ds_raw.png"))
    unlink(c("tw_files","ds_files"),recursive = TRUE)
    
  }
```
