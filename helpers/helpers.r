# TWEET RETRIEVAL

## Process raw tweets
process_tweets <- function(df) {
  
  ## Tags tweets that were not retrieved as a result of the initial query as non-original
  if(!"original" %in% colnames(df)) {
    df <- df %>%
      mutate(original=FALSE)
  }
  
  print("Processing tweets and mentions...")
  
  ## Processes raw tweets
  tweets <- df %>%
    mutate(hasLink=str_detect(text,"http"), ## whether a tweet contains links
           isReply=ifelse(!is.na(.$reply_to_status_id),TRUE,FALSE)) %>% ## wether the tweet is a reply
    group_by(status_id=(status_id %>% as.character)) %>%
    do(
      data.frame(
        original=.$original, ## wether tweet is original
        screen_name=(.$screen_name %>% as.character), ## user
        user_id=(.$user_id %>% as.character),
        text_original=(.$text %>% as.character),
        text=(.$text %>% 
                str_replace(.,"^RT ","") %>% 
                str_replace(.,"http.* ","") %>% 
                str_replace(.,"^@.*@[a-zA-Z0-9_.-]+ ","") %>%
                str_replace(.," @.*@[a-zA-Z0-9_.-]+","") %>%
                str_replace(.,"^@[a-zA-Z0-9_.-]+: ","") %>%
                str_replace(.,"^@[a-zA-Z0-9_.-]+ ","") %>%
                str_replace(.," @[a-zA-Z0-9_.-]+","") %>%
                str_replace(.,"<.*>","")),
        created=(.$created_at %>% 
                   strptime(format="%Y-%m-%d %H:%M:%S", tz=Sys.timezone(location=TRUE))),
        is_retweet=.$is_retweet, ## whether the tweet is a retweet
        retweet_status_id=(.$retweet_status_id %>% as.character),
        is_quote=.$is_quote,
        quote_status_id=(.$quoted_status_id %>% as.character),
        is_reply=.$isReply,
        reply_status_id=(.$reply_to_status_id %>% as.character),
        reply_user_id=(.$reply_to_user_id %>% as.character),
        retweet_count=(.$retweet_count %>% as.integer),
        favorite_count=(.$favorite_count %>% as.integer),
        platform=(.$source %>% as.character),
        has_link=(ifelse(!is.na(.$urls_expanded_url),T,F)),
        has_media=ifelse(!is.na(.$media_url),T,F),
        has_symbols=(ifelse(!is.na(.$symbols),T,F)),
        has_hashtags=(ifelse(!is.na(.$hashtags),T,F)),
        country=(.$country_code %>% as.character)
      )) %>% 
    as.data.frame() %>% 
    unique
  
  ## Processes user mentions (later used for edge weight)
  mentioned_users <- df %>%
    group_by(status_id,user_id) %>% 
    do(mentions_user_id=(.$mentions_user_id %>% unlist %>% as.character)) %>% 
    unnest %>%
    filter(!is.na(mentions_user_id))  
  
  
  ## Combining results in a list
  exp <- list(tweets,mentioned_users)
  names(exp) <- c("tweets","mentioned_users")
  return(exp)
  
}

## Retrieve retweeted and quoted originals
retrieve_rts_quotes <- function(df) {
  
  # Retweets
  
  ## Check if there are retweets to retrieve
  rts <- df %>%
    filter(!is.na(retweet_status_id)) 
  
  ## Initiating results
  retweet_user_ids <- NULL
  
  ## Retrieve retweets, if any.
  if(nrow(rts)>0) {
    
    print("Retrieving originals for retweets...")
    
    ## Retrieve all retweets
    rts <- rts %>%
      .$retweet_status_id %>%
      unique %>%
      lookup_statuses %>% 
      process_tweets  ## alle retweets ophalen
    
    ## Filter observations already in source file
    rts[[1]] <- rts[[1]] %>% 
      dplyr::filter(!status_id %in% df$status_id)
    rts[[2]] <- rts[[2]] %>% 
      filter(status_id %in% rts[[1]]$status_id)
    
    print(paste0(nrow(rts[[1]])," retweeted originals retrieved"))
    
    ## Retrieves user_id's for rts in order to retrieve the original tweets
    retweets_user_ids <- df %>% 
      filter(is_retweet==TRUE) %>% 
      group_by(retweet_status_id) %>% 
      do(data.frame(retweet_user_id=retrieve_user_id(rts[[1]],.$retweet_status_id)))
  }
  
  ## Return NA when there are no retweeted originals to retrieve
  if(is.null(retweets_user_ids)) {
    
    print("No originals for retweets to retrieve")
    
    tweets <- tweets %>% mutate(retweet_user_id=NA)
  }
  
  # Quotes
  
  ## Check if there are quotes to retrieve
  quotes <- df %>% 
    filter(!is.na(quote_status_id))
  
  ## Initiating results
  quotes_user_ids <- NULL
  
  ## Retrieve quotes, if any
  if(nrow(quotes)>0) {
    
    print("Retrieving originals for quotes...")
      
    quotes <- quotes %>%
      .$quote_status_id %>%
      unique %>%
      lookup_statuses %>% 
      process_tweets ## retrieve all quotes  
    
    ## Filter observations already in source file
    quotes[[1]] <- quotes[[1]] %>% 
      dplyr::filter(!status_id %in% df$status_id)
    quotes[[2]] <- quotes[[2]] %>% filter(status_id %in% quotes[[1]]$status_id)
    
    print(paste0(nrow(quotes[[1]])," quoted originals retrieved"))
    
    ## Check if there are user_ids associated with quotes to retrieve
    quotes_user_ids <- df %>% 
      filter(is_quote==TRUE) 
    
    ## Retrieve user ids for quotes
    if(nrow(quotes_user_ids)>0) {
      
      print("Retrieving user_ids of quoted original tweets...")
      
      quotes_user_ids <- quotes_user_ids %>% 
        group_by(quote_status_id) %>% 
        do(data.frame(quote_user_id=retrieve_user_id(quotes[[1]],.$quote_status_id))) %>%
        select(quote_status_id,quote_user_id)
    }
  }
  
  ## Return NA when there are no quoted originals to retrieve
  if(is.null(quotes_user_ids)) {
    
    print("No originals for quotes to retrieve")
    
    tweets <- tweets %>% mutate(quote_user_id=NA)
    
  }
  
  # Combining data 
  
  ## Rbind the retrieved tweets
  tweets <- rbind(rts[[1]],quotes[[1]]) %>%
    distinct %>%
    mutate(conversation=NA)
  
  ## Add user_ids for retweeted and quoted originals
  if(!is.null(retweets_user_ids)){
    tweets <- tweets %>% 
      left_join(.,retweets_user_ids,by=c("retweet_status_id"="retweet_status_id"))
  }
  if(!is.null(quotes_user_ids)){
    tweets <- tweets %>% 
      left_join(.,quotes_user_ids,by=c("quote_status_id"="quote_status_id"))
  }
  
  tweets <- tweets %>% unique 
  mentioned_users <- rbind(rts[[2]],quotes[[2]])
  
  exp <- list(tweets,mentioned_users)
  names(exp) <- c("tweets","mentioned_users")
  return(exp)
  
}

## Unfold and retrieve reply-chains and mark conversation numbers
retrieve_conversations <- function(df) {
  
  ## Creates index df in which replies are numbered
  tweets <- df %>%
    unique %>%
    mutate(conversation=dense_rank(.$reply_status_id) %>% as.factor) %>%
    filter(!is.na(conversation))
  
  ## How many tweets to retrieve
  ## Is recalculated after each retrieval loop
  to_retrieve <- tweets %>% 
    .$reply_status_id %>% 
    unique %>% 
    length
  
  ## Initiating index and results dfs
  tweet_ids <- NULL
  mentioned_users_ <- NULL
  
  i <- 1
  ## Keeps retrieving until no more left
  while(to_retrieve>0) {
    
    ## Determine which tweets to lookup
    lookup <- tweets %>% 
      filter(!reply_status_id %in% tweet_ids) %>% ## excludes previously retrieved tweets
      filter(!is.na(reply_status_id)) %>%
      .$reply_status_id %>%
      unique %>%
      as.character
    
    print("Retrieving tweets that were replied to...")
    
    ## Looks up tweets and checks if tweets were returned
    replies_raw <- lookup_statuses(lookup)
    
    check <- replies_raw %>% nrow %>% data.frame(x=.) %>% mutate(y=ifelse(x==0,FALSE,TRUE))
    
    ## Numbering results
    if(check$y==TRUE) {
      
      ## Processes the raw results
      replies <- process_tweets(replies_raw)
      
      ## Numbers the tweets that were just retrieved according to the associated tweet-and-reply-chain
      replies[[1]] <- left_join(replies[[1]],(tweets %>% select(reply_status_id,conversation)),
                                by=c("status_id"="reply_status_id"))
    }
    if(check$y==FALSE) {
      
      ## Returns NA in case of error (when tweets is deleted)
      replies <- list(NA,NA,NA,NA,NA,NA,NA)
    }
    
    ## Add the new tweets to the index
    tweet_ids <- c(tweet_ids,lookup) %>% unique
    
    ## Adding results to df
    if(check$y==TRUE) {
      ## Combines existing data with retrieved data
      tweets <- rbind(tweets,replies[[1]]) %>%
        unique
      mentioned_users_ <- rbind(mentioned_users_,replies[[2]])
    }
    
    ## Determining which tweets to retrieve in next round
    to_retrieve <- tweets %>% 
      filter(!reply_status_id %in% tweet_ids) %>%
      filter(!reply_status_id %in% lookup) %>%
      filter(!is.na(reply_status_id)) %>%
      .$reply_status_id %>% 
      unique %>% 
      length
    
    i <- i+1
    
    print(paste0(to_retrieve," tweets to retrieve in next round"))
    
  }
  
  print(paste0(nrow(tweets)," tweets retrieved associated with ",max(tweets$conversation %>% as.numeric)," conversations"))
  
  ## Adding extra columns (otherwise rbind error)
  tweets <- tweets %>%
    mutate(retweet_user_id=NA,
           quote_user_id=NA) %>%
    unique
  
  mentioned_users_ <- mentioned_users_ %>% filter(!status_id %in% df$status_id) %>% unique
  
  exp <- list(tweets,mentioned_users_)
  names(exp) <- c("tweets","mentioned_users")
  return(exp)
  
}

# USER RETRIEVAL

## Function to retrieve user_stats
retrieve_user_stats <- function(user_list) {
  
  print("Starting function retrieve_user_stats()")
  
  ## Creates a unique user list, counts them, and calculates required batches
  user_list <- as.character(user_list) %>% unique
  rows <- length(user_list)
  batches <- (rows/90000)%>%ceiling
  
  print(paste("## Retrieving a list of ",rows," users in",batches," batches"))
  
  ## Initiates data frame to write data to
  wip_user_stats <- NULL
  
  ## Retrieves all the users in one batch in case of less than 18k users
  if(rows<90000) {
    limit_check(rows/90000,expected=TRUE,call="users/lookup")
    
    print("### Starting first and last batch")
    wip_user_stats <- lookup_users(user_list)
    print("### Success")
  }
  
  ## In case of more than 18k users, retrieves the users in batches of 18k
  if(rows>90000) {
    
    limit_check(900,expected=TRUE,call="users/lookup")
    
    print("### Starting first of miltiple batches")
    wip_user_stats <- rbind(wip_user_stats,
                            lookup_users(user_list[1:90000]))  
    print("### Success")
    
    for(i in 1:(batches-1)) {
      
      limit_check(900,expected=TRUE,call="users/lookup")
      
      print(paste("### Starting batch",i+1))
      batch <- user_list[(1+(i*90000)):(90000+(i*90000))]
      batch <- batch[!is.na(batch)]
      print(paste("#### Retrieving",length(batch),"users"))
      
      tmp <- lookup_users(batch)
      print("### Succes")
      
      wip_user_stats <- rbind(wip_user_stats,tmp)
      
    }
    
  }
  
  print("Retrieval succesfull, proceeding with processing")
  
  ## Processes the data
  user_stats_t <- wip_user_stats %>%
    group_by(user_id=(user_id %>% as.character)) %>%
    do(
      data.frame(screen_name=(.$screen_name %>% as.character),
                 real_name=(.$name %>% as.character),
                 description=(.$description %>% as.character),
                 profile_image=(.$profile_image_url %>% as.character),
                 background_image=(.$profile_background_url %>% as.character),
                 banner_image=(.$profile_banner_url %>% as.character),
                 location=(.$location %>% as.character),
                 language=(.$account_lang %>% as.character),
                 statuses=(.$statuses_count %>% as.integer),
                 followers=(.$followers_count %>% as.integer),
                 following=(.$friends_count %>% as.integer),
                 favorites=(.$favourites_count %>% as.integer),
                 protected=(.$protected %>% as.logical),
                 verified=(.$verified %>% as.logical),
                 age_weeks=(((Sys.time() - .$account_created_at) %>% 
                               as.integer / 7) %>% round(0) %>% as.integer)
      )
    ) %>% unique
  
  return(user_stats_t)
  
}

## Function to retrieve profile urls
retrieve_url <- function(screen_name) {
  
  url <- paste("http://www.twitter.com/",tolower(screen_name),sep="")
  
  result <- read_html(url) %>% 
    html_node("span .u-textUserColor") %>%
    html_attr("title") %>%
    str_trim
  
  rest <- (runif(1)*5)
  Sys.sleep(ifelse(rest<2,3,rest))
  
  return(result)
  
}

# NETWORK RETRIEVAL

#### Returns ids of followers one by one. Note users in input df may not have more tha 75k followers
get_followers_in_batches <- function(user_id,requests) {
  
  limit_check(requests,FALSE,"followers/ids")
  
  followers <- get_followers(user_id %>% as.character,n=(requests*5000))$user_id %>% 
    as.data.frame %>% 
    ifelse(nrow(.)>0,.,NA) %>% 
    unlist %>% 
    as.character
  
  return(followers)
  
}

## Alternative function that returns NA in case of error
pos_get_followers <- possibly(get_followers_in_batches,NA)

#### Returns ids of followers for user accounts with more than 75k followers
get_followers_for_large_account <- function(user_id,calls_needed) {
  
  requests <- calls_needed
  windows <- (requests/15) %>% ceiling
  print(paste("######## Retrieving followers for user in",windows,"windows."))
  print(paste("######## Estimated time left:",(windows*15)-15,"minutes"))
  print("######## Starting window 1")
  
  limit_check(15,TRUE,"followers/ids")
  tmp <- get_followers(as.character(user_id %>% as.character),n=(calls_needed*5000))
  page <- next_cursor(tmp)
  
  followers <- tmp$user_id %>% 
    as.data.frame %>% 
    ifelse(nrow(.)>0,.,NA) %>% 
    unlist %>% 
    as.character
  
  for(i in 1:(windows-1)) {
    
    limit_check(15,TRUE,"followers/ids")
    
    print(paste("######## Starting window",i+1))
    
    tmp <- get_followers(as.character(user_id),n=(calls_needed*5000),page=page)
    page <- next_cursor(tmp)
    
    tmp <- tmp$user_id %>% 
      as.data.frame %>% 
      ifelse(nrow(.)>0,.,NA) %>% 
      unlist %>% 
      as.character
    
    followers <- c(followers,tmp)
    
  }
  
  return(followers)
  
}

## Alternative function that returns NA in case of error
pos_get_followers_for_large_account <- possibly(get_followers_for_large_account,NA)

#### Returns ids of following one by one. Note users in input df may not have more tha 75k followers
get_following_in_batches <- function(user_id,requests) {
  
  limit_check(requests,FALSE,"friends/ids")
  
  following <- get_friends(user_id %>% as.character,n=(requests*5000))$user_id %>% 
    as.data.frame %>% 
    ifelse(nrow(.)>0,.,NA) %>% 
    unlist %>% 
    as.character
  
  return(following)
  
}

## Alternative function that returns NA in case of error
pos_get_following <- possibly(get_following_in_batches,NA)

#### Returns ids of following for user accounts with more than 75k following
get_following_for_large_account <- function(user_id,calls_needed) {
  
  windows <- (calls_needed/15) %>% ceiling
  print(paste("######## Retrieving followers for user in",windows,"windows."))
  print(paste("######## Estimated time left:",(windows*15)-15,"minutes"))
  print(paste("######## Starting window 1 of ",windows))
  
  limit_check(15,TRUE,"friends/ids")
  tmp <- get_friends((user_id %>% as.character),n=(calls_needed*5000))
  page <- next_cursor(tmp)
  
  following <- tmp$user_id %>% 
    as.data.frame %>% 
    ifelse(nrow(.)>0,.,NA) %>% 
    unlist %>% 
    as.character
  
  ## hier calls needed gedeeld door 15. Moet werken nu.
  for(i in 1:(windows-1)) {
    
    limit_check(15,TRUE,"friends/ids")
    
    print(paste("######## Starting windows",i+1))
    
    tmp <- get_friends((user_id %>% as.character),n=(calls_needed**5000),page=page)
    page <- next_cursor(tmp)
    
    tmp <- tmp$user_id %>% 
      as.data.frame %>% 
      ifelse(nrow(.)>0,.,NA) %>% 
      unlist %>% 
      as.character
    
    following <- c(following,tmp)
    
  }
  
  return(following)
  
}

## Alternative function that returns NA in case of error
pos_get_following_for_large_account <- possibly(get_following_for_large_account,NA)

# COMPUTE CHARACTERISTICS

## TWEETS

## Function to retrieve conversation numbers
retrieve_con <- function(status_id_input) {
  tweets_complete %>%
    filter(status_id==status_id_input) %>%
    .$conversation
}

### retrieves number of statuses by given user_id
get_statuses <- function(input_user_id) {
  tweets_complete[tweets_complete$user_id==input_user_id,] %>% nrow
}

### retrieves whether input user_id is associated with an account that actively tweeted
get_original <- function(input_user_id) {
  ifelse((tweets_complete[tweets_complete$user_id==input_user_id & tweets_complete$original==TRUE,] %>% nrow)>0,TRUE,FALSE) 
}

## USERS

## Function to match retweets, quotes, etc. with user id's
retrieve_user_id <- function(df,input) {
  df %>% 
    filter(status_id %in% input) %>%
    .$user_id %>%
    unique %>%
    as.character
}

### processes urls from descriptions and retrieves and processes urls from link field
urls_from_description_and_link_field <- function(user_stats_t) {
  
  ## data frame of users with associated urls
  print("## Processing associated urls in descriptions")
  
  pos_expand_urls <- possibly(expand_urls,data.frame(status_code=NA,expanded_url=NA))
  
  associated_urls_1 <- user_stats_t %>%
    group_by(user_id,screen_name) %>%
    do(
      data.frame(url_desc_orig=.$description %>% str_extract_all("http*[^,]+") %>% str_extract_all("http*[^ ]+| http*$") %>% str_split("\r|\n") %>% unlist %>% as.character)
    ) %>%
    group_by(user_id,screen_name,url=url_desc_orig) %>%
    do(
      pos_expand_urls(.$url_desc_orig) %>%
        select(status_code,expanded_url)
    )%>% group_by(user_id) %>%
    do(url_filtered=ifelse(!is.na(.$status_code),ifelse(!is.na(.$expanded_url),.$expanded_url,.$url %>% as.character),.$url %>% as.character)) %>% 
    unnest %>%
    mutate(type="description") %>% 
    unique
  
  print("#### Succes")
  
  print("##Processing urls on profile pages")
  associated_urls_2 <- user_stats_t %>% 
    group_by(user_id,screen_name) %>% 
    do(url=plyr::try_default(retrieve_url(.$screen_name),NA)) %>%
    unnest %>%
    filter(!is.na(url)) %>%
    group_by(user_id,screen_name,url) %>%
    do(
      plyr::try_default(expand_urls(.$url),default=data.frame(orig_url=NA,expanded_url=NA,status_code=NA)) %>%
        select(status_code,expanded_url)
    ) %>% group_by(user_id) %>%
    do(url_filtered=ifelse(!is.na(.$status_code),.$expanded_url,.$url)) %>% 
    unnest %>%
    mutate(type="url") %>% unique
  print("#### Success")
  
  associated_urls <- rbind(associated_urls_1,associated_urls_2)
  
  return(associated_urls)
  
}

### detects if URL points to any influencer channels. returns name of platform
detect_influencer <- function(associated_urls) {
  
  print("## Identifying influencers")
  influencer <- associated_urls %>%
    group_by(user_id,url_filtered) %>%
    do(
      data.frame(influencer=plyr::try_default(identify_influencer(.$url_filtered),NA))
    ) %>%
    unique
  
  print("#### Succes")
  return(influencer)
  
}

identify_influencer <- function(url) {
  
  if(length(url)>1) {
    url <- paste0(url,collapse=" ")
  }
  
  input <- ifelse(is.na(url)," ",url)
  
  platform <- NULL
  
  if(input %>% str_detect("[I-i]nstagram")) {platform <- c(platform,"Instagram")}
  if(input %>% str_detect("[L-l]inkedin")) {platform <- c(platform,"Linkedin")}
  if(input %>% str_detect("[F-f]acebook")) {platform <- c(platform,"Facebook")}
  if(input %>% str_detect("[Y-y]outube|[Y-y]outu.be")) {platform <- c(platform,"YouTube")}
  
  return(platform)
  
}

# OTHER HELPERS

## Wordcloud data by community
create_wordcloud <- function(wordcount,tf_idf,mod_class) {
  
  wordcloud_count <- wordcount %>% 
    filter(Community %in% mod_class) %>% 
    group_by(Community) %>% 
    mutate(percentage=(count/sum(count))*100) %>% 
    ungroup
  
  wordcloud_tf_idf <- tf_idf %>% 
    filter(Community %in% mod_class)
  
  wordcloud_count %>% 
    left_join(.,wordcloud_tf_idf %>% 
                select(word.lemma,tf_idf),by=c("word.lemma"="word.lemma")) %>%
    arrange(desc(percentage,tf_idf)) %>%
    mutate(score=percentage*tf_idf) %>%
    arrange(desc(score)) %>%
    head(n=50)
}

## Create community labels
create_label <- function(com) {
  
  wordclouds_description %>% 
    filter(Community==com) %>% 
    .$word.lemma %>% 
    .[1:3] %>% 
    paste0(collapse="-")  
  
}

## Retrieve community size
retrieve_size <- function(com) {
  
  nodelist_data_frame %>%
    group_by(Community) %>%
    summarize(count=n()) %>%
    filter(Community==com) %>%
    .$count
  
}

## Retrieve authers by com
retrieve_authors <- function(com) {
  nodelist_data_frame %>%
    filter(original==TRUE) %>%
    group_by(Community) %>%
    summarize(count=n()) %>%
    filter(Community==com) %>%
    .$count
}

## Retrieve tweets by com
retrieve_tweets <- function(com) {
  tweets_complete %>% 
    left_join(.,nodelist_data_frame %>% select(Id,Community),by=c("user_id"="Id")) %>%
    filter(Community==com) %>%
    nrow
}

## Retrieve rts and quotes by com
retrieve_retweets_quotes <- function(com) {
  tweets_complete %>% 
    left_join(.,nodelist_data_frame %>% select(Id,Community),by=c("user_id"="Id")) %>%
    filter(Community==com,is_retweet==TRUE | is_quote==TRUE) %>%
    nrow
}

## return nr of members for com
nr_of_members <- function(com) {
  
  nodelist_data_frame %>% filter(Community==com) %>% nrow
  
} 

## Retrieve replies by com
retrieve_reply <- function(com) {
  tweets_complete %>% 
    left_join(.,nodelist_data_frame %>% select(Id,Community),by=c("user_id"="Id")) %>%
    filter(Community==com,is_reply==TRUE) %>%
    nrow
}

## Calculates color for wordclouds
calculateColors <- function(values) {
  
  ## Use n equally spaced breaks to assign each value to n-1 equal sized bins 
  ii <- cut(values, breaks = seq(min(values), max(values), len = 100), 
            include.lowest = TRUE)
  ## Use bin indices, ii, to select color from vector of n-1 equally spaced colors
  colors <- colorRampPalette(c("lightgrey", "darkblue"))(99)[ii]
  
  return(colors)
  
}


## Creates plots
plot_cloud <- function(df,com) {
  
  df %>%
    filter(Community==com) %>%
    head(25) %>%
    select(word.lemma,count,tf_idf) %>%
    mutate(tf_idf=ifelse(is.na(tf_idf),0,tf_idf)) %>%
    mutate(color=calculateColors(tf_idf)) %>% 
    select(word=word.lemma,freq=count,color) %>%
    wordcloud2(.,fontFamily = 'Submariner', fontWeight = 'Bold', color=.$color)
  
}

## Helper function to calculate flow weights
weight <- function(com) {
  
  raw <- tweets_complete %>%
    left_join(.,nodelist_data_frame %>% select(Id,Community),by=c("user_id"="Id")) %>%
    left_join(.,plots[,1:2],by=c("Community"="Community"))
  
  ## Where are tweets from this community retweeted? (tweets flow there)
  ids_com <- raw %>% 
    filter(Community==com) %>%
    .$status_id
  
  rt_wt <- raw %>%
    filter(retweet_status_id %in% ids_com) %>%
    group_by(Community,Label) %>%
    summarize(count=n()) %>%
    filter(!is.na(Label)) %>%
    ungroup %>%
    mutate(perc=count/sum(count)) %>%
    arrange(desc(perc)) %>%
    select(target=Community,Label,retweets=count)
  
  
  ## Where are tweets from this community quoted? (tweets flow there)
  qt_wt <- raw %>%
    filter(quote_status_id %in% ids_com) %>%
    group_by(Community,Label) %>%
    summarize(count=n()) %>%
    filter(!is.na(Label)) %>%
    ungroup %>%
    mutate(perc=count/sum(count)) %>%
    arrange(desc(perc)) %>%
    select(target=Community,Label,quotes=count)
  
  
  ## To tweets from which community is mostly replied? (replies flow there)
  ids <- raw %>% 
    filter(Community==com, is_reply==TRUE) %>% 
    .$status_id
  
  convs <- conversations %>%
    filter(status_id %in% ids) %>% 
    .$conversation %>% 
    unique()
  
  related_ids <- conversations %>%
    filter(conversation %in% convs, !status_id %in% ids) %>%
    .$status_id ## al status id from associated conversations except input ids
  
  rp_wt <- raw %>%
    filter(status_id %in% related_ids) %>% 
    group_by(Community,Label) %>% 
    summarize(count=n()) %>%
    filter(!is.na(Label)) %>%
    ungroup %>%
    mutate(perc=count/sum(count)) %>% 
    arrange(desc(perc)) %>%
    select(target=Community,Label,replies=count)
  
  ## From which community followers? (tweets flow there)
  ids <- raw %>%
    filter(Community==com) %>%
    .$user_id
  
  fl_wt <- edgelist_data_frame %>%
    filter(source %in% ids) %>%
    left_join(.,(nodelist_data_frame %>% select(Id,Community)),by=c("target"="Id")) %>%
    left_join(.,plots[,1:2],by=c("Community"="Community")) %>%
    filter(!is.na(Label)) %>%
    group_by(Community,Label) %>% 
    summarize(count=n()) %>%
    ungroup %>%
    select(target=Community,Label,Weight=count)
  
  exp <- rt_wt %>%
    left_join(.,qt_wt %>% select(target,quotes),by=c("target"="target")) %>%
    left_join(.,rp_wt %>% select(target,replies),by=c("target"="target")) %>%
    left_join(.,fl_wt %>% select(target,Weight),by=c("target"="target"))
  
  return(exp)
  
}
