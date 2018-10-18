## Returns requests left for certain api call (standard: retrieve friends)
requests_left <- function(call="followers/ids") {
  rate_limit(twitter_token) %>% filter(query==call) %>% .$remaining
}

## Returns time left until refresh of rate limit
time_left <- function(call="followers/ids") {
  rate_limit(twitter_token) %>% filter(query==call) %>% .$reset
}

## Checks limit before retrieving data
limit_check <- function(requests,expected=FALSE,call="follower/ids") {
  
  ## Limit check
  calls_to_go <- requests_left(call)
  calls_needed <- requests
  time_to_go <- time_left(call) %>% as.numeric
  
  while(calls_to_go<calls_needed) {
    if(expected==FALSE) { 
      print("## Rate limit exceeded unexpectedly!")
    }
    print(paste("#### We need",calls_needed,"calls, but have",calls_to_go ,"left..."))
    print(paste("#### The current time is",Sys.time(),"Please wait."))
    print(paste("#### Rate limit resetting in",time_to_go,"minutes."))
    if(calls_to_go<calls_needed) {
      Sys.sleep(60*(time_to_go+1))  
    }
    calls_to_go <- requests_left(call)
    time_to_go <- time_left(call) %>% as.integer
    if(calls_to_go>=calls_needed) {
      print("Rate Limit refreshed. Continuing...")
    }
  }
  
}