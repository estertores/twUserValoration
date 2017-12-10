library(twitteR)
library(curl)
library(httr)


setup_twitter_oauth(ConsumerKey, ConsumerSecret, AccessToken, AccessTokenSecret)

GetTimeLine <- function( user ){
  cat(paste0("Asking for ", user, " timeline... "))
  timeLineWithRT <- userTimeline(user, n=3200, includeRts = T)
  cat("DONE!\n")
  timeLine <- sapply(timeLineWithRT, function(x) {if(!x$isRetweet){x} else{ NULL}} )
  timeLine[sapply(timeLine, is.null)] <- NULL
  timeLine
}

GetRepercussionIndex <- function( timeLine, valoration ){
  totalDays <- as.numeric(difftime(timeLine[[1]]$created, timeLine[[length(timeLine)]]$created, units = "days"))
  
  repercussion <- sapply(timeLine, function(tw){ 
    (tw$retweetCount/max(tw$retweetCount, valoration@repercussionRT) + 
       tw$favoriteCount/max(tw$favoriteCount, valoration@repercussionFAV))/2
  })
  index <- sum(repercussion) / max(length(repercussion), valoration@frequency*totalDays)
  index
}

f1 <- function( followers, lambda ){
  return (exp(-lambda/followers))
}

f2 <- function( followers, mu ){
  return (atan(followers/mu)/(pi/2))
}

setClass("valoration",
         slots = c(frequency="numeric", 
                   repercussionRT="numeric", 
                   repercussionFAV="numeric",
                   lambda="numeric",
                   mu="numeric"))

GetTimeLineSummary <- function( tl ){
  do.call(rbind, lapply(tl, function(x) data.frame(
    start = x$created,
    event = paste0("RT: ", x$retweetCount, "\nFAV: ", x$favoriteCount))))
}

