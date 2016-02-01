#!/usr/bin/Rscript
# doing duty to do

library(unikonstanzmensabot)
options("httr_oauth_cache"=FALSE)


# download data
mensaplan()

# generate tweets
tweets <- gen_tweets()
tweets

# tweet away
RES <- list()
for ( i in seq_len(dim(tweets)[1]) ) {
  RES[[i]] <- tweet(tweets[i,])
}

lapply(RES, class)

# logging
line <- paste(
  Sys.time(), tweets$loc, tweets$lang, tweets$date, tweets$nchar, tweets$type,
sep=" , ")
write(line, file="dutytodo.log", append=TRUE)


# bulk data retrieval
#library(unikonstanzmensabot)
#dates <- seq(from=as.Date("2015-10-12"), to=as.Date("2015-12-24"), by="day")
#for ( i in dates ) {
#  mensaplan(as.character(as.Date(i, origin="1970-01-01")))
#}


# tweet_force <- function(tweets){
#   stopifnot( dim(tweets)[1]<=1 )
#   db_ensure_table_exists("tweets")
#
#   # tweet away
#   req <-
#     httr::POST(
#       url  = "https://api.twitter.com/1.1/statuses/update.json",
#       body = list(
#         status = tweets$tweet,
#         lat = "47.690528",
#         long = "9.188986",
#         display_coordinates = "true"
#       ) ,
#       httr::config(token = twitter_token())
#     )
#   # write to db or return contents
#   if( httr::status_code(req) == 200 ){
#     cnt <- httr::content(req)
#     tweets$id_str     <- cnt$id_str
#     tweets$created_at <- cnt$created_at
#     tweets$text       <- cnt$text
#     tweet <- tweets[, c("loc", "lang", "date", "type", "id_str", "text", "created_at")]
#     db <- db_connect()
#     RSQLite::dbWriteTable(db, "tweets", tweet, append=TRUE)
#   }else{
#     warning(
#       "something went wrong"
#     )
#   }
#   # return
#   return(req)
# }
