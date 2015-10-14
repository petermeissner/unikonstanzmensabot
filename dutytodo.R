#!/usr/bin/Rscript
# doing duty to do

library(unikonstanzmensabot)

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
