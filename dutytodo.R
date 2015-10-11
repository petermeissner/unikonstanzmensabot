# doing duty to do
library(unikonstanzmensabot)
  # bulk data retrieval
  #dates <- seq(from=as.Date("2015-10-12"), to=as.Date("2015-12-24"), by="day")
  #for ( i in dates ) {
  #  mensaplan(as.character(as.Date(i, origin="1970-01-01")))
  #}
mensaplan()
tweets <- gen_tweets()
lapply(tweets, tweet)
