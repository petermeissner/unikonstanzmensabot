#
library(unikonstanzmensabot)
mensaplan()
tweets <- gen_tweets()
lapply(tweets$tweet, tweet)
