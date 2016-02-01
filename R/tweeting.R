#' function for generating tweets
#' @param date date for which tweets shall be generated
#' @param lang language of tweets
#' @param loc location of mensa
#' @param dish types
#' @param regex possible regex for further subsetting
#' @param invert shall regex be inverted?
gen_tweets <- function(date=Sys.Date(), lang="de", loc="mensa_giessberg", type="all", regex="^.*", invert=FALSE){
  # dev
  # date=Sys.Date(); lang="de"; loc="mensa_giessberg"; type="all"; regex="^.*"; invert=FALSE
  # get data
  mpdat <- db_get_dish_data(lang=lang, loc=loc, date=date)
  # subset data
  iffer <-
    mpdat$date %in% as.character(date) &
    !grepl("[kK]eine[ ]*Ausgabe", mpdat$dish) &
    !grepl("keine Daten", mpdat$type) &
    mpdat$loc %in% loc &
    mpdat$lang %in% lang &
    (mpdat$type %in% type | type == "all") &
    !invert*grepl(regex, apply(mpdat, 1, paste, collapse=" "))
  mpdat <- mpdat[iffer,]
  # gen tweets
  tweets <- character(0)
  tweet_dat <- mpdat[0, c("loc","date","lang","type")]
  for ( i in seq_along(mpdat$dish) ){
    if(stringr::str_length(mpdat$dish[i])>105){
      tmp_dish <- unlist(stringr::str_split(stringr::str_wrap(mpdat$dish[i], 105),"\n"))
      tmp_dish <- paste0(seq_along(tmp_dish), "/", length(tmp_dish) , " ", tmp_dish)
    }else{
      tmp_dish <- mpdat$dish[i]
    }
    tmp_tw <-
      paste0(
      stringr::str_replace_all(
        paste0("[",mpdat$type[i],"]", " ", tmp_dish ),
        " *\\(.*?\\)",
        ""
      ),
      " (", mpdat$date[i], ")",
      ""
    )
    tweets <- c(tweets, tmp_tw)
    #add data to tweetdat
    for ( k in seq_along(tmp_tw) ){
      tweet_dat <- rbind(tweet_dat, mpdat[i, c("loc","date","lang","type")])
    }
  }
  tweets <- stringr::str_replace(tweets, ",,", ", ")
  tweets <- data.frame( tweet=ifelse(nchar(tweets) > 140, paste0(substring(tweets, 1, 136), " ..."), tweets ),
                        stringsAsFactors = FALSE)
  tweets$nchar <- stringr::str_length(tweets$tweet)
  tweets <- tweets[!grepl("\\[\\]", tweets$tweet), ]
  # combine with other data
  tweets <- cbind(tweets, tweet_dat)
  # return
  return(tweets)
}


twitter_token <- function(){
  # get key and secret
  key    = Sys.getenv("unikonstanzmensabot_twitter_key")
  secret = Sys.getenv("unikonstanzmensabot_twitter_secret")
  token        = Sys.getenv("unikonstanzmensabot_twitter_token")
  token_secret = Sys.getenv("unikonstanzmensabot_twitter_token_secret")
  stopifnot(key!="", secret!="")

  # make app
  myapp <- httr::oauth_app( "twitter", key, secret )

  if( token=="" | token_secret==""){
    # get credentials
    twitter_token <- httr::oauth1.0_token(httr::oauth_endpoints("twitter"), myapp)
    # return
    return(twitter_token)
  }else{
    twitter_token <-
      httr::Token1.0$new(
        endpoint      = NULL,
        params        = list(as_header = TRUE),
        app           = myapp,
        credentials   = list(
          oauth_token = token,
          oauth_token_secret = token_secret
        )
      )
    return(twitter_token)
  }
}


tweet <- function(tweets){
  stopifnot( dim(tweets)[1]<=1 )
  db_ensure_table_exists("tweets")

  # check if tweet was already made
  dat <- db_get_tweet_data( date=tweets$date, loc=tweets$loc, lang=tweets$lang )
  if ( any(tweets$type %in% dat$type) ){
    warning("was already tweeted")
    return(NULL)
  }

    # tweet away
    req <-
      httr::POST(
        url  = "https://api.twitter.com/1.1/statuses/update.json",
        body = list(
          status = tweets$tweet,
          lat = "47.690528",
          long = "9.188986",
          display_coordinates = "true"
        ) ,
        httr::config(token = twitter_token())
    )
    # write to db or return contents
    if( httr::status_code(req) == 200 ){
      cnt <- httr::content(req)
      tweets$id_str     <- cnt$id_str
      tweets$created_at <- cnt$created_at
      tweets$text       <- cnt$text
      tweet <- tweets[, c("loc", "lang", "date", "type", "id_str", "text", "created_at")]
      db <- db_connect()
      RSQLite::dbWriteTable(db, "tweets", tweet, append=TRUE)
    }else{
      warning(
        "something went wrong"
      )
    }
  # return
  return(req)
}








