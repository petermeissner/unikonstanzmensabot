#' function for generating tweets
#' @param date date for which tweets shall be generated
#' @param lang language of tweets
#' @param loc location of mensa
#' @param dish types
#' @param regex possible regex for further subsetting
#' @param invert shall regex be inverted?
gen_tweets <- function(date=Sys.Date(), lang="de", loc="mensa_giessberg", type="all", regex="^.*", invert=FALSE){
  # get data
  mpdat <- db_get_dish_data(lang=lang, loc=loc, date=date)
  # subset data
  iffer <-
    mpdat$date %in% date &
    !grepl("[kK]eine[ ]*Ausgabe", mpdat$dish) &
    !grepl("keine Daten", mpdat$type) &
    mpdat$loc %in% loc &
    mpdat$lang %in% lang &
    (mpdat$type %in% type | type == "all") &
    !invert*grepl(regex, apply(mpdat, 1, paste, collapse=" "))
  mpdat <- mpdat[iffer,]
  # gen tweets
  tweets <-
    paste0(
      stringr::str_replace_all(
        paste0(" [",mpdat$type,"]", " ", mpdat$dish),
        " *\\(.*?\\)",
        ""
      ),
      " (", mpdat$date, ")"
    )
  tweets <- data.frame( tweet=ifelse(nchar(tweets) > 140, paste0(substring(tweets, 1, 136), " ..."), tweets ),
                        stringsAsFactors = FALSE)
  tweets$nchar <- nchar(tweets$tweet)
  tweets <- tweets[!grepl("\\[\\]", tweets$tweet), ]
  # combine with other data
  tweets <- cbind(tweets, mpdat[, c("loc","date","lang","type")])
  # return
  return(tweets)
}


twitter_token <- function(){
  # get key and secret
  key    = Sys.getenv("unikonstanzmensabot_twitter_key")
  secret = Sys.getenv("unikonstanzmensabot_twitter_secret")
  stopifnot(key!="", secret!="")

  # make app
  myapp <- httr::oauth_app( "twitter", key, secret )
  # get credentials
  twitter_token <- httr::oauth1.0_token(httr::oauth_endpoints("twitter"), myapp)
  # return
  twitter_token
}


tweet <- function(tweets){
  # tweet away
  req <-
    httr::POST(
      url  = "https://api.twitter.com/1.1/statuses/update.json",
      body = list(
        status = tweets$tweet,
        lat = 47.690528,
        long = 9.188986,
        display_coordinates = "true"
      ) ,
      httr::config(token = twitter_token()))
  # return
  return(httr::content(req))
}








