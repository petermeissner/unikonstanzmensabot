#' function for generating tweets
#' @param date date for which tweets shall be generated
#' @param lang language of tweets
#' @param loc location of mensa
#' @param dish types
#' @param regex possible regex for further subsetting
#' @param invert shall regex be inverted?
gen_tweets <- function(date=Sys.Date(), lang="de", loc="mensa_giessberg", types="all", regex="^.*", invert=FALSE){
  # get data
  mpdat <- mp_data()
  # subset data
  iffer <-
    mpdat$date_dish %in% date &
    !grepl("[kK]eine[ ]*Ausgabe", mpdat$dish) &
    !grepl("keine Daten", mpdat$types) &
    mpdat$location %in% loc &
    mpdat$language %in% lang &
    mpdat$http_status == 200 &
    (mpdat$types %in% types | types == "all") &
    !invert*grepl(regex, apply(mpdat, 1, paste, collapse=" "))
  mpdat <- mpdat[iffer,]
  # gen tweets
  tweets <-
    stringr::str_replace_all(
      paste0("[",mpdat$types,"]", " ", mpdat$dish),
      " *\\(.*?\\)",
      ""
    )
  tweets <- data.frame( tweet=ifelse(nchar(tweets) > 140, paste0(substring(tweets, 1, 136), " ..."), tweets ),
                        stringsAsFactors = FALSE)
  tweets$nchar <- nchar(tweets$tweet)
  # combine with other data
  tweets <- cbind(tweets, mpdat[, c("location","date_dish","language","types")])
  # return
  return(tweets)
}