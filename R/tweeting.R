#' function for generating tweets

dings <- 1
if(1==2){

  gen_tweets <- function(date=Sys.Date(), lang="de", loc="mensa_giessberg", types="all", regex="^.*", invert=FALSE){}
  mpdat <- mp_data()

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

  tweets <-
    stringr::str_replace_all(
      paste0(mpdat$types, ": ", mpdat$dish),
      " *\\(.*?\\)",
      ""
    )
  tweets <- data.frame( tweet=ifelse(nchar(tweets) > 140, paste0(substring(tweets, 1, 136), " ..."), tweets ),
                        stringsAsFactors = FALSE)
  tweets$nchar <- nchar(tweets$tweet)
  tweets <- cbind(tweets, mpdat[, c("types","","")])
    mpdat




}