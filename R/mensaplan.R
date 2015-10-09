#' function POSTing HTTP requests to data server
#' @param date date for which to get data
#' @param lang language of the mensa plan -- either "de" or "en"
#' @param loc location identifier

mp_post <-
  function(
    lang = c("de","en"),
    date = format(Sys.Date(), "%Y-%m-%d"),
    loc  = "mensa_giessberg"
  )
  {
    url <- "https://www.max-manager.de/daten-extern/seezeit/html/inc/ajax-php_konnektor.inc.php"
    post_results <-
      httr::POST(
        url,
        body = list(
          func = "make_spl",
          loc  = loc,
          lang = lang[1],
          date = date
        )
      )
    return(post_results)
  }

mp_request <- function(
  lang = c("de","en"),
  date = format(Sys.Date(), "%Y-%m-%d"),
  loc= "mensa_giessberg")
{
  lang <- lang[1]
  # create table in DB
  db_ensure_table_exists("", "requests")
  # check for old requests
  db <- db_connect()
  sql <- paste0(
    "SELECT * FROM requests WHERE \n",
    "lang = '",   lang,"' AND \ndate = '", date, "' AND \nloc = '",loc,"' AND \nstatus = '200'"
  )
  res <- RSQLite::dbGetQuery(db, sql)
  if( dim(res)[1]==0 ) {
    res  <- mp_post(lang, date, loc)
    # extract content
    cont <- httr::content(res, encoding="UTF-8", type="text")
    # prepare data frame for db
    df <-
      data.frame(
        req_date  = res$date,
        status    = res$status_code,
        res$request$fields,
        httr_content_as_list(res$times, "t_"),
        httr_content_as_list(res$cookies, "cookies_"),
        length    = length(res$content),
        content   = cont,
        stringsAsFactors = FALSE
      )
    # write data to db
    RSQLite::dbWriteTable(db, "requests", df, append = TRUE )
  }else{
    df <- res
    df$req_date <- Sys.time()
    df$status   <- 0
    df[,7:21]   <- ""
    RSQLite::dbWriteTable(db, "requests", df, append = TRUE )
  }
  RSQLite::dbDisconnect(db)
  return(TRUE)
}

#' function parsing data retrieved
#' @param post_results results from get_mesaplan()

mp_parse <- function(post_results){
  # infor derives directly from http request
  http_status    <- post_results$status_code
  date_request   <- post_results$date
  content        <- httr::content(post_results, as="text", encoding="UTF-8")
  content_length <- nchar(content)
  content        <- ifelse(content_length==0, "<body></body>", content)
  date_dish      <- post_results$request$fields$date
  language       <- post_results$request$fields$lang
  location       <- post_results$request$fields$loc
  # dish types
  tmp       <- rvest::html_text(rvest::html_nodes(xml2::read_html(content, encoding="UTF-8"), xpath = "//tr/td[1]"))
  if( length(tmp)>0 ){
    types     <- stringr::str_replace_all(iconv(even(tmp), "UTF-8", "latin1"), "\n", "")
  }else{
    types <- NA
  }
  # dishes and additives
  tmp       <-
    rvest::html_text(
      rvest::html_nodes(
        xml2::read_html(content, encoding = "UTF-8"),
        xpath = "//tr/td[2]"
      )
    )
  if( length(tmp)>0 ){
    dish      <- stringr::str_replace_all(iconv(even(tmp), "UTF-8", "latin1"), "\n", "")
    additives <- stringr::str_replace_all(iconv( odd(tmp), "UTF-8", "latin1"), "\n", "")
  }else{
    dish      <- NA
    additives <- NA
  }
  # results
  res <-
    data.frame(
      date_request,
      date_dish,
      language,
      types,
      dish,
      additives,
      location,
      http_status,
      content_length
  )
  # return
  return(res)
}

#' wrapper function for get_mensaplan() and parse_mensaplan
#' @param date date for which to get data
#' @param lang language of the mensa plan -- either "de" or "en"
#' @param loc location identifier
mensaplan <- function(
  lang = c("de","en"),
  date = format(Sys.Date(), "%Y-%m-%d"),
  loc  = "mensa_giessberg"
){
  res <- mp_parse(mp_scrape(lang, date, loc))
  mp_save(res)
  class(res) <- c("mensaplan", class(res))
  return(res)
}

#' customized print function for mensaplan data.frames
#' @inheritParams print.data.frame
print.mensaplan <- function (
  x, ..., digits = NULL, quote = FALSE, right = TRUE, row.names = TRUE
)
{
  n <- length(row.names(x))
  if (length(x) == 0L) {
    cat(sprintf(ngettext(n, "data frame with 0 columns and %d row",
                         "data frame with 0 columns and %d rows", domain = "R-base"),
                n), "\n", sep = "")
  }
  else if (n == 0L) {
    print.default(names(x), quote = FALSE)
    cat(gettext("<0 rows> (or 0-length row.names)\n"))
  }
  else {
    m <- as.matrix(format.data.frame(x, digits = digits,
                                     na.encode = FALSE))
    if (!isTRUE(row.names))
      dimnames(m)[[1L]] <- if (identical(row.names, FALSE))
        rep.int("", n)
    else row.names
    for (i in seq_along(m[1,])) {
      if ( max(nchar(m[,i])) > 20 ) {
        m[,i] <- paste0(substring(m[,i], 1, 16), " ...")
      }
    }
    print(m, ..., quote = quote, right = right)
  }
  invisible(x)
}











