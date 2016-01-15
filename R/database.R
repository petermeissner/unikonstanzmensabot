#' function for determining the path to the db used for storage
#' @param path path to database
db_path <- function(path=""){
  if ( path != "" ) {
    return(path)
  }else{
    ifelse(
      Sys.getenv("ukm_bot_data_path")=="",
      path.expand("~/.unikonstanzmensabot_data.sqlite3"),
      Sys.getenv("ukm_bot_data_path")
    )
  }
}

#' function for connectiong to db
#' @param path path to database
db_connect <- function(path=""){
  RSQLite::dbConnect( RSQLite::SQLite(), db_path( path ) )
}

#' function for disconnecting from db
#' @param db connection to db
db_disconnect <- function(db){
  RSQLite::dbDisconnect(db)
}

#' function for ensuring that a particular table exists in db
db_ensure_table_exists <- function(table="", path=""){
  # check if info on table exists
  stopifnot( !is.null(storage$tables[[table]]) )
  # connect to db
  db <- db_connect(path)
  # create table if not existent
  if( !(table %in% RSQLite::dbListTables(db)) ){
    create_table <- storage$tables[[table]]
    res <- RSQLite::dbGetQuery(db, create_table)
    if (is.null(res) ) res <- TRUE
  }else{
    res <- TRUE
  }
  db_disconnect(db)
  return(res)
}



#' function for retrieving data from requests table in db
db_get_request_data <- function(date=Sys.Date(), status=200, loc="mensa_giessberg", lang="de"){
  db <- db_connect()
  sql_innize <- function(x){paste0("(", paste0("'",x ,"'", collapse = ", "), ")")}
  sql <- paste0(
    "SELECT * FROM requests WHERE ",
    "     \n status IN ",  sql_innize(status),
    "  AND\n date   IN ", sql_innize(date),
    "  AND\n loc    IN ", sql_innize(loc),
    "  AND\n lang   IN ",  sql_innize(lang[1])
  )
  res <- RSQLite::dbGetQuery(db, sql)
  db_disconnect(db)
  return(res)
}


#' function for retrieving data from dishes table in db
db_get_dish_data <- function(date=Sys.Date(), loc="mensa_giessberg", lang="de"){
  db <- db_connect()
  sql_innize <- function(x){paste0("(", paste0("'",x ,"'", collapse = ", "), ")")}
  sql <- paste0(
    "SELECT * FROM dishes WHERE ",
    "     \n loc IN ",  sql_innize(loc),
    "  AND\n lang   IN ", sql_innize(lang),
    "  AND\n date    IN ", sql_innize(date)
  )
  res <- RSQLite::dbGetQuery(db, sql)
  db_disconnect(db)
  return(res)
}

#' function for repairing encodings
function(text){
  grep()
}

#' function for retrieving data from tweets table in db
db_get_tweet_data <- function(date=Sys.Date(), loc="mensa_giessberg", lang="de"){
  db <- db_connect()
  sql_innize <- function(x){paste0("(", paste0("'",x ,"'", collapse = ", "), ")")}
  sql <- paste0(
    "SELECT * FROM tweets WHERE ",
    "     \n loc IN ",  sql_innize(loc),
    "  AND\n lang   IN ", sql_innize(lang),
    "  AND\n date    IN ", sql_innize(date)
  )
  res <- RSQLite::dbGetQuery(db, sql)
  db_disconnect(db)
  return(res)
}




