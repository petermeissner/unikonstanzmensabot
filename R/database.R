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

#' function for connectiong to db used for storage
#' @param path path to database
db_connect <- function(path=""){
  DBI::dbConnect( RSQLite::SQLite(), db_path( path ) )
}


#' function for ensuring that dishes table exists in db
db_ensure_exists_dishes <- function(path=""){
  db <- db_connect(path)
  if( !("dishes" %in% DBI::dbListTables(db)) ){
    createTable <-
      'CREATE TABLE dishes (
    "date_request" INTEGER,
    "date_dish" TEXT,
    "language" TEXT,
    "types" TEXT,
    "dish" TEXT,
    "additives" TEXT,
    "location" TEXT,
    "http_status" INTEGER,
    "content_length" INTEGER
    )'
    res <- DBI::dbSendQuery(db, createTable)
  }
  DBI::dbDisconnect(db)
  return(res)
}

#' function for ensuring that tweets table exists in db
db_ensure_exists_tweets <- function(path=""){
  db <- db_connect(path)
  if( !("dishes" %in% DBI::dbListTables(db)) ){
    createTable <-
      'CREATE TABLE tweets (
    "location" TEXT,
    "date_dish" TEXT,
    "language" TEXT,
    "types" TEXT,
    "attempts" INTEGER,
    "tweeted" INTEGER,
    "date_attempts" INTEGER,
    "tweet" TEXT
    )'
    res <- DBI::dbSendQuery(db, createTable)
  }
  DBI::dbDisconnect(db)
  return(res)
  }

#' function for saving mensaplan to disk
#' @param res the result of a call to mensaplan() or parse_mensaplan()
mp_save <- function(res, path=""){
  make_ids <- function(x){
    stringr::str_replace_all(
      apply( x[, idvars],  1,  stringr::str_c, sep="", collapse="|"),
    " ",  ""  )
  }
  # create dishes table if needed
  db_ensure_exists_dishes(path)
  # connect
  db <- db_connect(path)
  # check for duplicates and sanatize if needed
  tmp <- DBI::dbReadTable(db, "dishes")
  idvars <- c("location", "language", "date_dish", "types", "http_status", "content_length")
  tmp_ids <- make_ids(tmp)
  if( any(duplicated(tmp_ids) )){
    DBI::dbWriteTable(db, "dishes", tmp[!duplicated(tmp_ids), ], overwrite=TRUE)
  }
  res_ids <- make_ids(res)
  message("\nAdding ", sum(!(res_ids %in% tmp_ids)), " dishes to database.")
  # write new data to db
  dbres <- DBI::dbWriteTable(db, "dishes", res[!(res_ids %in% tmp_ids),], append=TRUE)
  DBI::dbDisconnect(db)
  # return
  return(dbres)
}

#' function for reading the contents of the local mensaplan database
mp_data <- function(path=""){
  db  <- db_connect(path)
  res <- DBI::dbReadTable(db, "dishes")
  DBI::dbDisconnect(db)
  for(i in seq_along(res[1,])){
    if( class(res[,i])=="character" ) {
      Encoding(res[,i]) <- "UTF-8"
    }
  }
  res$date_request <- as.POSIXct(res$date_request, origin="1970-01-01")
  res$date_dish <- as.Date(res$date_dish)
  class(res) <- c("mensaplan", class(res))
  return(res)
}








