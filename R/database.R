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
db_ensure_table_exists <- function(path="", table=""){
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

#' function for saving mensaplan to disk
#' @param res the result of a call to mensaplan() or parse_mensaplan()
mp_save <- function(res, path=""){
  make_ids <- function(x){
    idvars <- c("location", "language", "date_dish", "types", "http_status", "content_length")
    stringr::str_replace_all(
      apply( x[, idvars],  1,  stringr::str_c, sep="", collapse="|"),
    " ",  ""  )
  }
  # create dishes table if needed
  db_ensure_table_exists(path, "dishes")
  # connect
  db <- db_connect(path)
  # read in all data add new data and sanatize
  tmp <- RSQLite::dbReadTable(db, "dishes")
  old_length <- dim(tmp)[1]
  tmp <- rbind(tmp, res)
  tmp <- tmp[order(-tmp$content_length, -tmp$date_request),]
  dupl_vars <- c("location", "language", "date_dish", "types")
  duplicates <- tmp[duplicated(tmp[, dupl_vars]),]
  drop <- NULL
  keep <- NULL
  for( i in seq_along(duplicates[,1])){
    iffer <-
      tmp$location == duplicates[i,"location"] &
      tmp$language == duplicates[i,"language"] &
      tmp$date_dish == duplicates[i,"date_dish"] &
      tmp$types == duplicates[i,"types"]
    keep <- c(keep, seq_along(tmp[,1])[iffer & tmp$http_status==200][1])
    drop <- c(drop, seq_along(tmp[,1])[iffer & tmp$http_status==200])
  }
  drop <- drop[!(drop %in% keep)]
  if(!is.null(drop)){
    tmp <- tmp[-drop,]
  }
  new_length <- dim(tmp)[1]
  # write data back to db
  dbres <- RSQLite::dbWriteTable(db, "dishes", tmp, overwrite=TRUE)
  db_disconnect(db)
  message("\nAdding ", ifelse(new_length > old_length, new_length-old_length, 0), " dishes to database.")
  # return
  return(dbres)
}

#' function for reading the contents of the local mensaplan database
mp_data <- function(path=""){
  db  <- db_connect(path)
  res <- RSQLite::dbReadTable(db, "dishes")
  db_disconnect(db)
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








