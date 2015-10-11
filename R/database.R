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









