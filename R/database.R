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

#' function for saving mensaplan to disk
#' @param res the result of a call to mensaplan() or parse_mensaplan()
mp_save <- function(res, path=""){
  make_ids <- function(x){
    stringr::str_replace_all(
      apply( x[, idvars],  1,  stringr::str_c, sep="", collapse="|"),
    " ",  ""  )
  }
  # connect
  db <- db_connect()
  # create dishes table if needed
  if( !("dishes" %in% DBI::dbListTables(db)) ){
    createDishes <-
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
    DBI::dbSendQuery(db, createDishes)
  }
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
  return(res)
}








