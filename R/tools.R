#' function giving back elements with odd index
#' @param x vector for which to give back even index
odd  <- function(x) x[(seq_len(floor(length(x)/2)))*2]

#' function giving back elements with even index
#' @param x for which to give back odd index
even <- function(x) x[(seq_len(ceiling(length(x)/2)))*2-1]

#' function for making lists with prefixed names
#' @param x named vector to make list of
httr_content_as_list <- function(x, prefix=""){
  x <- as.list(x)
  if(length(names(x))>0) names(x) <- paste0(prefix, names(x))
  for( i in seq_along(x) ){
    if( length(x[[i]])==0 ){
      x[[i]] <- ""
    }
  }
  return(x)
}

