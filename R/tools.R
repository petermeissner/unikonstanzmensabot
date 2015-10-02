#' function giving back elements with odd index
#' @param x vector for which to give back even index
odd  <- function(x) x[(seq_len(floor(length(x)/2)))*2]

#' function giving back elements with even index
#' @param x for which to give back odd index
even <- function(x) x[(seq_len(ceiling(length(x)/2)))*2-1]

