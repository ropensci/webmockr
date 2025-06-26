# query mapper for BodyPattern
# attempt to convert input to an R object regardless of format
query_mapper <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }
  x
}
