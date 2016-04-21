#' Save results locally
#' @import digest
#' @param x Output from API call
#' @param y Cache key
#' @export
#' @keywords internal
save_local <- function(x, y, path="~/") {
  hash <- digest::digest(y)
  filepath <- paste(path, hash, ".rds", sep="")
  saveRDS(object = x, file = filepath)
}
