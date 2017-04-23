http_verbs <- c("any", "get","post","put","patch","head","delete")

cc <- function(x) Filter(Negate(is.null), x)

hdl_lst <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (inherits(x, "list")) {
    return(paste(names(x), unname(x), sep = "=", collapse = ", "))
  } else {
    x
  }
}

parseurl <- function(x) {
  tmp <- urltools::url_parse(x)
  tmp <- as.list(tmp)
  if (!is.na(tmp$parameter)) {
    tmp$parameter <- sapply(strsplit(tmp$parameter, "&")[[1]], function(z) {
      zz <- strsplit(z, split = "=")[[1]]
      as.list(stats::setNames(zz[2], zz[1]))
    }, USE.NAMES = FALSE)
  }
  tmp
}

url_build = function(uri, args = NULL) {
  if (is.null(args)) return(uri)
  paste0(uri, "?", paste(names(args), args, sep = "=", collapse = ","))
}

`%||%` <- function(x, y) if (is.null(x)) y else x
