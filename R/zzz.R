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

url_builder <- function(uri, args = NULL) {
  if (is.null(args)) return(uri)
  paste0(uri, "?", paste(names(args), args, sep = "=", collapse = ","))
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0 || nchar(x) == 0) y else x

`!!` <- function(x) if (is.null(x) || is.na(x)) FALSE else TRUE

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
}

crul_head_parse <- function(z) {
  if (grepl("HTTP\\/", z)) {
    list(status = z)
  } else {
    ff <- regexec("^([^:]*):\\s*(.*)$", z)
    xx <- regmatches(z, ff)[[1]]
    as.list(stats::setNames(xx[[3]], tolower(xx[[2]])))
  }
}

crul_headers_parse <- function(x) do.call("c", lapply(x, crul_head_parse))

webmockr_crul_fetch <- function(x) {
  if (is.null(x$disk) && is.null(x$stream)) {
    curl::curl_fetch_memory(x$url$url, handle = x$url$handle)
  }
  else if (!is.null(x$disk)) {
    curl::curl_fetch_disk(x$url$url, x$disk, handle = x$url$handle)
  }
  else {
    curl::curl_fetch_stream(x$url$url, x$stream, handle = x$url$handle)
  }
}

# modified from purrr:::has_names
along_rep <- function(x, y) rep(y, length.out = length(x))
hz_namez <- function(x) {
  nms <- names(x)
  if (is.null(nms)) {
    along_rep(x, FALSE)
  }
  else {
    !(is.na(nms) | nms == "")
  }
}
