# headers <- list(`Content-type` = 'application/json', Stuff = "things")
# normalize_headers(x = headers)
#
# headers <- list(`Content-type` = 'application/json', `x-frame-options` = c("SAMEORIGIN", "sameorigin"))
# normalize_headers(x = headers)
normalize_headers <- function(x = NULL) {
  if (is.null(x) || length(x) == 0) return(x)

  res <- list()
  for (i in seq_along(x)) {
    name <- paste0(
      vapply(strsplit(as.character(names(x)[i]), '_|-')[[1]], function(w) simple_cap(w), ""),
      collapse = "-"
    )
    value <- switch(
      class(x[[i]]),
      list = if (length(x[[i]]) == 1) x[[i]][[1]] else sort(vapply(x[[i]], function(z) as.character(z), "")),
      if (length(x[[i]]) > 1) paste0(as.character(x[[i]]), collapse = ",") else as.character(x[[i]])
    )
    res[[i]] <- list(name, value)
  }

  unlist(lapply(res, function(z) stats::setNames(z[2], z[1])), FALSE)
}

simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
