# headers <- list(`Content-type` = 'application/json', Stuff = "things")
# normalize_headers(x = headers)
normalize_headers <- function(x = NULL) {
  if (is.null(x) || length(x) == 0) return(x)
  res <- Map(function(name, value) {
    name <- paste0(
      vapply(strsplit(as.character(name), '_|-')[[1]], function(w) simple_cap(w), ""),
      collapse = "-"
    )
    value <- switch(
      class(value),
      #when Regexp then value
      list = if (length(value) == 1) value[[1]] else sort(vapply(value, function(z) as.character(z), "")),
      as.character(value)
    )
    list(name, value)
  }, names(x), unlist(unname(x)))
  vapply(res, function(z) stats::setNames(z[2], z[1]), list(1))
}

simple_cap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")
}
