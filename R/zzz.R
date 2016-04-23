http_verbs <- c("get","post","put","patch","head","delete")

cc <- function(x) Filter(Negate(is.null), x)

hdl_lst <- function(x) {
  if (is.null(x) || length(x) == 0) return("")
  if (inherits(x, "list")) {
    return(paste(names(x), unname(x), sep = "=", collapse = ", "))
  } else {
    x
  }
}
