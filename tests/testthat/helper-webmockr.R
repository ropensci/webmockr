sm <- function(x) suppressMessages(x)

get_err_mssg <- function(x) {
  tmp <- tryCatch(x, error = function(e) e)
  if (inherits(tmp, "error")) unclass(tmp)$message else tmp
}

# from https://stackoverflow.com/a/14838321/1091766
re_escape <- function(strings){
  vals <- c("\\\\", "\\[", "\\]", "\\(", "\\)",
            "\\{", "\\}", "\\^", "\\$","\\*",
            "\\+", "\\?", "\\.", "\\|")
  replace.vals <- paste0("\\\\", vals)
  for(i in seq_along(vals)){
    strings <- gsub(vals[i], replace.vals[i], strings)
  }
  strings
}
