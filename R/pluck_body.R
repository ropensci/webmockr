#' Extract the body from an HTTP request
#' 
#' Returns an appropriate representation of the data contained within a request
#' body based on its encoding.
#' 
#' @export
#' @param x an unexecuted crul *or* httr request object
#' @return one of the following:
#' - `NULL` if the request is not associated with a body
#' - list containing the multipart-encoded body
#' - character vector with the JSON- or raw-encoded body, or upload form file

pluck_body <- function(x) {
  assert_request(x)
  if (is_body_empty(x)) return(NULL)

  # multipart body
  if (!is.null(x$fields)) {
    form_file_comp <- vapply(x$fields, inherits, logical(1), "form_file")
    if (any(form_file_comp)) {
      return(x$fields[form_file_comp][[1]])
    } else {
      return(x$fields)
    }

  # json/raw-encoded body
  } else if (!is.null(x$options$postfields) && is.raw(x$options$postfields)) {
    return(rawToChar(x$options$postfields))
  } else {
    stop("couldn't fetch request body; file an issue at \n",
         "  https://github.com/ropensci/webmockr/issues/",
         call. = FALSE)
  }
}

assert_request <- function(x) {
  request_slots <- c("url", "method", "options", "headers")
  if (!is.list(x) || !all(request_slots %in% names(x))) {
    stop(deparse(substitute(x)), " is not a valid request ", call. = FALSE)
  }
}

is_body_empty <- function(x) {
  is.null(x$fields) &&
    (is.null(x$options$postfieldsize) || x$options$postfieldsize == 0L)
}
