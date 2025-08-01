#' Extract the body from an HTTP request
#'
#' Returns an appropriate representation of the data contained within a request
#' body based on its encoding.
#'
#' @param x an unexecuted crul, httr *or* httr2 request object
#' @export
#' @keywords internal
#' @return one of the following:
#' - `NULL` if the request is not associated with a body
#' - `NULL` if an upload is used not in a list
#' - list containing the multipart-encoded body
#' - character vector with the JSON- or raw-encoded body, or upload form file
pluck_body <- function(x) {
  assert_request(x)
  if (is_body_empty(x)) {
    return(NULL)
  }

  # multipart body
  if (!is.null(x$fields)) {
    return(x$fields)

    # json/raw-encoded body
  } else if (
    has_name(x$options, "postfields") && is.raw(x$options$postfields)
  ) {
    return(rawToChar(x$options$postfields))

    # upload not in a list
  } else if (!is.null(x$options$postfieldsize_large)) {
    return(paste0("upload, file size: ", x$options$postfieldsize_large))

    # unknown, fail out
  } else {
    abort(
      "couldn't fetch request body; file an issue at \n",
      "  https://github.com/ropensci/webmockr/issues/"
    )
  }
}

assert_request <- function(x) {
  request_slots <- c("url", "method", "options", "headers")
  if (!is.list(x) || !all(request_slots %in% names(x))) {
    webmockr_abort(
      format_error("{.arg {deparse(substitute(x))}} is not a valid request")
    )
  }
}

is_body_empty <- function(x) {
  is.null(x$fields) &&
    (!has_name(x$options, "postfieldsize_large") ||
      x$options$postfieldsize_large == 0L) &&
    (!has_name(x$options, "postfieldsize") || x$options$postfieldsize == 0L)
}
