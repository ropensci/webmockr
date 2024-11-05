errors_to_skip_stub_removal <- function() {
  mssgs <- c(
    "\".data\" is missing",
    "must be of class StubbedRequest",
    "not registered",
    "Unknown"
    # ,
    # "all objects must be error classes"
  )
  paste0(mssgs, collapse = "|")
}

stub_removal_message <- c(
  "Encountered an error constructing stub",
  "Removed stub",
  "To see a list of stubs run stub_registry()"
)

#' Handle stub removal
#' @keywords internal
#' @param .data an object of class `StubbedRequest` required
#' @param code a code block. required
#' @return if no error, the result of running `code`; if an error occurs
#' [withCallingHandlers()] throws a warning and then the stub is removed
handle_stub_removal <- function(.data, code) {
  withCallingHandlers({
    force(code)
  },
  error = function(cnd) {
    if (!grepl(errors_to_skip_stub_removal(), cnd$message)) {
      warn(stub_removal_message)
      remove_request_stub(.data)
    }
  })
}

# FIXME: add envir handling so that error message says the exported user fxn that the error occurred in
