wbenv <- new.env()
bucket <- new.env()

start_server <- function(x) {
  app <- list(
    call = function(req) {
      wsUrl = paste(sep = '',
                    '"',
                    "ws://",
                    ifelse(is.null(req$HTTP_HOST), req$SERVER_NAME, req$HTTP_HOST),
                    '"')

      tmp <- list(
        status = 200L,
        headers = list(
          'Content-Type' = 'application/json'
        ),
        body = sprintf('{
          "http_method": "%s",
          "url": "%s",
          "port": "%s",
          "query": "%s",
          "user_agent": "%s"
        }', req$REQUEST_METHOD, req$SERVER_NAME,
        req$SERVER_PORT, req$QUERY_STRING, req$HTTP_USER_AGENT)
      )
      assign(basename(tempfile()), tmp, envir = bucket)
      tmp
    }
  )
  #wbenv$server <- startDaemonizedServer("0.0.0.0", 9200, app)
  wbenv$server <- startDaemonizedServer("80", 9200, app)
  message("server started")
}

stop_server <- function(x = NULL) {
  stopDaemonizedServer(if (is.null(x)) wbenv$server else x)
}

bucket_list <- function(x) ls(envir = bucket)

bucket_unique <- function(x) {
  hashes <- vapply(ls(envir = bucket), function(z) digest::digest(get(z, envir = bucket)), "")
  if (any(duplicated(hashes))) {
    torm <- names(hashes)[duplicated(hashes)]
    invisible(lapply(torm, function(z) rm(list = z, envir = bucket)))
  }
}
