#' Response class
#'
#' @export
#' @param options (list) a list of options
#' @details
#' **Methods**
#'   \describe{
#'     \item{`set_request_headers(headers)`}{
#'       set request headers
#'       - headers: a list of key-value pair headers
#'     }
#'     \item{`get_request_headers()`}{
#'       get request headers
#'     }
#'     \item{`set_response_headers(headers)`}{
#'       set response headers
#'       - headers: a list of key-value pair headers
#'     }
#'     \item{`get_response_headers()`}{
#'       get response headers
#'     }
#'     \item{`set_body(body)`}{
#'       - body: must be a string
#'     }
#'     \item{`get_body()`}{
#'       get body
#'     }
#'     \item{`set_status()`}{
#'       - body: must be an integer status code
#'     }
#'     \item{`get_status()`}{
#'       get status code
#'     }
#'     \item{`set_exception()`}{
#'       set exception
#'     }
#'     \item{`get_exception()`}{
#'       get exception
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' (x <- Response$new())
#'
#' x$set_url("https://httpbin.org/get")
#' x
#'
#' x$set_request_headers(list('Content-Type' = "application/json"))
#' x
#' x$request_headers
#'
#' x$set_response_headers(list('Host' = "httpbin.org"))
#' x
#' x$response_headers
#'
#' x$set_status(404)
#' x
#' x$get_status()
#'
#' x$set_body("hello world")
#' x
#' x$get_body()
#'
#' x$set_exception("exception")
#' x
#' x$get_exception()
#' }
Response <- R6::R6Class(
  'Response',
  public = list(
    url = NULL,
    body = NULL,
    content = NULL,
    request_headers = NULL,
    response_headers = NULL,
    options = NULL,
    status_code = 200,
    exception = NULL,
    should_timeout = NULL,

    initialize = function(options = list()) {
      if (inherits(options, "file") || inherits(options, "character")) {
        self$options <- read_raw_response(options)
      } else {
        self$options <- options
      }
    },

    print = function(x, ...) {
      cat("<webmockr response> ", sep = "\n")
      cat(paste0("  url: ", self$url), sep = "\n")
      cat(paste0("  status: ", self$status_code), sep = "\n")
      cat("  headers: ", sep = "\n")
      for (i in seq_along(self$request_headers)) {
        cat("    request headers: ", sep = "\n")
        cat(paste0("     ",
            paste(names(self$request_headers)[i], self$request_headers[[i]],
                  sep = ": ")), sep = "\n")
      }
      for (i in seq_along(self$response_headers)) {
        cat("    response headers: ", sep = "\n")
        cat(paste0("     ",
            paste(names(self$response_headers)[i], self$response_headers[[i]],
                   sep = ": ")), sep = "\n")
      }
      cat(paste0("  exception: ", self$exception), sep = "\n")
      cat(paste0("  body: ", self$body), sep = "\n")
    },

    set_url = function(url) {
      self$url <- url
    },
    get_url = function() self$url,

    set_request_headers = function(headers) {
      self$request_headers <- private$normalize_headers(headers)
    },
    get_request_headers = function() self$request_headers,

    set_response_headers = function(headers) {
      self$response_headers <- private$normalize_headers(headers)
    },
    get_respone_headers = function() self$response_headers,

    set_body = function(body) {
      self$body <- body
    },
    get_body = function() self$body %||% '',

    set_status = function(status) {
      self$status_code <- status
    },
    get_status = function() self$status_code %||% 200,

    set_exception = function(exception) {
      self$exception <- exception
    },
    get_exception = function() self$exception
  ),

  private = list(
    normalize_headers = function(x) normalize_headers(x)
  )
)

# class ResponseFactory
#   def self.response_for(options)
#     if options.respond_to?(:call)
#       WebMock::DynamicResponse.new(options)
#     else
#       WebMock::Response.new(options)
#     end
#   end
# end

# class Response
#   def initialize(options = {})
#     if options.is_a?(IO) || options.is_a?(String)
#       self.options = read_raw_response(options)
#     else
#       self.options = options
#     end
#   end

#   def headers
#     @headers
#   end

#   def headers=(headers)
#     @headers = headers
#     if @headers && !@headers.is_a?(Proc)
#       @headers = Util::Headers.normalize_headers(@headers)
#     end
#   end

#   def body
#     @body || ''
#   end

#   def body=(body)
#     @body = body
#     assert_valid_body!
#     stringify_body!
#   end

#   def status
#     @status || [200, ""]
#   end

#   def status=(status)
#     @status = status.is_a?(Integer) ? [status, ""] : status
#   end

#   def exception
#     @exception
#   end

#   def exception=(exception)
#     @exception = case exception
#     when String then StandardError.new(exception)
#     when Class then exception.new('Exception from WebMock')
#     when Exception then exception
#     end
#   end

#   def raise_error_if_any
#     raise @exception if @exception
#   end

#   def should_timeout
#     @should_timeout == true
#   end

#   def options=(options)
#     options = WebMock::Util::HashKeysStringifier.stringify_keys!(options)
#     HashValidator.new(options).validate_keys('headers', 'status', 'body', 'exception', 'should_timeout')
#     self.headers = options['headers']
#     self.status = options['status']
#     self.body = options['body']
#     self.exception = options['exception']
#     @should_timeout = options['should_timeout']
#   end

#   def evaluate(request_signature)
#     self.body = @body.call(request_signature) if @body.is_a?(Proc)
#     self.headers = @headers.call(request_signature) if @headers.is_a?(Proc)
#     self.status = @status.call(request_signature) if @status.is_a?(Proc)
#     @should_timeout = @should_timeout.call(request_signature) if @should_timeout.is_a?(Proc)
#     @exception = @exception.call(request_signature) if @exception.is_a?(Proc)
#     self
#   end

#   def ==(other)
#     self.body == other.body &&
#     self.headers === other.headers &&
#     self.status == other.status &&
#     self.exception == other.exception &&
#     self.should_timeout == other.should_timeout
#   end

#   private

#   def stringify_body!
#     if @body.is_a?(IO) || @body.is_a?(Pathname)
#       io = @body
#       @body = io.read
#       io.close if io.respond_to?(:close)
#     end
#   end

#   def assert_valid_body!
#     valid_types = [Proc, IO, Pathname, String, Array]
#     return if @body.nil?
#     return if valid_types.any? { |c| @body.is_a?(c) }
#     raise InvalidBody, "must be one of: #{valid_types}. '#{@body.class}' given"
#   end

#   def read_raw_response(raw_response)
#     if raw_response.is_a?(IO)
#       string = raw_response.read
#       raw_response.close
#       raw_response = string
#     end
#     socket = ::Net::BufferedIO.new(raw_response)
#     response = ::Net::HTTPResponse.read_new(socket)
#     transfer_encoding = response.delete('transfer-encoding') #chunks were already read by curl
#     response.reading_body(socket, true) {}

#     options = {}
#     options[:headers] = {}
#     response.each_header {|name, value| options[:headers][name] = value}
#     options[:headers]['transfer-encoding'] = transfer_encoding if transfer_encoding
#     options[:body] = response.read_body
#     options[:status] = [response.code.to_i, response.message]
#     options
#   end

#   InvalidBody = Class.new(StandardError)

# end

# class DynamicResponse < Response
#   attr_accessor :responder

#   def initialize(responder)
#     @responder = responder
#   end

#   def evaluate(request_signature)
#     options = @responder.call(request_signature)
#     Response.new(options)
#   end
# end
