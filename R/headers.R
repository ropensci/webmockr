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

# class Headers

#   def self.sorted_headers_string(headers)
#     headers = WebMock::Util::Headers.normalize_headers(headers)
#     str = '{'
#     str << headers.map do |k,v|
#       v = case v
#         when Regexp then v.inspect
#         when Array then "["+v.map{|w| "'#{w.to_s}'"}.join(", ")+"]"
#         else "'#{v.to_s}'"
#       end
#       "'#{k}'=>#{v}"
#     end.sort.join(", ")
#     str << '}'
#   end

#   def self.decode_userinfo_from_header(header)
#     header.sub(/^Basic /, "").unpack("m").first
#   end

#   def self.basic_auth_header(*credentials)
#     "Basic #{Base64.strict_encode64(credentials.join(':')).chomp}"
#   end

# end
