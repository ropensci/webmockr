match <- function(uri, processor = NULL) {
  uri <- urltools::url_parse(uri)
  mapping <- list()

  expansions <- parse_template_pattern(pattern, processor)
  expansions_regexp <- parse_template_pattern(pattern, processor)


}

parse_template_pattern <- function(pattern, processor = NULL) {
  pattern <- escape_for_regex(pattern)
  gregexpr("\\\\{(.*?)\\\\}", pattern, perl = TRUE)
  #gsub("\\\\{(.*?)\\\\}", "", escape_for_regex(pattern), perl = TRUE)
}

# from .rs.escapeForRegex
escape_for_regex <- function (regex) {
  gsub("([\\-\\[\\]\\{\\}\\(\\)\\*\\+\\?\\.\\,\\\\\\^\\$\\|\\#\\s])",
       "\\\\\\1", regex, perl = TRUE)
}

def parse_template_pattern(pattern, processor=nil)
    # Escape the pattern. The two gsubs restore the escaped curly braces
    # back to their original form. Basically, escape everything that isn't
    # within an expansion.
    escaped_pattern = Regexp.escape(
      pattern
    ).gsub(/\\\{(.*?)\\\}/) do |escaped|
      escaped.gsub(/\\(.)/, "\\1")
    end

    expansions = []

    # Create a regular expression that captures the values of the
    # variables in the URI.
    regexp_string = escaped_pattern.gsub( EXPRESSION ) do |expansion|

      expansions << expansion
      _, operator, varlist = *expansion.match(EXPRESSION)
      leader = Regexp.escape(LEADERS.fetch(operator, ''))
      joiner = Regexp.escape(JOINERS.fetch(operator, ','))
      combined = varlist.split(',').map do |varspec|
        _, name, modifier = *varspec.match(VARSPEC)

        result = processor && processor.respond_to?(:match) ? processor.match(name) : nil
        if result
          "(?<#{name}>#{ result })"
        else
          group = case operator
          when '+'
            "#{ RESERVED }*?"
          when '#'
            "#{ RESERVED }*?"
          when '/'
            "#{ UNRESERVED }*?"
          when '.'
            "#{ UNRESERVED.gsub('\.', '') }*?"
          when ';'
            "#{ UNRESERVED }*=?#{ UNRESERVED }*?"
          when '?'
            "#{ UNRESERVED }*=#{ UNRESERVED }*?"
          when '&'
            "#{ UNRESERVED }*=#{ UNRESERVED }*?"
          else
            "#{ UNRESERVED }*?"
          end
          if modifier == '*'
            "(?<#{name}>#{group}(?:#{joiner}?#{group})*)?"
          else
            "(?<#{name}>#{group})?"
          end
        end
      end.join("#{joiner}?")
      "(?:|#{leader}#{combined})"
    end

    # Ensure that the regular expression matches the whole URI.
    regexp_string = "^#{regexp_string}$"
    return expansions, Regexp.new(regexp_string)
  end

end

# def match(uri, processor=nil)
#   uri = Addressable::URI.parse(uri)
#   mapping = {}
#
#   # First, we need to process the pattern, and extract the values.
#   expansions, expansion_regexp =
#     parse_template_pattern(pattern, processor)
#
#   return nil unless uri.to_str.match(expansion_regexp)
#   unparsed_values = uri.to_str.scan(expansion_regexp).flatten
#
#   if uri.to_str == pattern
#     return Addressable::Template::MatchData.new(uri, self, mapping)
#   elsif expansions.size > 0
#     index = 0
#     expansions.each do |expansion|
#       _, operator, varlist = *expansion.match(EXPRESSION)
#       varlist.split(',').each do |varspec|
#         _, name, modifier = *varspec.match(VARSPEC)
#         mapping[name] ||= nil
#         case operator
#         when nil, '+', '#', '/', '.'
#           unparsed_value = unparsed_values[index]
#           name = varspec[VARSPEC, 1]
#           value = unparsed_value
#           value = value.split(JOINERS[operator]) if value && modifier == '*'
#         when ';', '?', '&'
#           if modifier == '*'
#             if unparsed_values[index]
#               value = unparsed_values[index].split(JOINERS[operator])
#               value = value.inject({}) do |acc, v|
#                 key, val = v.split('=')
#                 val = "" if val.nil?
#                 acc[key] = val
#                 acc
#               end
#             end
#           else
#             if (unparsed_values[index])
#               name, value = unparsed_values[index].split('=')
#               value = "" if value.nil?
#             end
#           end
#         end
#         if processor != nil && processor.respond_to?(:restore)
#           value = processor.restore(name, value)
#         end
#         if processor == nil
#           if value.is_a?(Hash)
#             value = value.inject({}){|acc, (k, v)|
#               acc[Addressable::URI.unencode_component(k)] =
#                 Addressable::URI.unencode_component(v)
#               acc
#             }
#           elsif value.is_a?(Array)
#             value = value.map{|v| Addressable::URI.unencode_component(v) }
#           else
#             value = Addressable::URI.unencode_component(value)
#           end
#         end
#         if !mapping.has_key?(name) || mapping[name].nil?
#           # Doesn't exist, set to value (even if value is nil)
#           mapping[name] = value
#         end
#         index = index + 1
#       end
#     end
#     return Addressable::Template::MatchData.new(uri, self, mapping)
#   else
#     return nil
#   end
# end
