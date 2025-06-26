webmockr (2.1.0)
=========

## CHANGES

* Many previously exported R6 classes that are internal use only are no longer exported
* Five other functions (four prefixed with `build_`, and `pluck_body`) are no longer exported, internal use only

## MINOR IMPROVEMENTS

* Update tests to use testthat edition 3, and run in parallel
* Moved `crul` from Imports to Suggests


webmockr 2.0.0
==============

## BREAKING CHANGES

* Previous to this version when stubs were constructed starting with `stub_request()` if an error occurred in a pipe chain, or non-pipe flow, the stub prior to the error remained. This was not correct behavior from a logical perspective - i.e., one would expect if an error occurred that thing they were trying to do did not stick around. The new behavior as of this version deletes the stub upon any error during its creation. Under the hood we're using `withCallingHandlers` to handle different types of errors, throw warnings, etc.

## NEW FEATURES

* Partial matching. New functions `including()` and `excluding()` for use with `wi_th()` support partial for bodies and queries (header partial matching was already supported without any additional steps). See `?partial`. This makes it slightly to a whole lot easier to do matching depending on the HTTP request your trying to match (e.g., let's say you're trying to match against a query with 20 parameters - if you can match uniquely to it with 1 or 2 of those params, then you're all set) (#38)
* Basic auth internal work for `RequestPattern`. Shouldn't change behavior (#133)
* New features for supporting request body diffs. There are two ways to use request body diffing. First, you can toggle it on/off globally like `webmockr_configure(show_body_diff = TRUE)` or `webmockr_configure(show_body_diff = FALSE)`. Second, a new function `stub_body_diff()` is a standalone function that compares by default the last stub created and the last http request made - but you can pass in any stub and http request. Note that body diffing functionality requires the suggested package `diffobj` (#126)
* As part of the above body diffing functionality, two new functions are offered: `last_request()` and `last_stub()`, which get the last http request made and the last webmockr stub created, respectively.  (#126)

## MINOR IMPROVEMENTS

* Removed `global_stubs` field from the `StubRegistry` class as it was completely unused (holdover from the initial port from Ruby). Should not impact users at all. (#127)
* Wider use of `rlang` functions throughout the package for nicer assertions and condition handling. This change alters the main error message you get when there's no match to registered stubs. Hopefully this feels like an improvement to you; let me know. (#129)
* `StubRegistry` gains new method `is_stubbed()` to check if a stub is in the stub registry


webmockr 1.0.0
==============

## NEW FEATURES

* `webmockr` now supports the `httr2` library, in addition to `httr` and `crul`. Note that you'll see different behavior from `httr2` relative to the other 2 http clients because it turns http errors (http statuses 400 and above) into R errors (#122)
* `webmockr` can now mock async http requests with `crul` (w/ `crul` v1.5 or greater). no change was required in `webmockr` for this to happen. a PR was merged in `crul` to hook into `webmockr`. there's no support for async in `httr` as that package does not do any async and no support in `httr2` because `req_perform_parallel` does not have a mocking hook as does `req_perform` (#124)


webmockr 0.9.0
==============

## BUG FIXES

* `to_return()` supports returning multiple responses to match many requests to the same matching stub. however, the internals were broken for this, but is now fixed  (#115) thanks @kenahoo for the report
* matching stubs with specifying a request body to match on (e.g., `stub_request('post', 'https://httpbin.org/post') %>% wi_th(body = list(a=5))`) was not working in some cases; internal matching logic was borked. now fixed. (#118) thanks @konradoberwimmer for the report
* The `status` parameter in `to_return()` was documented to accept an integer, but it errored when an integer was passed (e.g., `to_return(status=200L)`). This bug is now fixed  (#117) thanks @maelle for the report

## MINOR IMPROVEMENTS

* Config options changes (see `webmockr_configure()`). Three options that were present but not implemented are now removed: `show_body_diff`, ` query_values_notation`, ` net_http_connect_on_start`. One option that was present but not implemented yet is now implemented: ` show_stubbing_instructions` (#27) (#120)

## DOCUMENTATION

* `StubCounter` added to pkgdown docs page at <https://docs.ropensci.org/webmockr/reference/StubCounter.html> (#119) @maelle


webmockr 0.8.2
==============

## BUG FIXES

* change to `UriPattern` to make sure regex matching is working as intended (#114) thanks @kenahoo


webmockr 0.8.0
==============

## NEW FEATURES

* `enable()` and the `enable()` method on the `Adapter` R6 class gain new parameter `quiet` to toggle whether messages are printed or not  (#112)

## MINOR IMPROVEMENTS

* to re-create http response objects for both httr and crul we were using the url from the request object; now we use the url from the response object, BUT if there is no url in the response object we fall back to using the url from the request object (#110) (#113)
* improve docs: add further explanation to manual files for both `to_raise()` and `to_return()` to explain the difference between them and when you may want to use them (#100)


webmockr 0.7.4
==============

## MINOR IMPROVEMENTS

* to support vcr being able to recreate httr objects fully (see github issue ropensci/vcr#132) we needed to handle additional parts of httr request objects: fields and output - with this change vcr should return objects much closer to what real httr requests return (#109)

## BUG FIXES

* bug fix + improvement: fixes for simple authentication - `wi_th()` now supports `basic_auth` to mock basic authentication either with `crul::auth()` or `httr::authenticate()` (#108)


webmockr 0.7.0
==============

## NEW FEATURES

* Gains ability to define more than 1 returned HTTP response, and the order in which the HTTP responses are returned. The idea is from the Ruby webmock library, but the implementation is different because the Ruby and R languages are very different. You can give more than one `to_return()` one creating a stub, or if you want to return the same response each time, you can use the new `times` parameter within `to_return()`. As a related use case (#31) you can mock http retry's using this new feature (#10) (#32) (#101)
* Gains new function `webmockr_reset()` to be able to reset stub registry and request registry in one function call (#97) (#101)
* Gains support for mocking simple authentication. `wi_th()` now accepts `basic_auth` in addition to query, body, and headers. Note that authentication type is ignored (#103)

## MINOR IMPROVEMENTS

* change to how URI's are matched in `stub_request()`: we weren't allowing matching URI's without schemes; you can now do that. In addition, webmockr can match URI's without the "http" scheme, but does not match if the scheme is "https". See `UriPattern` for more (#102)
* another change to how URI's are matched: now query params compared separately to the URI; note that regex not allowed in query params (#104) - And now query parameters are compared with the same code both when regex uri is used and when it is not (#107)
* URI matching for stubs is now done only on the URI's themselves; that is, query parameters are removed before comparison, so only the base url with http scheme, plus paths, are compared (#107)
* wasn't sure `write_disk_path` behavior was correct when using httr, seems to be working, added tests for it (#79)
* values for query parameters given to `wi_th()` are now all coerced to character class to make sure that all comparisons of stubs and requests are done with the same class (character) (#107)

## BUG FIXES

* fix for `uri_regex` usage in `stub_request()`: no longer curl escape the `uri_regex` given, only escape a non-regex uri (#106)


webmockr 0.6.2
==============

* change to `CrulAdapter`: do not use `normalizePath` on the `write_disk_path` path so that relative paths are not changed to full paths - added tests for this (#95) (#96)


webmockr 0.6.0
==============

## NEW FEATURES

* new `Adapter` class to consolidate common code for the `HttrAdapter` and `CrulAdapter` classes, which inherit from `Adapter`; not a user facing change (#87)
* pkgdown documentation site gains grouping of functions to help the user navigate the package: see https://docs.ropensci.org/webmockr/reference/ (#93)

## MINOR IMPROVEMENTS

* now correctly fails with informative message when `write_disk_path` is `NULL` when the user is trying to write to disk while using webmockr (#78)
* improve README construction; use html child for the details section (#81)
* fix matching stub matching for bodies when bodies are JSON encoded (#82)
* when vcr was loaded real HTTP requests were being performed twice when they should have only been performed once (#91) (#92)

## BUG FIXES

* fix for `set_body()` method in the `Response` class - handle cases where user writing to disk and not, and handle raw bytes correctly (#80)
* fix to `to_s()` method in `StubbedRequest` class - was formatting query parameters incorrectly (#83)
* fix to `BodyPattern` class to handle upload objects in a list; related issue fixed where `wi_th()` parameter `body` was not handling upload objects (#84) (#85)
* httr requests were failing when vcr loaded, but with no cassette inserted; fixed `handle_request()` to skip vcr-related code unless a cassette is inserted (#86) (#88)


webmockr 0.5.0
==============

## NEW FEATURES

* `webmockr` now supports mocking writing to disk. TLDR: see `?mocking-disk-writing` to get started - That is, both of the major high level http clients in R, crul and httr, support writing directly to disk (rather than the user manually getting the http response and writing it to disk). supporting this required quite a bit of work, both in code and in thinking about how to support the various scenarios in which users can find themselves when dealing with writing to disk - Please get in touch if you have problems with this (#57) (#76)
* gains `request_registry_clear()` method to easily clear all requests in the request registry (#75)

## MINOR IMPROVEMENTS

* better docs for R6 classes with R6 support in new roxygen2 version on cran (#77)
* httr simple auth was being ignored - its now supported (simple auth with crul already worked) (#74)

## BUG FIXES

* fix to handle raw responses that can not be converted to character, such as images; needed due to issue https://github.com/ropensci/vcr/issues/112 (#72) (#73)


webmockr 0.4.0
==============

## MINOR IMPROVEMENTS

* fix link to http testing book, change ropensci to ropenscilabs (#67)
* fixes to request matching: single match types working now (e.g., just match on query, or just on headers); in addition, header matching now works; added examples of single match types (#68) (#69)

## BUG FIXES

* fix stub specification within crul and httr adapters; typo in setting headers (#70)


webmockr 0.3.4
==============

## DEFUNCT

* underscore methods `to_return_()` and `wi_th_()` are defunct (#60) (#64)

## NEW FEATURES

* `to_return()` gains parameter `.list` (#60) (#64)

## MINOR IMPROVEMENTS

* typo fixes (#62) thanks @Bisaloo !
* improved the print method for stubs, found in `StubbedRequest`, to have better behavior for very long strings such as in headers and bodies (#63)

## BUG FIXES

* fix date in mocked `httr` response object to match the date format that `httr` uses in real HTTP requests (#58) (#61) via <https://github.com/ropensci/vcr/issues/91>
* fix response headers in mocked `httr` response objects. `httr` makes the list of headers insensitive to case, so we now use that function from the package (#59) (#61)
* `to_return()` and `wi_th()` drop use of the `lazyeval` package and fall back to using the simple `list(...)` - fixes problem where creating stubs was failing within `test_that()` blocks due to some weird lazy eval conflicts (i think) (#60) (#64) thanks @karawoo !


webmockr 0.3.0
==============

## MINOR IMPROVEMENTS

* returned mocked response headers were retaining case that the user gave - whereas they should be all lowercased to match the output in `crul` and `httr`. now fixed. (#49) thanks @hlapp
* returned mocked response headers were not all of character class, but depended on what class was given by the user on creating the stub. this is now fixed, returning all character class values for response headers (#48) thanks @hlapp
* skip tests that require `vcr` if `vcr` is not available (#53)
* internal change to crul adapter to produce the same http response as a new version of crul returns - adds a `response_headers_all` slot  (#51) (#54)


webmockr 0.2.9
==============

## MINOR IMPROVEMENTS

* make `request_registry()` and `stub_registry()` print methods more similar to avoid confusion for users (#35)
* update docs for `enable`/`disable` to indicate that `crul` and `httr` supported (#46) (related to #45)
* wrap httr adapter examples in `requireNamespace` so only run when httr available
* clean up `.onLoad` call, removing commented out code, and add note about creating adapter objects does not load crul and httr packages

## BUG FIXES

* fix to `enable()` and `disable()` methods. even though `httr` is in Suggests, we were loading all adapters (crul, httr) with `stop` when the package was not found. We now give a message and skip when a package not installed. In addition, we `enable()` and `disable()` gain an `adapter` parameter to indicate which package you want to enable or disable. If `adapter` not given we attempt all adapters. Note that this bug shouldn't have affected `vcr` users as `httr` is in Imports in that package, so you'd have to have `httr` installed   (#45) thanks to @maelle for uncovering the problem


webmockr 0.2.8
==============

## NEW FEATURES

* Added support for integration with package `httr`; see `HttrAdapter` for the details; `webmockr` now integrates with two HTTP R packages: `crul` and `httr` (#43) (#44)
* Along with `httr` integration is a new method `httr_mock()` to turn on mocking for `httr`; and two methods `build_httr_response` and `build_httr_request` meant for internal use


webmockr 0.2.6
==============

## NEW FEATURES

* Added support for integration with package `vcr` (now on CRAN) for doing HTTP request caching


webmockr 0.2.4
==============

## NEW FEATURES

* New function `enabled()` to ask if `webmockr` is enabled, gives a
boolean
* `wi_th()` gains new parameter `.list` as an escape hatch to avoid
NSE. examples added in the `wi_th` man file to clarify its use

## MINOR IMPROVEMENTS

* matching by request body was not supported, it now is; added examples
of matching on request body, see `?stub_request`  (#36)
* make sure that the adapter for `crul` handles all types of matches (#29)
* removed all internal usage of pipes in the package. still exporting
pipe for users (#30)
* fixed internals to give vcr error when vcr loaded - for future release
with vcr support (#34)
* require newest `crul` version

## BUG FIXES

* Error messages with the suggest stub were not giving bodies. They
now give bodies if needed along with method, uri, headers, query (#37)
* Fixed `Response` class that was not dealing with capitalization
correctly


webmockr 0.2.0
==============

## NEW FEATURES

* New function `to_raise()` to say that a matched response should return a certain exception, currently `to_raise` accepts error classes from the `fauxpas` package (#9)
* New function `to_timeout()` to say that a matched response should return a timeout. This is a special case of `to_raise` to easily do a timeout expectation (#11)
* New function `request_registry()` to list requests in the request registry (#23)
* package `crul` moved to Imports from Suggests as it's the only http client supported for now. will move back to Suggests once we support at least one other http client
* `webmockr_configure()` changes: `turn_on` has been removed; `allow_net_connect` and `allow_localhost` were ignored before, but are now used and are now set to `FALSE` by default; fixed usage of `allow` which now accepts character vector of URLs instead of a boolean; the following correctly marked as being ignored for now until fixed `net_http_connect_on_start`, `show_stubbing_instructions`, `query_values_notation`, `show_body_diff` (#19) (#21)
* `webmockr_disable_net_connect()` now accepts an `allow` parameter to disable all other connections except those URLs given in `allow`
* `webmockr_net_connect_allowed()` now accepts a `uri` parameter to test if a URI/URL is allowed

## MINOR IMPROVEMENTS

* Fixed printed stub statement when printed to the console - we weren't including headers accurately (#18)
* Added examples to the `stub_registry()` and `stub_registry_clea()` manual files (#24)
* internal methods `build_crul_request` and `build_crul_response` moved outside of the `CrulAdapter` class so that they can be accessed like `webmockr::` in other packages
* `enable()` and `disable()` now return booleans invisibly
* General improvements to documentation throughout
* Added linting of user inputs to the `to_return()` method, and docs details on what to input to the method
* Added linting of user inputs to the `wi_th()` method, and docs details on what to input to the method

## BUG FIXES

* Fixed option `allow_localhost`, which wasn't actually workin before (#25)

## DEPRECATED AND DEFUNCT

* `webmockr_enable()` and `webmockr_disable` are now defunct. Use `webmockr::enable()` and `webmockr::disable()` instead



webmockr 0.1.0
==============

## NEW FEATURES

* Released to CRAN.
