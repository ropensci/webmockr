webmockr 0.3.0
==============

### MINOR IMPROVEMENTS

* returned mocked response headers were retaining case that the user gave - whereas they should be all lowercased to match the output in `crul` and `httr`. now fixed. (#49) thanks @hlapp
* returned mocked response headers were not all of character class, but depended on what class was given by the user on creating the stub. this is now fixed, returning all character class values for response headers (#48) thanks @hlapp
* skip tests that require `vcr` if `vcr` is not available (#53)
* internal change to crul adapter to produce the same http response as a new version of crul returns - adds a `response_headers_all` slot  (#51) (#54)


webmockr 0.2.9
==============

### MINOR IMPROVEMENTS

* make `request_registry()` and `stub_registry()` print methods more similar to avoid confusion for users (#35)
* update docs for `enable`/`disable` to indicate that `crul` and `httr` supported (#46) (related to #45)
* wrap httr adapter examples in `requireNamespace` so only run when httr available
* clean up `.onLoad` call, removing commented out code, and add note about creating adapter objects does not load crul and httr packages

### BUG FIXES

* fix to `enable()` and `disable()` methods. even though `httr` is in Suggests, we were loading all adapters (crul, httr) with `stop` when the package was not found. We now give a message and skip when a package not installed. In addition, we `enable()` and `disable()` gain an `adapter` parameter to indicate which package you want to enable or disable. If `adapter` not given we attempt all adapters. Note that this bug shouldn't have affected `vcr` users as `httr` is in Imports in that package, so you'd have to have `httr` installed   (#45) thanks to @maelle for uncovering the problem


webmockr 0.2.8
==============

### NEW FEATURES

* Added support for integration with package `httr`; see `HttrAdapter` for the details; `webmockr` now integrates with two HTTP R packages: `crul` and `httr` (#43) (#44)
* Along with `httr` integration is a new method `httr_mock()` to turn on mocking for `httr`; and two methods `build_httr_response` and `build_httr_request` meant for internal use


webmockr 0.2.6
==============

### NEW FEATURES

* Added support for integration with package `vcr` (now on CRAN) for doing HTTP request caching


webmockr 0.2.4
==============

### NEW FEATURES

* New function `enabled()` to ask if `webmockr` is enabled, gives a
boolean
* `wi_th()` gains new parameter `.list` as an escape hatch to avoid
NSE. examples added in the `wi_th` man file to clarify its use

### MINOR IMPROVEMENTS

* matching by request body was not supported, it now is; added examples
of matching on request body, see `?stub_request`  (#36)
* make sure that the adapter for `crul` handles all types of matches (#29)
* removed all internal usage of pipes in the package. still exporting
pipe for users (#30)
* fixed internals to give vcr error when vcr loaded - for future release
with vcr support (#34)
* require newest `crul` version

### BUG FIXES

* Error messages with the suggest stub were not giving bodies. They 
now give bodies if needed along with method, uri, headers, query (#37)
* Fixed `Response` class that was not dealing with capitalization 
correctly


webmockr 0.2.0
==============

### NEW FEATURES

* New function `to_raise()` to say that a matched response should return a certain exception, currently `to_raise` accepts error classes from the `fauxpas` package (#9)
* New function `to_timeout()` to say that a matched response should return a timeout. This is a special case of `to_raise` to easily do a timeout expectation (#11)
* New function `request_registry()` to list requests in the request registry (#23)
* package `crul` moved to Imports from Suggests as it's the only http client supported for now. will move back to Suggests once we support at least one other http client
* `webmockr_configure()` changes: `turn_on` has been removed; `allow_net_connect` and `allow_localhost` were ignored before, but are now used and are now set to `FALSE` by default; fixed usage of `allow` which now accepts character vector of URLs instead of a boolean; the following correctly marked as being ignored for now until fixed `net_http_connect_on_start`, `show_stubbing_instructions`, `query_values_notation`, `show_body_diff` (#19) (#21)
* `webmockr_disable_net_connect()` now accepts an `allow` parameter to disable all other connections except those URLs given in `allow`
* `webmockr_net_connect_allowed()` now accepts a `uri` parameter to test if a URI/URL is allowed

### MINOR IMPROVEMENTS

* Fixed printed stub statement when printed to the console - we weren't including headers accurately (#18)
* Added examples to the `stub_registry()` and `stub_registry_clea()` manual files (#24)
* internal methods `build_crul_request` and `build_crul_response` moved outside of the `CrulAdapter` class so that they can be accesed like `webmockr::` in other packages
* `enable()` and `disable()` now return booleans invisibly
* General improvements to documentation throughout
* Added linting of user inputs to the `to_return()` method, and docs details on what to input to the method
* Added linting of user inputs to the `wi_th()` method, and docs details on what to input to the method

### BUG FIXES

* Fixed option `allow_localhost`, which wasn't actually workin before (#25)

### DEPRECATED AND DEFUNCT

* `webmockr_enable()` and `webmockr_disable` are now defunct. Use `webmockr::enable()` and `webmockr::disable()` instead



webmockr 0.1.0
==============

### NEW FEATURES

* Released to CRAN.
