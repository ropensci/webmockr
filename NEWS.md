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
