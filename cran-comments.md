## Test environments

* local OS X install, R 3.5.2 Patched
* ubuntu 14.04 (on travis-ci), R 3.5.2
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

  License components with restrictions and base license permitting such:
    MIT + file LICENSE
  File 'LICENSE':
    YEAR: 2019
    COPYRIGHT HOLDER: Scott Chamberlain

## Reverse dependencies

I have checked the 8 reverse dependencies, and there were no problems. 
See (<https://github.com/ropensci/webmockr/tree/master/revdep>).

---

This version contains fixes for returned mocked response headers, matches mocked responses to new crul version responses, and skips tests that require vcr if vcr is not available on the system.

Thanks!
Scott Chamberlain
