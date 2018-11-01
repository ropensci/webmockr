## Test environments

* local OS X install, R 3.5.1 Patched
* ubuntu 14.04 (on travis-ci), R 3.5.1
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

  License components with restrictions and base license permitting such:
    MIT + file LICENSE
  File 'LICENSE':
    YEAR: 2018
    COPYRIGHT HOLDER: Scott Chamberlain

## Reverse dependencies

I have checked the 8 reverse dependencies, and there were no problems. 
See (<https://github.com/ropensci/webmockr/tree/master/revdep>).

---

This version fixes a bug related to when the httr package is not available; and some other minor changes.

Thanks!
Scott Chamberlain
