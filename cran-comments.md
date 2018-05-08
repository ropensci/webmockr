## Test environments

* local OS X install, R 3.5.0
* ubuntu 12.04 (on travis-ci), R 3.5.0
* win-builder (devel and release)

## R CMD check results

0 errors | 0 warnings | 1 note

  License components with restrictions and base license permitting such:
    MIT + file LICENSE
  File 'LICENSE':
    YEAR: 2018
    COPYRIGHT HOLDER: Scott Chamberlain

## Reverse dependencies

I have checked the 2 reverse dependencies, and there were no problems. 
See (<https://github.com/ropensci/webmockr/tree/master/revdep#platform>).
I have notified the one other maintainer.

---

This version gains a new function, adds a parameter to the wi_th function, 
fixes matching by request body, prints request body in suggested stub, and 
a few other small fixes.

Thanks!
Scott Chamberlain
