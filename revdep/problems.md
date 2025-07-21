# archiveRetriever

<details>

* Version: 0.4.0
* GitHub: https://github.com/liserman/archiveRetriever
* Source code: https://github.com/cran/archiveRetriever
* Date/Publication: 2024-06-11 09:40:02 UTC
* Number of recursive dependencies: 77

Run `revdepcheck::revdep_details(, "archiveRetriever")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      nrow(output) not equal to 4.
      1/1 mismatches
      [1] 1 - 4 == -3
      ── Error ('test_scrape_urls.R:687:5'): scrape_urls() returns a data frame ──────
      Error in `names(x) <- value`: 'names' attribute [2] must be the same length as the vector [0]
      Backtrace:
          ▆
       1. ├─vcr::use_cassette(...) at test_scrape_urls.R:686:3
       2. │ └─cassette$call_block(...)
       3. └─archiveRetriever::scrape_urls(...) at test_scrape_urls.R:687:5
       4.   └─base::`colnames<-`(`*tmp*`, value = cnames)
      
      [ FAIL 10 | WARN 8 | SKIP 16 | PASS 61 ]
      Error: Test failures
      Execution halted
    ```

# citecorp

<details>

* Version: 0.3.0
* GitHub: https://github.com/ropenscilabs/citecorp
* Source code: https://github.com/cran/citecorp
* Date/Publication: 2020-04-16 15:20:02 UTC
* Number of recursive dependencies: 45

Run `revdepcheck::revdep_details(, "citecorp")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘test-all.R’
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
      sort(names(x)) not equal to c("doi", "paper").
      Lengths differ: 0 is not 2
      ── Failure ('test-oc_lookup.R:25:3'): oc_doi2ids works ─────────────────────────
      sort(names(y)) not equal to c("doi", "paper").
      Lengths differ: 0 is not 2
      ── Failure ('test-oc_lookup.R:43:3'): oc_pmid2ids works ────────────────────────
      sort(names(x)) not equal to `cols`.
      Lengths differ: 0 is not 4
      ── Failure ('test-oc_lookup.R:61:3'): oc_pmcid2ids works ───────────────────────
      sort(names(x)) not equal to `cols`.
      Lengths differ: 0 is not 4
      
      [ FAIL 15 | WARN 0 | SKIP 0 | PASS 43 ]
      Error: Test failures
      Execution halted
    ```

# magmaR

<details>

* Version: 1.0.4
* GitHub: NA
* Source code: https://github.com/cran/magmaR
* Date/Publication: 2025-05-23 15:02:02 UTC
* Number of recursive dependencies: 106

Run `revdepcheck::revdep_details(, "magmaR")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error ('test-update.R:12:5'): (code run outside of `test_that()`) ───────────
      Error in `.perform_curl_get(fxn = "/retrieve", target, requestBody, parse = TRUE, 
          verbose = verbose)`: You are unauthorized. Update your 'token' input with 'magmaRset()', then retry.
      Backtrace:
          ▆
       1. ├─vcr::use_cassette(...) at test-update.R:11:1
       2. │ └─cassette$call_block(...)
       3. └─magmaR::retrieveMatrix(targ, "example", "rna_seq", "all", "gene_counts") at test-update.R:12:5
       4.   └─magmaR::retrieveIds(target, projectName, modelName)
       5.     └─magmaR:::.retrieve(...)
       6.       └─magmaR:::.perform_curl_get(...)
      
      [ FAIL 14 | WARN 0 | SKIP 0 | PASS 18 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ...
    ```
      ‘Download.Rmd’ using ‘UTF-8’... failed
      ‘Upload.Rmd’ using ‘UTF-8’... failed
     ERROR
    Errors in running code in vignettes:
    when running code in ‘Download.Rmd’
      ...
    
    > library(magmaR)
    
    > prod <- magmaRset(token = TOKEN, url = URL)
    ...
    
    > revs <- list(biospecimen = list(`EXAMPLE-HS1-WB1` = list(biospecimen_type = "Whole Blood"), 
    +     `EXAMPLE-HS2-WB1` = list(biospecimen_type = "Whol ..." ... [TRUNCATED] 
    
    > updateValues(target = prod, project = "example", revisions = revs, 
    +     auto.proceed = TRUE)
    
      When sourcing ‘Upload.R’:
    Error: You are unauthorized. Update your 'token' input with 'magmaRset()', then retry.
    Execution halted
    ```

# qualtRics

<details>

* Version: 3.2.1
* GitHub: https://github.com/ropensci/qualtRics
* Source code: https://github.com/cran/qualtRics
* Date/Publication: 2024-08-16 16:20:02 UTC
* Number of recursive dependencies: 87

Run `revdepcheck::revdep_details(, "qualtRics")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
           ▆
        1. ├─testthat::expect_warning(...) at test-metadata.R:48:1
        2. │ └─testthat:::expect_condition_matching(...)
        3. │   └─testthat:::quasi_capture(...)
        4. │     ├─testthat (local) .capture(...)
        5. │     │ └─base::withCallingHandlers(...)
        6. │     └─rlang::eval_bare(quo_get_expr(.quo), quo_get_env(.quo))
        7. ├─vcr::use_cassette(...)
        8. │ └─cassette$call_block(...)
        9. └─qualtRics::metadata("SV_3gbwq8aJgqPwQDP", get = list(flow = TRUE)) at test-metadata.R:50:5
       10.   └─base::data.frame(...)
      
      [ FAIL 11 | WARN 2 | SKIP 16 | PASS 64 ]
      Error: Test failures
      Execution halted
    ```

# rnoaa

<details>

* Version: 1.4.0
* GitHub: https://github.com/ropensci/rnoaa
* Source code: https://github.com/cran/rnoaa
* Date/Publication: 2023-04-27 08:30:09 UTC
* Number of recursive dependencies: 151

Run `revdepcheck::revdep_details(, "rnoaa")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘testthat.R’
     ERROR
    Running the tests in ‘tests/testthat.R’ failed.
    Last 13 lines of output:
      ── Error ('test-swdi.R:50:5'): Box co-ordinates return correctly ───────────────
      Error in `UseMethod("xpathApply")`: no applicable method for 'xpathApply' applied to an object of class "character"
      Backtrace:
          ▆
       1. ├─vcr::use_cassette(...) at test-swdi.R:47:3
       2. │ └─cassette$call_block(...)
       3. └─rnoaa::swdi(...) at test-swdi.R:50:5
       4.   └─rnoaa:::check_response_swdi(temp, format)
       5.     ├─base::gsub("\n", "", xpathApply(res, "//error", xmlValue)[[1]])
       6.     │ └─base::is.factor(x)
       7.     └─XML::xpathApply(res, "//error", xmlValue)
      
      [ FAIL 2 | WARN 0 | SKIP 68 | PASS 31 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking DESCRIPTION meta-information ... NOTE
    ```
      Missing dependency on R >= 4.1.0 because package code uses the pipe
      |> or function shorthand \(...) syntax added in R 4.1.0.
      File(s) using such syntax:
        ‘vis_miss.R’
    ```

# vcr

<details>

* Version: 1.7.0
* GitHub: https://github.com/ropensci/vcr
* Source code: https://github.com/cran/vcr
* Date/Publication: 2025-03-10 10:10:02 UTC
* Number of recursive dependencies: 65

Run `revdepcheck::revdep_details(, "vcr")` for more info

</details>

## Newly broken

*   checking tests ...
    ```
      Running ‘test-all.R’
     ERROR
    Running the tests in ‘tests/test-all.R’ failed.
    Last 13 lines of output:
        'test-write_disk_path_package_context.R:4:3'
      
      ══ Failed tests ════════════════════════════════════════════════════════════════
      ── Error ('test-localhost_port.R:25:3'): testing against localhost port works ──
      Error in `readLines(con, warn = readLines.warn)`: cannot open the connection
      Backtrace:
          ▆
       1. └─yaml::yaml.load_file(path) at test-localhost_port.R:25:3
       2.   ├─yaml::yaml.load(...)
       3.   │ └─base::paste(string, collapse = "\n")
       4.   └─base::readLines(con, warn = readLines.warn)
      
      [ FAIL 1 | WARN 10 | SKIP 50 | PASS 501 ]
      Error: Test failures
      Execution halted
    ```

## In both

*   checking running R code from vignettes ...
    ```
      ‘cassette-manual-editing.Rmd’ using ‘UTF-8’... failed
      ‘configuration.Rmd’ using ‘UTF-8’... failed
      ‘debugging.Rmd’ using ‘UTF-8’... failed
      ‘design.Rmd’ using ‘UTF-8’... failed
      ‘internals.Rmd’ using ‘UTF-8’... failed
      ‘lightswitch.Rmd’ using ‘UTF-8’... failed
      ‘record-modes.Rmd’ using ‘UTF-8’... failed
      ‘request_matching.Rmd’ using ‘UTF-8’... failed
      ‘vcr.Rmd’ using ‘UTF-8’... failed
      ‘write-to-disk.Rmd’ using ‘UTF-8’... failed
    ...
      9.             └─knitr::knit(..., tangle = opts_knit$get("tangle"), envir = envir)
     10.               └─xfun::read_utf8(input)
     11.                 └─base::readLines(con, encoding = "UTF-8", warn = FALSE)
     12.                   └─base::file(con, "r")
    ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    
    
      When tangling ‘write-to-disk.Rmd’:
    Error: cannot open the connection
    Execution halted
    ```

