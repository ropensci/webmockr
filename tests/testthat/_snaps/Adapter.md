# show_body_diff configuration setting

    Code
      POST("https://hb.opencpu.org/post", body = list(apple = "red"))
    Condition
      Warning:
      `RequestPattern()` was deprecated in webmockr 2.1.0.
      i The deprecated feature was likely used in the R6 package.
        Please report the issue at <https://github.com/r-lib/R6/issues>.
      Error:
      ! Real HTTP connections are disabled.
      ! Unregistered request:
      i POST:  https://hb.opencpu.org/post  with body {apple: red}  with headers {Accept: application/json, text/xml, application/xml, */*}
      
      You can stub this request with the following snippet:
       stub_request('post', uri = 'https://hb.opencpu.org/post') %>%
           wi_th(
             headers = list('Accept' = 'application/json, text/xml, application/xml, */*'),
             body = list(apple="red")
           )
      
      registered request stubs:
       GET: https://hb.opencpu.org/post   with body {"apple":"green"}
      
      
      Body diff:
      < stub$body    > request_s..
      @@ 1,3 @@      @@ 1,3 @@    
        $apple         $apple     
      < [1] "green"  > [1] "red"  
                                  
      
      ============================================================

# show_body_diff configuration setting - > 1 stub

    Code
      POST("https://hb.opencpu.org/post", body = list(apple = "red"))
    Condition
      Error:
      ! Real HTTP connections are disabled.
      ! Unregistered request:
      i POST:  https://hb.opencpu.org/post  with body {apple: red}  with headers {Accept: application/json, text/xml, application/xml, */*}
      
      You can stub this request with the following snippet:
       stub_request('post', uri = 'https://hb.opencpu.org/post') %>%
           wi_th(
             headers = list('Accept' = 'application/json, text/xml, application/xml, */*'),
             body = list(apple="red")
           )
      
      registered request stubs:
       GET: https://hb.opencpu.org/post   with body {"apple":"green"}
       GET: https://hb.opencpu.org/post   with body {"apple":"green"}
       GET: https://hb.opencpu.org/post   with body {"pear":"purple"}
      
      
      Body diff:
      i diffs: >1 stub found, showing diff with least differences
      < stub$body    > request_s..
      @@ 1,3 @@      @@ 1,3 @@    
        $apple         $apple     
      < [1] "green"  > [1] "red"  
                                  
      
      ============================================================

