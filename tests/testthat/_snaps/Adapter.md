# show_body_diff configuration setting

    Code
      POST("https://hb.cran.dev/post", body = list(apple = "red"))
    Condition
      Error:
      ! Real HTTP connections are disabled.
      ! Unregistered request:
      i POST:  https://hb.cran.dev/post  with body {apple: red}  with headers {Accept: application/json, text/xml, application/xml, */*}
      
      You can stub this request with the following snippet:
       stub_request('post', uri = 'https://hb.cran.dev/post') %>%
           wi_th(
             headers = list('Accept' = 'application/json, text/xml, application/xml, */*'),
             body = list(apple="red")
           )
      
      registered request stubs:
       GET: https://hb.cran.dev/post   with body {"apple":"green"}
      
      
      Body diff:
      < stub$body    > request_s..
      @@ 1,3 @@      @@ 1,3 @@    
        $apple         $apple     
      < [1] "green"  > [1] "red"  
                                  
      
      ============================================================

# show_body_diff configuration setting - > 1 stub

    Code
      POST("https://hb.cran.dev/post", body = list(apple = "red"))
    Condition
      Error:
      ! Real HTTP connections are disabled.
      ! Unregistered request:
      i POST:  https://hb.cran.dev/post  with body {apple: red}  with headers {Accept: application/json, text/xml, application/xml, */*}
      
      You can stub this request with the following snippet:
       stub_request('post', uri = 'https://hb.cran.dev/post') %>%
           wi_th(
             headers = list('Accept' = 'application/json, text/xml, application/xml, */*'),
             body = list(apple="red")
           )
      
      registered request stubs:
       GET: https://hb.cran.dev/post   with body {"apple":"green"}
       GET: https://hb.cran.dev/post   with body {"apple":"green"}
       GET: https://hb.cran.dev/post   with body {"pear":"purple"}
      
      
      Body diff:
      i diffs: >1 stub found, showing diff with least differences
      < stub$body    > request_s..
      @@ 1,3 @@      @@ 1,3 @@    
        $apple         $apple     
      < [1] "green"  > [1] "red"  
                                  
      
      ============================================================

