res <- readr::read_fwf("B3/COTAHIST_A2020.TXT",
                       col_positions = readr::fwf_widths(col$width,col$names),
                       skip = 1)

res %>%
  filter(bdi == "02") %>%
  distinct(symbol) %>%
  pwalk(function(symbol) {
    cat(symbol," ")
    walk(c("history", "div", "split"),
         function(tp) {
           try(download_yahoo_symbol(symbol, dest_dir = "yahoo/",type = tp))
           cat(".")
           Sys.sleep(2)
         })
    cat("ok!\n")
  })
