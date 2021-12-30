

.yahooURL <- function(symbol, from, to = Sys.Date(), period = "1d", type = "history") {
  # as.POSIXct converte "Date" considerando que Date está em UTC e deve ir para
  # o tz do sistema. assim acaba por alterar para o dia anterior.
  # converter para POSIXlt antes resolve o problema
  from <- as.POSIXct(as.POSIXlt(as.Date(from)))
  to <- as.POSIXct(as.POSIXlt(as.Date(to)))

  period <- match.arg(period, c("1d", "1wk", "1mo"))
  type <- match.arg(type, c("history", "div", "split"))
  url_end <- if (type == "history") "&includeAdjustedClose=true" else ""
  n <- if (unclass(Sys.time()) %% 1L >= 0.5) 1L else 2L

  paste0("https://query", n, ".finance.yahoo.com/v7/finance/download/",
         symbol,
         sprintf("?period1=%.0f&period2=%.0f", from, to),
         "&interval=", period,
         "&events=", type,
         url_end)
}

download_yahoo_symbol <- function(symbol,
                                  dest_dir = ".",
                                  from = "1994-01-01", to = Sys.Date(),
                                  type = c("history", "div", "split"),
                                  is_B3 = TRUE) {

  dest_file_name <- paste0(symbol,
                           `if`(type == "history","",
                                paste0("_", type)))

  dest_file <- normalizePath(file.path(dest_dir,dest_file_name))
  if (is_B3) symbol <- paste0(symbol, ".SA")

  url <- .yahooURL(symbol,
                   from = from, to = to,
                   type = type)

  download.file(url, dest_file,
                mode = "wb",
                cacheOK = FALSE)
}

download_B3 <- function(year, zip_dir = ".", dest_dir = ".") {
  year <- as.integer(year)
  file_name <- paste0("COTAHIST_A",year,".ZIP")
  zip_file <- normalizePath(file.path(zip_dir, file_name), mustWork = FALSE)
  url <- paste0("https://bvmf.bmfbovespa.com.br/InstDados/SerHist/", file_name)

  download.file(url, destfile = zip_file, mode = "wb")

  unzip(zip_file, exdir = dest_dir)
}



