library(tidyverse)


col

col <- tibble::tribble(
           ~names,       ~type, ~width, ~select, ~adjust, ~sintese,
           "tipo", "character",     2L,   FALSE,   FALSE,    FALSE,
           "data",      "Date",     8L,    TRUE,   FALSE,     TRUE,
            "bdi", "character",     2L,    TRUE,   FALSE,    FALSE,
         "symbol", "character",    12L,    TRUE,   FALSE,    FALSE,
        "mercado", "character",     3L,   FALSE,   FALSE,    FALSE,
        "company", "character",    12L,    TRUE,   FALSE,    FALSE,
            "esp", "character",     3L,    TRUE,   FALSE,    FALSE,
             "e0", "character",     1L,   FALSE,   FALSE,    FALSE,
         "evento", "character",     4L,    TRUE,   FALSE,     TRUE,
            "gov", "character",     2L,    TRUE,   FALSE,    FALSE,
             "e1", "character",     7L,   FALSE,   FALSE,    FALSE,
           "open",    "double",    13L,    TRUE,    TRUE,     TRUE,
           "high",    "double",    13L,    TRUE,    TRUE,     TRUE,
            "low",    "double",    13L,    TRUE,    TRUE,     TRUE,
           "mean",    "double",    13L,    TRUE,    TRUE,     TRUE,
          "close",    "double",    13L,    TRUE,    TRUE,     TRUE,
            "buy",    "double",    13L,    TRUE,    TRUE,     TRUE,
           "sell",    "double",    13L,    TRUE,    TRUE,     TRUE,
   "qtd_negocios",    "double",     5L,    TRUE,   FALSE,     TRUE,
          "qtd_t",    "double",    18L,    TRUE,   FALSE,     TRUE,
          "vol_t",    "double",    18L,    TRUE,    TRUE,     TRUE,
             "e2",   "integer",    22L,   FALSE,   FALSE,    FALSE,
          "fator",   "integer",     7L,    TRUE,   FALSE,     TRUE,
             "e3", "character",    13L,   FALSE,   FALSE,    FALSE,
           "isin", "character",    12L,    TRUE,   FALSE,     TRUE,
            "nro",   "integer",     3L,    TRUE,   FALSE,    FALSE
   )

tps <- col$type %>%
  str_sub(end = 1) %>%
  paste0(collapse = "")

res <- readr::read_fwf("B3/COTAHIST_A2020.TXT",
                       col_positions = readr::fwf_widths(col$width,col$names),
                       col_types = tps,
                       skip = 1,
                       locale = locale(date_format = "%Y%m%d")) %>%
  filter(tipo == "01") %>%
  filter(mercado == "010") %>%
  filter(bdi == "02")

ativos <- res %>%
  pull(symbol) %>%
  unique() %>%
  sort()


arqs <- tibble(file_name = dir("yahoo/",full.names = TRUE)) %>%
  mutate(ativo = file_name %>%
           basename() %>%
           str_extract("^[^_]+"))

arqs <- arqs %>%
  filter(!ativo %in% ativos)

ativos[!ativos %in% arqs$ativo]

arqs %>%
  group_by(ativo) %>%
  filter(n()!=3) %>%
  pull(file_name)

arqs <- arqs %>%
  nest(fls = file_name)

teste1 <- arqs %>%
  mutate(fls = map(fls,~.x[[1]]))


files <- teste1$fls[[1]]
file_dir <- "yahoo"
symbol <- "A1AP34"
hist_file_path <- file.path(file_dir, symbol)
div_file_path <- paste0(hist_file_path,"_div")
split_file_path <- paste0(hist_file_path,"_split")

df <- readr::read_csv(hist_file_path, show_col_types = FALSE) %>%
  full_join(readr::read_csv(div_file_path, show_col_types = FALSE), by = "Date") %>%
  full_join(readr::read_csv(split_file_path, show_col_types = FALSE), by = "Date")

tmp <- res %>%
  filter(symbol == !!symbol) %>%
  select(data,evento,close)

tmp %>%
  full_join(df, by = c("data" = "Date")) %>%
  View()
