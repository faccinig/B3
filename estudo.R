library(tidyverse)
library(tidyquant)


file_dir <- "yahoo"
symbol <- "EMBR3"
hist_file_path <- file.path(file_dir, symbol)
div_file_path <- paste0(hist_file_path,"_div")
split_file_path <- paste0(hist_file_path,"_split")

df <- readr::read_csv(hist_file_path, show_col_types = FALSE) %>%
  full_join(readr::read_csv(div_file_path, show_col_types = FALSE), by = "Date") %>%
  full_join(readr::read_csv(split_file_path, show_col_types = FALSE), by = "Date")


df %>%
  filter(is.na(Close))

embr3 <- df %>%
  filter(!is.na(Close)) %>%
  mutate(ajuste = `Adj Close`/Close) %>%
  mutate(across(c(Open,High,Low, Close),
                ~.x * ajuste)) %>%
  rename_all(str_to_lower)


ATR <- function(df, n = 14L) {
  n <- 14L
  closeLag <- lag(df$close)

  trueHigh <- pmax(df$high, closeLag, na.rm = FALSE)
  trueLow <- pmin(df$low, closeLag, na.rm = FALSE)
  tr <- trueHigh - trueLow

  media_movel(tr, n = n)
}

media_movel <- function(vector, n = 14L) {
  zoo::rollmeanr(vector, k = n, fill = NA)
}

n_suporte = 21L
desvios_atr = 0.5

simular <- function(n_atr = 14L,
                    n_suporte = 14L,
                    desvios_atr = 3) {

  data <- embr3 %>%
    mutate(atr = lag(ATR(., n_atr)),
           l_atr = lag(low) - desvios_atr * atr,
           suporte = lag(zoo::rollmaxr(high, n_suporte, NA)))


  data$size <- 0L
  data$stop_loss <- NA_real_
  data$order <- NA_character_
  data$cash <- 0
  data$stock_flow <- 0
  data$stock <- 0
  data$patrimonio <- 0
  initial_cash <- 100000
  for (inicio in seq_along(data$date)) {
    data$cash[inicio] <- initial_cash
    if (!is.na(data$l_atr[inicio])
        && !is.na(data$suporte[inicio])) {
      break()
    }
  }

  for (i in seq.int(inicio, length(data$date))) {
    data$cash[i] <- data$cash[i - 1L]
    data$size[i] <- data$size[i - 1L]

    if (data$high[i] > data$suporte[i] &&
        data$size[i] == 0L) {
      # Executa a ordem de compra
      data$order[i] <- "buy"
      # calcula o tamanho da posição a ser comprada
      risco_aceito <- data$cash[i] * 0.01
      risco_assumido <- data$suporte[i] - data$l_atr[i]
      lote_ideal <- risco_aceito/risco_assumido
      lote_ideal <- (lote_ideal %/% 100) * 100

      data$size[i] <- lote_ideal
      data$stock_flow[i] <- data$size[i] * data$suporte[i]
      data$cash[i] <- data$cash[i] - data$size[i] * data$suporte[i]
    }

    if (data$size[i] > 0L) {
      # insere ordem de stop loss quando tem posição comprada
      if (!is.na(data$stop_loss[i-1L])
          && data$stop_loss[i-1L] > data$l_atr[i]) {
        # se já existe stop_loss e ele for menor que a banda atual
        # usar o último
        data$stop_loss[i] <- data$stop_loss[i-1L]

      } else {
        data$stop_loss[i] <- data$l_atr[i]
      }
    }

    if (!is.na(data$stop_loss[i])
        && data$stop_loss[i] >= data$low[i]) {
      # Executa a ordem de stop loss
      data$order[i] <- "sell"
      data$stock_flow[i] <- -data$size[i] * data$stop_loss[i]
      data$cash[i] <- data$cash[i] + data$size[i] * data$stop_loss[i]
      data$size[i] <- 0L
    }

    data$stock[i] = data$close[i] * data$size[i]
    data$patrimonio[i] <- data$cash[i] + data$stock[i]
  }
  last(data$patrimonio)
}


res <- crossing(desvios_atr = seq(2, 9, by = 0.25),
                n_suporte = 5:20) %>%
  mutate(resultado = pmap_dbl(., hlp::with_dots(simular)))

res %>%
  ggplot(aes(x = desvios_atr, n_suporte, fill = resultado)) +
  geom_tile()


tmp <- teste %>%
  filter(between(date,
                 as.Date("2015-01-01"),
                 as.Date("2017-01-01")))

tmp %>%
  ggplot(aes(date, close)) +
  geom_candlestick(aes(open = open, high = high, low = low, close = close)) +
  geom_line(aes(y = stop_loss), color = "red") +
  geom_line(aes(y = suporte)) +
  geom_point(data = tmp %>%
               filter(order == "buy"),
             shape = 17,
             color = "green") +
  geom_point(data = tmp %>%
               filter(order == "sell"),
             shape = 25,
             color = "orange",
             fill = "orange")


geom_point
