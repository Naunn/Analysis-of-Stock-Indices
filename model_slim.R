library(dplyr)
library(tidyr)
library(readxl)
library(forecast)
library(Quandl) # https://financetrain.com/financial-time-series-data

covid <-
  as.Date("2020-02-19") # pomiedzy "2020-01-30" (wyciek wirusa poza Chiny), a "2020-03-11" (ogloszenie pandemii)
rus_aggression <- as.Date("2022-02-24")

df_all_slim_real <-
  read_excel("data/prepared/df_all_slim_real.xlsx") %>%
  mutate(DATE = as.Date(DATE)) %>%
  # uzupelnienie brakow "w gore"
  fill(everything(), .direction = c("up")) %>%
  # dodanie zewnetrznych regresorow
  mutate(
    pandemic = ifelse(DATE > covid &
                        DATE < rus_aggression, 1, 0),
    # od czasu wybuchu wojny, pandemiczne regulacje "wyparowaly"
    rus_aggresion = ifelse(DATE >= rus_aggression, 1, 0)
  )

df_all_slim_norm <-
  read_excel("data/prepared/df_all_slim_norm.xlsx") %>%
  mutate(DATE = as.Date(DATE)) %>%
  # uzupelnienie brakow "w gore"
  fill(everything(), .direction = c("up"))

# konwersja na tabele szeregu czasowego https://www.statology.org/r-convert-data-frame-to-time-series/
df_ts <-
  read.zoo(df_all_slim_real)
df_ts_norm <-
  read.zoo(df_all_slim_norm)

df_ts %>% colnames()
# [1] "^BTC"        "^ETH"        "^WIG_real"   "^WIG20_real" "^PX_real"    "^OMXT_real"
# [7] "^BUX_real"   "^DAX_real"   "^CAC_real"   "^UKX_real"   "^SPX_real"   "^HEX_real"
# [13] "^RTS_real"   "^XU100_real" "^SHC_real"

start(df_ts) # "2018-01-01"
end(df_ts) # "2023-05-24"
frequency(df_ts) # 1 (day)

# wizualizacja # https://financetrain.com/plotting-financial-time-series-data-multiple-columns-in-r
df_ts %>% plot() # malo czytelne

# pojedynczy sygnal
df_ts$`^WIG_real` %>% plot()

# kryptowaluty
df_ts[, 1:2] %>% ts.plot(col = 1:2)
legend(
  "topleft",
  colnames(df_ts[, 1:2]),
  col = 1:2,
  bty = "n",
  horiz = FALSE,
  lty = 1,
  cex = 1
)

df_ts_norm[, 1:2] %>% ts.plot(col = 1:2)
legend(
  "topleft",
  colnames(df_ts[, 1:2]),
  col = 1:2,
  bty = "n",
  horiz = FALSE,
  lty = 1,
  cex = 1
)

# rynki
df_ts[, 3:15] %>% ts.plot(col = 3:15)
legend(
  "top",
  colnames(df_ts[, 3:15]),
  col = 3:15,
  bty = "n",
  horiz = TRUE,
  lty = 1,
  cex = 0.7,
  text.width = 0.7
)

df_ts_norm[, 3:15] %>% ts.plot(col = 3:15)
legend(
  "top",
  colnames(df_ts[, 3:15]),
  col = 3:15,
  bty = "n",
  horiz = TRUE,
  lty = 1,
  cex = 0.7,
  text.width = 0.7
)

# porownanie srednich wykorzystujac model arima (automat)
# nie potrzeba zewnetrzengo regresora z uwagi na podzial sygnalow na zdarzenia
# (powinno to zniwelowac efekt szumu, w porownaniu do mierzenia zwyklych srednich z wartosci)

# podzielimy na 3 okresy: przed pandemia; po panedmii, a przed agresja rosji; po agresji rosji
pre_covid <- window(df_ts, start = start(df_ts), end = covid)
post_covid <- window(df_ts, start = covid, end = rus_aggression)
post_aggression <-
  window(df_ts, start = rus_aggression, end = end(df_ts))

# utworzenie modeli
pre_covid_model <- auto.arima(pre_covid$`^OMXT_real`)
checkresiduals(pre_covid_model) # stacjonarny
post_covid_model <- auto.arima(post_covid$`^OMXT_real`)
checkresiduals(post_covid_model) # stacjonarny
post_aggression_model <- auto.arima(post_aggression$`^OMXT_real`)
checkresiduals(post_aggression_model) # stacjonarny

# wizualne porownanie modeli
plot(df_ts$`^OMXT_real`)
lines(fitted(pre_covid_model), col = 'green')
lines(fitted(post_covid_model), col = 'blue')
lines(fitted(post_aggression_model), col = 'red')

# porownanie srednich
pre_covid_mean <- fitted(pre_covid_model) %>% mean()
pre_covid_mean
post_covid_mean <- fitted(post_covid_model) %>% mean()
post_covid_mean
post_aggression_mean <- fitted(post_aggression_model) %>% mean()
post_aggression_mean

# wizualne porownanie srednich z modeli (chatGPT)
pre_covid_mean_ts <- zoo(pre_covid_mean, index(pre_covid))
post_covid_mean_ts <- zoo(post_covid_mean, index(post_covid))
post_aggression_mean_ts <-
  zoo(post_aggression_mean, index(post_aggression))

plot(df_ts$`^OMXT_real`)
lines(pre_covid_mean_ts, col = 'green')
lines(post_covid_mean_ts, col = 'blue')
lines(post_aggression_mean_ts, col = 'red')

# predykcja calosci
fit <-
  auto.arima(df_ts$`^OMXT_real`, xreg = as.matrix(df_all_slim_real[, 17:18]))
checkresiduals(fit)
periods = 21
plot(forecast(fit, h = periods, xreg = as.matrix(
  # chatGPT
  data.frame(
    "pandemic" = rep(0, length.out = periods),
    "rus_aggresion" = rep(1, length.out = periods)
  )
)))
