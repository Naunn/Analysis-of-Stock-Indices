library(dplyr)
library(readxl)
library(Quandl) # https://financetrain.com/financial-time-series-data

df_all_slim_real <-
  read_excel("data/prepared/df_all_slim_real.xlsx") %>%
  mutate(DATE = as.Date(DATE)) %>%
  # uzupelnienie brakow "w gore"
  fill(everything(), .direction = c("up"))

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
df_ts[, 3:13] %>% ts.plot(col = 3:13)
legend(
  "top",
  colnames(df_ts[, 3:13]),
  col = 3:13,
  bty = "n",
  horiz = TRUE,
  lty = 1,
  cex = 0.7,
  text.width = 0.7
)

df_ts_norm[, 3:13] %>% ts.plot(col = 3:13)
legend(
  "top",
  colnames(df_ts[, 3:13]),
  col = 3:13,
  bty = "n",
  horiz = TRUE,
  lty = 1,
  cex = 0.7,
  text.width = 0.7
)

# porownanie srednich wykorzystujac model sredniej ruchomej
# (powinno to zniwelowac efekt szumu, w porownaniu do mierzenia zwyklych srednich)

# podzielimy na 3 okresy: przed pandemia; po panedmii, a przed agresja rosji; po agresji rosji
covid <-
  as.Date("2020-02-19") # pomiedzy "2020-01-30" (wyciek wirusa poza Chiny), a "2020-03-11" (ogloszenie pandemii)
rus_aggression <- as.Date("2022-02-24")
pre_covid <- window(df_ts, start = start(df_ts), end = covid)
post_covid <- window(df_ts, start = covid, end = rus_aggression)
post_aggression <-
  window(df_ts, start = rus_aggression, end = end(df_ts))

# utworzenie modeli
pre_covid_ma <- arima(pre_covid$`^WIG_real`, order = c(0, 0, 1))
post_covid_ma <- arima(post_covid$`^WIG_real`, order = c(0, 0, 1))
post_aggression_ma <-
  arima(post_aggression$`^WIG_real`, order = c(0, 0, 1))

# wizualne porownanie modeli
plot(df_ts$`^WIG_real`)
lines(fitted(pre_covid_ma), col = 'green')
lines(fitted(post_covid_ma), col = 'blue')
lines(fitted(post_aggression_ma), col = 'red')

# porownanie srednich
pre_covid_mean <- fitted(pre_covid_ma) %>% mean()
post_covid_mean <- fitted(post_covid_ma) %>% mean()
post_aggression_mean <- fitted(post_aggression_ma) %>% mean()

# wizualne porownanie srednich z modeli (chatGPT)
pre_covid_mean_ts <- zoo(pre_covid_mean, index(pre_covid))
post_covid_mean_ts <- zoo(post_covid_mean, index(post_covid))
post_aggression_mean_ts <-
  zoo(post_aggression_mean, index(post_aggression))

plot(df_ts$`^WIG_real`)
lines(pre_covid_mean_ts, col = 'green')
lines(post_covid_mean_ts, col = 'blue')
lines(post_aggression_mean_ts, col = 'red')

# test srednich (dla modeli) poprzez test srednich, gdzie hipoteza zerowa = srednie sa takie same
t.test(fitted(pre_covid_ma),
       fitted(post_covid_ma),
       alternative = c("two.sided"))
# data:  fitted(pre_covid_ma) and fitted(post_covid_ma)
# t = 37.006, df = 959.05, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   4755.563 5288.184
# sample estimates:
#   mean of x mean of y
# 57804.85  52782.97
# p-value < 0.05, zatem odrzucamy hipoteze o rownosci srednich na korzysc hipotezy alternatywnej

t.test(fitted(post_covid_ma),
       fitted(post_aggression_ma),
       alternative = c("two.sided"))
# data:  fitted(post_covid_ma) and fitted(post_aggression_ma)
# t = 75.163, df = 1186.2, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   12026.83 12671.52
# sample estimates:
#   mean of x mean of y
# 52782.97  40433.80
# p-value < 0.05, zatem odrzucamy hipoteze o rownosci srednich na korzysc hipotezy alternatywnej

t.test(fitted(pre_covid_ma),
       fitted(post_aggression_ma),
       alternative = c("two.sided"))
# data:  fitted(pre_covid_ma) and fitted(post_aggression_ma)
# t = 149.5, df = 659.9, p-value < 2.2e-16
# alternative hypothesis: true difference in means is not equal to 0
# 95 percent confidence interval:
#   17142.89 17599.20
# sample estimates:
#   mean of x mean of y
# 57804.85  40433.80
# p-value < 0.05, zatem odrzucamy hipoteze o rownosci srednich na korzysc hipotezy alternatywnej

# predykcja z autodopasowania
fit <- df_ts$`^WIG_real` %>% forecast::auto.arima()
plot(forecast::forecast(fit, h = 21))
