library(dplyr)    # %>%; select(); transmute(); filter(); glimpse();
library(tidyr)    # pivot_wider(); drop_na(); fill(); everything()
library(forecast)
library(Quandl) # https://financetrain.com/financial-time-series-data
library(ggplot2)

# PRZYGOTOWANIE DANYCH =================================================================================================
# inflacja rok do roku (https://stats.oecd.org)
inflacja_skum <-
  read.csv(file = "data/oecd/inflation/KEI_23042023223303307.csv") %>%
  transmute(Country,
            Measure,
            Date = as.Date(paste0(TIME, "-01")),
            Value = round(Value / 100, 4)) %>%
  pivot_wider(names_from = 'Country',
              values_from = 'Value',
              names_prefix = 'inflation_acc_') %>%
  rename(DATE = Date) %>%
  filter(Measure == "Growth previous period") %>%
  select(
    DATE,
    inflation_acc_Poland,
    `inflation_acc_Czech Republic`,
    inflation_acc_Estonia,
    inflation_acc_Hungary,
    inflation_acc_Germany,
    inflation_acc_France,
    `inflation_acc_United Kingdom`,
    `inflation_acc_United States`,
    inflation_acc_Finland,
    # inflation_acc_Ukraine, # brak danych
    inflation_acc_Russia,
    inflation_acc_Türkiye,
    `inflation_acc_China (People's Republic of)`
  ) %>%
  mutate_if(is.numeric, cumsum)

# polska gielda https://stooq.pl/q/?s=wig&c=5y&t=l&a=ln&b=0
polska <-
  read.csv("./data/stooq/csv/wig_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^WIG",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# czeska gielda https://stooq.pl/q/?s=^px&c=5y&t=l&a=ln&b=0
czechy <-
  read.csv("./data/stooq/csv/^px_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^PX",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# estonska gielda https://stooq.pl/q/?s=^omxt&c=5y&t=l&a=ln&b=0
estonia <-
  read.csv("./data/stooq/csv/^omxt_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^OMXT",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# wegierska gielda https://stooq.pl/q/?s=^bux&c=5y&t=l&a=ln&b=0
wegry <-
  read.csv("./data/stooq/csv/^bux_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^BUX",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# niemiecka gielda https://stooq.pl/q/?s=^dax&c=5y&t=l&a=ln&b=0
niemcy <-
  read.csv("./data/stooq/csv/^dax_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^DAX",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# francuska gielda https://stooq.pl/q/?s=^cac&c=5y&t=l&a=ln&b=0
francja <-
  read.csv("./data/stooq/csv/^cac_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^CAC",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# brytyjska gielda https://stooq.pl/q/?s=^ukx&c=5y&t=l&a=ln&b=0
wlk_bryt <-
  read.csv("./data/stooq/csv/^ukx_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^UKX",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# amerykanska gielda https://stooq.pl/q/?s=^spx&c=5y&t=l&a=ln&b=0
usa <-
  read.csv("./data/stooq/csv/^spx_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^SPX",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# finlandzka gielda https://stooq.pl/q/?s=^hex&c=5y&t=l&a=ln&b=0
finlandia <-
  read.csv("./data/stooq/csv/^hex_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^HEX",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# rosyjska gielda https://stooq.pl/q/?s=^rts&c=5y&t=l&a=ln&b=0
rosja <-
  read.csv("./data/stooq/csv/^rts_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^RTS",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# turecka gielda https://stooq.pl/q/?s=^xu100&c=5y&t=l&a=ln&b=0
turcja <-
  read.csv("./data/stooq/csv/^xu100_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^XU100",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# chinska gielda https://stooq.pl/q/?s=^shc&c=5y&t=l&a=ln&b=0
chiny <-
  read.csv("./data/stooq/csv/^shc_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^SHC",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# kurs bitcoina https://stooq.pl/q/?s=btc.v&c=5y&t=l&a=ln&b=0
bitcoin <-
  read.csv("./data/stooq/csv/btc_v_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^BTC",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# kurs ethernum https://stooq.pl/q/?s=eth.v&c=5y&t=l&a=ln&b=0
ethernum <-
  read.csv("./data/stooq/csv/eth_v_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^ETH",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# zlaczenie tabel - poniewaz brak notowan na sylwestra, wiec musimy zaczac od kryptowalut
df_all <-
  bitcoin %>%
  left_join(ethernum, by = "DATE") %>%
  left_join(polska, by = "DATE") %>%
  left_join(czechy, by = "DATE") %>%
  left_join(estonia, by = "DATE") %>%
  left_join(wegry, by = "DATE") %>%
  left_join(niemcy, by = "DATE") %>%
  left_join(francja, by = "DATE") %>%
  left_join(wlk_bryt, by = "DATE") %>%
  left_join(usa, by = "DATE") %>%
  left_join(finlandia, by = "DATE") %>%
  left_join(ukraina, by = "DATE") %>%
  left_join(rosja, by = "DATE") %>%
  left_join(turcja, by = "DATE") %>%
  left_join(chiny, by = "DATE") %>%
  left_join(inflacja_rr, by = "DATE") %>%
  left_join(inflacja_skum, by = "DATE") %>%
  # poniewaz inflacja jest miesiac po miesiacu, wiec uzupelniamy cale miesiace "w dol"
  fill(everything(), .direction = c("down"))

df_all_real <-
  df_all %>%
  mutate(
    `^WIG_real` = `^WIG` * (1 - inflation_acc_Poland),
    `^PX_real` = `^PX` * (1 - `inflation_acc_Czech Republic`),
    `^OMXT_real` = `^OMXT` * (1 - inflation_acc_Estonia),
    `^BUX_real` = `^BUX` * (1 - inflation_acc_Hungary),
    `^DAX_real` = `^DAX` * (1 - inflation_acc_Germany),
    `^CAC_real` = `^CAC` * (1 - inflation_acc_France),
    `^UKX_real` = `^UKX` * (1 - `inflation_acc_United Kingdom`),
    `^SPX_real` = `^SPX` * (1 - `inflation_acc_United States`),
    `^HEX_real` = `^HEX` * (1 - inflation_acc_Finland),
    `^RTS_real` = `^RTS` * (1 - inflation_acc_Russia),
    `^XU100_real` = `^XU100` * (1 - inflation_acc_Türkiye),
    `^SHC_real` = `^SHC` * (1 - `inflation_acc_China (People's Republic of)`)
  )

# WIZUALIZACJA =========================================================================================================
# https://rstudio-pubs-static.s3.amazonaws.com/419265_91c5fb9acf1742f88369c161ca59b6ef.html
ggplot(df_all_real, aes(x = DATE)) +
  labs(title = "Porównanie wskaźników (przy i bez uwzglednienia inflacji) dla Polski",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^WIG`), color = "black") +
  geom_line(aes(y = `^WIG_real`), color = "blue") # jest roznica ...

ggplot(df_all_real, aes(x = DATE)) +
  labs(title = "Porównanie wskaźników (przy i bez uwzglednienia inflacji) dla Turcji",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^XU100`), color = "black") +
  geom_line(aes(y = `^XU100_real`), color = "blue") # jest roznica ...

ggplot(df_all_real, aes(x = DATE)) +
  # The World Health Organization declared the COVID-19 outbreak
  geom_vline(
    xintercept = as.numeric(as.Date("2020-01-30")),
    color = "red",
    linetype = "dashed"
  ) +
  # The World Health Organization declared the COVID-19 to be pandemic
  geom_vline(
    xintercept = as.numeric(as.Date("2020-03-11")),
    color = "blue",
    linetype = "dotdash"
  ) +
  # Russian aggression against Ukraine
  geom_vline(
    xintercept = as.numeric(as.Date("2022-02-24")),
    color = "black",
    linetype = "dashed"
  ) +
  # Bankruptcy of FTX
  geom_vline(
    xintercept = as.numeric(as.Date("2022-11-07")),
    color = "red",
    linetype = "dashed"
  ) +
  # Colapse of Silicon Valley Bank
  geom_vline(
    xintercept = as.numeric(as.Date("2023-03-10")),
    color = "blue",
    linetype = "dotted"
  ) +
  labs(title = "Polska (WIG)",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^WIG_real`), color = "blue")
# Na oko, rynek zdecydowanie odczul covid, a nastepnie wojne

ggplot(df_all_real, aes(x = DATE)) +
  # The World Health Organization declared the COVID-19 outbreak
  geom_vline(
    xintercept = as.numeric(as.Date("2020-01-30")),
    color = "red",
    linetype = "dashed"
  ) +
  # The World Health Organization declared the COVID-19 to be pandemic
  geom_vline(
    xintercept = as.numeric(as.Date("2020-03-11")),
    color = "blue",
    linetype = "dotdash"
  ) +
  # Russian aggression against Ukraine
  geom_vline(
    xintercept = as.numeric(as.Date("2022-02-24")),
    color = "black",
    linetype = "dashed"
  ) +
  # Bankruptcy of FTX
  geom_vline(
    xintercept = as.numeric(as.Date("2022-11-07")),
    color = "red",
    linetype = "dashed"
  ) +
  # Colapse of Silicon Valley Bank
  geom_vline(
    xintercept = as.numeric(as.Date("2023-03-10")),
    color = "blue",
    linetype = "dotted"
  ) +
  labs(title = "Rosja (RTS)",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^RTS_real`), color = "blue")
# Na oko, rynek zdecydowanie odczul covid, a nastepnie wojne

ggplot(df_all_real, aes(x = DATE)) +
  # The World Health Organization declared the COVID-19 outbreak
  geom_vline(
    xintercept = as.numeric(as.Date("2020-01-30")),
    color = "red",
    linetype = "dashed"
  ) +
  # The World Health Organization declared the COVID-19 to be pandemic
  geom_vline(
    xintercept = as.numeric(as.Date("2020-03-11")),
    color = "blue",
    linetype = "dotdash"
  ) +
  # Russian aggression against Ukraine
  geom_vline(
    xintercept = as.numeric(as.Date("2022-02-24")),
    color = "black",
    linetype = "dashed"
  ) +
  # Bankruptcy of FTX
  geom_vline(
    xintercept = as.numeric(as.Date("2022-11-07")),
    color = "red",
    linetype = "dashed"
  ) +
  # Colapse of Silicon Valley Bank
  geom_vline(
    xintercept = as.numeric(as.Date("2023-03-10")),
    color = "blue",
    linetype = "dotted"
  ) +
  labs(title = "Niemcy (DAX)",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^DAX_real`), color = "blue")
# Na oko, rynek zdecydowanie odczul covid, a nastepnie wojne

ggplot(df_all_real, aes(x = DATE)) +
  # The World Health Organization declared the COVID-19 outbreak
  geom_vline(
    xintercept = as.numeric(as.Date("2020-01-30")),
    color = "red",
    linetype = "dashed"
  ) +
  # The World Health Organization declared the COVID-19 to be pandemic
  geom_vline(
    xintercept = as.numeric(as.Date("2020-03-11")),
    color = "blue",
    linetype = "dotdash"
  ) +
  # Russian aggression against Ukraine
  geom_vline(
    xintercept = as.numeric(as.Date("2022-02-24")),
    color = "black",
    linetype = "dashed"
  ) +
  # Bankruptcy of FTX
  geom_vline(
    xintercept = as.numeric(as.Date("2022-11-07")),
    color = "red",
    linetype = "dashed"
  ) +
  # Colapse of Silicon Valley Bank
  geom_vline(
    xintercept = as.numeric(as.Date("2023-03-10")),
    color = "blue",
    linetype = "dotted"
  ) +
  labs(title = "Stany Zjednoczone (SPX)",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^SPX_real`), color = "blue")
# Na oko, rynek zdecydowanie odczul covid, a nastepnie wojne

ggplot(df_all_real, aes(x = DATE)) +
  # The World Health Organization declared the COVID-19 outbreak
  geom_vline(
    xintercept = as.numeric(as.Date("2020-01-30")),
    color = "red",
    linetype = "dashed"
  ) +
  # The World Health Organization declared the COVID-19 to be pandemic
  geom_vline(
    xintercept = as.numeric(as.Date("2020-03-11")),
    color = "blue",
    linetype = "dotdash"
  ) +
  # Russian aggression against Ukraine
  geom_vline(
    xintercept = as.numeric(as.Date("2022-02-24")),
    color = "black",
    linetype = "dashed"
  ) +
  # Bankruptcy of FTX
  geom_vline(
    xintercept = as.numeric(as.Date("2022-11-07")),
    color = "red",
    linetype = "dashed"
  ) +
  # Colapse of Silicon Valley Bank
  geom_vline(
    xintercept = as.numeric(as.Date("2023-03-10")),
    color = "blue",
    linetype = "dotted"
  ) +
  labs(title = "Bitcoin",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^BTC`), color = "blue")
# Na oko, rynek zdecydowanie odczul covid, a nastepnie wojne

ggplot(df_all_real, aes(x = DATE)) +
  # The World Health Organization declared the COVID-19 outbreak
  geom_vline(
    xintercept = as.numeric(as.Date("2020-01-30")),
    color = "red",
    linetype = "dashed"
  ) +
  # The World Health Organization declared the COVID-19 to be pandemic
  geom_vline(
    xintercept = as.numeric(as.Date("2020-03-11")),
    color = "blue",
    linetype = "dotdash"
  ) +
  # Russian aggression against Ukraine
  geom_vline(
    xintercept = as.numeric(as.Date("2022-02-24")),
    color = "black",
    linetype = "dashed"
  ) +
  # Bankruptcy of FTX
  geom_vline(
    xintercept = as.numeric(as.Date("2022-11-07")),
    color = "red",
    linetype = "dashed"
  ) +
  # Colapse of Silicon Valley Bank
  geom_vline(
    xintercept = as.numeric(as.Date("2023-03-10")),
    color = "blue",
    linetype = "dotted"
  ) +
  labs(title = "Ethernum",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^ETH`), color = "blue")
# Na oko, rynek zdecydowanie odczul covid, a nastepnie wojne

# MODEL ================================================================================================================
covid <-
  as.Date("2020-02-19") # pomiedzy "2020-01-30" (wyciek wirusa poza Chiny), a "2020-03-11" (ogloszenie pandemii)
rus_aggression <- as.Date("2022-02-24")

# nalezy przekonwertowac zbior do dedykowanego dla szeregow czasowych formatu 'zoo'
df_ts <-
  df_all_real %>%
  # wybranie tylko kolumn z realnymi wartosciami
  select(c(DATE, ends_with("_real"), `^BTC`, `^ETH`)) %>%
  # uzupelnienie brakow "w gore"
  fill(everything(), .direction = c("up")) %>% 
  read.zoo()

# podzielimy na 3 okresy: przed pandemia; po panedmii, a przed agresja rosji; po agresji rosji
pre_covid <- window(df_ts, start = start(df_ts), end = covid)
post_covid <- window(df_ts, start = covid, end = rus_aggression)
post_aggression <-
  window(df_ts, start = rus_aggression, end = end(df_ts))

# utworzenie modeli - należy wybrac wskaznik
df_ts %>% colnames()
# [1] "^WIG_real"   "^PX_real"    "^OMXT_real"  "^BUX_real"   "^DAX_real"   "^CAC_real"  
# [7] "^UKX_real"   "^SPX_real"   "^HEX_real"   "^RTS_real"   "^XU100_real" "^SHC_real"  
# [13] "^BTC"        "^ETH"   
pre_covid_model <- auto.arima(pre_covid$`^WIG_real`)
if (checkresiduals(pre_covid_model)$p.value > 0.05){
  print("Szereg jest stacjonarny")
} else {
  print("Szereg NIE jest stacjonarny")
}
post_covid_model <- auto.arima(post_covid$`^WIG_real`)
if (checkresiduals(post_covid_model)$p.value > 0.05){
  print("Szereg jest stacjonarny")
} else {
  print("Szereg NIE jest stacjonarny")
}
post_aggression_model <- auto.arima(post_aggression$`^WIG_real`)
if (checkresiduals(post_aggression_model)$p.value > 0.05){
  print("Szereg jest stacjonarny")
} else {
  print("Szereg NIE jest stacjonarny")
}

# wizualne porownanie modeli
plot(df_ts$`^WIG_real`, main = "Polska (WIG)")
lines(fitted(pre_covid_model), col = 'green')
lines(fitted(post_covid_model), col = 'blue')
lines(fitted(post_aggression_model), col = 'red')
