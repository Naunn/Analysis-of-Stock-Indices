library(dplyr)    # %>%; select(); transmute(); filter(); glimpse();
library(tidyr)    # pivot_wider(); drop_na(); fill(); everything()
library(openxlsx) # write.xlsx();

# stopy procentowe (https://stats.oecd.org)
stopy_procentowe <-
  read.csv(file = "data/oecd/interest rate/MEI_FIN_23042023220434720.csv") %>%
  filter(Subject == "Long-term interest rates, Per cent per annum") %>%
  transmute(Country,
            Date = as.Date(paste0(TIME, "-01")),
            Value = round(Value / 100, 4)) %>%
  pivot_wider(names_from = 'Country',
              values_from = 'Value',
              names_prefix = 'intrate_') %>%
  rename(DATE = Date)

# inflacja rok do roku (https://stats.oecd.org)
inflacja <-
  read.csv(file = "data/oecd/inflation/KEI_23042023223303307.csv") %>%
  transmute(Country,
            Measure,
            Date = as.Date(paste0(TIME, "-01")),
            Value = round(Value / 100, 4)) %>%
  pivot_wider(names_from = 'Country',
              values_from = 'Value',
              names_prefix = 'inflation_') %>%
  rename(DATE = Date) %>%
  filter(Measure == "Growth on the same period of the previous year") %>%
  select(!Measure)

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

# polska gielda (20) https://stooq.pl/q/?s=wig20&c=5y&t=l&a=ln&b=0
polska20 <-
  read.csv("./data/stooq/csv/wig20_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^WIG20",
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

# ukrainska gielda https://stooq.pl/q/?s=^ux&c=5y&t=l&a=ln&b=0
ukraina <-
  read.csv("./data/stooq/csv/^ux_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^UX",
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

# zlaczenie tabel - poniewaz brak notowan na sylwestra, zatem musimy utworzyc sztuczny ciag dat
DATE <-
  seq.Date(from = as.Date("2018-01-01"), to = as.Date("2023-05-25"), by = "day") %>% 
  as.data.frame()
colnames(DATE) <- "DATE"

df_all <-
  DATE %>% 
  left_join(polska, by = "DATE") %>%
  left_join(polska20, by = "DATE") %>%
  left_join(niemcy, by = "DATE") %>%
  left_join(francja, by = "DATE") %>%
  left_join(wlk_bryt, by = "DATE") %>%
  left_join(usa, by = "DATE") %>%
  left_join(finlandia, by = "DATE") %>%
  left_join(ukraina, by = "DATE") %>%
  left_join(rosja, by = "DATE") %>%
  left_join(turcja, by = "DATE") %>%
  left_join(chiny, by = "DATE") %>%
  left_join(bitcoin, by = "DATE") %>%
  left_join(ethernum, by = "DATE") %>% 
  left_join(stopy_procentowe, by = "DATE") %>% 
  left_join(inflacja, by = "DATE") %>%
  # poniewaz stopy procentowe i inflacja sa miesiac po miesiacu, zatem musimy uzupelnic cale miesiace "w dol"
  fill(everything(), .direction = c("down"))

# zapis do pliku
write.xlsx(
  x = df_all,
  file = "./data/prepared/df_all_slim.xlsx",
  overwrite = TRUE,
  rowNames = FALSE
)

