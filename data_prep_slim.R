library(dplyr)    # %>%; select(); transmute(); filter(); glimpse();
library(readr)    # read_table(); separate_wider_delim();
library(tidyr)    # pivot_wider(); drop_na();

# stopy procentowe (https://stats.oecd.org)
stopy_procentowe <-
  read.csv(file = "data/oecd/interest rate/MEI_FIN_23042023220434720.csv") %>%
  filter(Subject == "Long-term interest rates, Per cent per annum") %>% 
  transmute(Country,
            Date = as.Date(paste0(TIME, "-01")),
            Value = round(Value / 100, 4)) %>%
  pivot_wider(names_from = 'Country', values_from = 'Value') %>%
  rename(DATE = Date)

# inflacja rok do roku (https://stats.oecd.org)
inflacja <-
  read.csv(file = "data/oecd/inflation/KEI_23042023223303307.csv") %>%
  transmute(Country,
            Measure,
            Date = as.Date(paste0(TIME, "-01")),
            Value = round(Value / 100, 4)) %>%
  pivot_wider(names_from = 'Country', values_from = 'Value') %>%
  rename(DATE = Date) %>% 
  filter(Measure == "Growth on the same period of the previous year") %>%
  select(!Measure)

# polska gielda https://stooq.pl/q/?s=wig&c=5y&t=l&a=ln&b=0
polska <-
  read.csv("./data/stooq/world/custom/wig_d.csv") %>%
  transmute(
    DATE = as.Date(Data),
    TICKER = "^WIG",
    VALUE = (as.numeric(Otwarcie) + as.numeric(Zamkniecie)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# niemiecka gielda https://stooq.pl/q/?s=^dax&c=5y&t=l&a=ln&b=0
niemcy <-
  read_table("./data/stooq/world/indices/^dax.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# francuska gielda https://stooq.pl/q/?s=^cac&c=5y&t=l&a=ln&b=0
francja <-
  read_table("./data/stooq/world/indices/^cac.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# brytyjska gielda https://stooq.pl/q/?s=^ukx&c=5y&t=l&a=ln&b=0
wlk_bryt <-
  read_table("./data/stooq/world/indices/^ukx.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# amerykanska gielda https://stooq.pl/q/?s=^spx&c=5y&t=l&a=ln&b=0
usa <-
  read_table("./data/stooq/world/indices/^spx.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# finlandzka gielda https://stooq.pl/q/?s=^hex&c=5y&t=l&a=ln&b=0
finlandia <-
  read_table("./data/stooq/world/indices/^hex.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# ukrainska gielda https://stooq.pl/q/?s=^ux&c=5y&t=l&a=ln&b=0
ukraina <-
  read_table("./data/stooq/world/indices/^ux.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# rosyjska gielda https://stooq.pl/q/?s=^rts&c=5y&t=l&a=ln&b=0
rosja <-
  read_table("./data/stooq/world/indices/^rts.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# turecka gielda https://stooq.pl/q/?s=^xu100&c=5y&t=l&a=ln&b=0
turcja <-
  read_table("./data/stooq/world/indices/^xu100.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# chinska gielda https://stooq.pl/q/?s=^shc&c=5y&t=l&a=ln&b=0
chiny <-
  read_table("./data/stooq/world/indices/^shc.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# kurs bitcoina https://stooq.pl/q/?s=btc.v&c=5y&t=l&a=ln&b=0
bitcoin <-
  read_table("./data/stooq/world/cryptocurrencies/major/BTC.V.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

# kurs ethernum https://stooq.pl/q/?s=eth.v&c=5y&t=l&a=ln&b=0
ethernum <-
  read_table("./data/stooq/world/cryptocurrencies/major/ETH.V.txt") %>%
  separate_wider_delim(
    cols = '<TICKER>,<PER>,<DATE>,<TIME>,<OPEN>,<HIGH>,<LOW>,<CLOSE>,<VOL>,<OPENINT>',
    delim = ',',
    names = c(
      'TICKER',
      'PER',
      'DATE',
      'TIME',
      'OPEN',
      'HIGH',
      'LOW',
      'CLOSE',
      'VOL',
      'OPENINT'
    )
  ) %>% 
  transmute(
    DATE = as.Date(DATE, "%Y%m%d"),
    TICKER,
    VALUE = (as.numeric(OPEN) + as.numeric(CLOSE)) / 2
  ) %>%
  filter(DATE >= "2018-01-01") %>%
  pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
  drop_na() %>%
  as.data.frame()

