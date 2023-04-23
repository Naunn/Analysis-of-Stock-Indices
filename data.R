# BIBLIOTEKI ===========================================================================================================
library(dplyr)    # %>%; select(); transmute(); filter(); glimpse();
library(purrr)    # set_names(); map_df();
library(readr)    # read_table(); separate_wider_delim();
library(tidyr)    # pivot_wider(); drop_na();
library(corrplot) # corrplot();
library(ggplot2)

# FUNKCJE ==============================================================================================================
# Read all the files and create a FileName column to store filenames
# https://stackoverflow.com/questions/3397885/how-do-you-read-multiple-txt-files-into-r

load_data <- function(path, date) {
  list_of_files <-
    list.files(
      path = path,
      recursive = TRUE,
      pattern = "\\.txt$",
      full.names = TRUE
    )
  
  df <-
    list_of_files %>%
    set_names(.) %>%
    map_df(read_table, .id = "FileName") %>%
    select(!FileName) %>%
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
    filter(DATE >= date) %>%
    pivot_wider(names_from = 'TICKER', values_from = 'VALUE') %>%
    drop_na()
  
  return(df)
}

# DANE =================================================================================================================
## OBLIGACJE SKARBOWE (10-LETNIE) ======================================================================================
df_bonds <-
  load_data(path = "./data/stooq/world/bonds",
            date = "2018-01-01")
df_bonds %>% glimpse()
# Rows: 808
# Columns: 29
# $ DATE      <date> 2018-01-04, 2018-01-05, 2018-01-09, 2018-01-10, 2018-01-11, …
# $ `10ATY.B` <dbl> 0.6060, 0.6085, 0.6095, 0.6250, 0.6485, 0.6780, 0.6660, 0.659…  #AUSTRIA
# $ `10AUY.B` <dbl> 2.6635, 2.6270, 2.6430, 2.7050, 2.7200, 2.7275, 2.7595, 2.750…  #AUSTRALIA
# $ `10BEY.B` <dbl> 0.66015, 0.65580, 0.66840, 0.69050, 0.70550, 0.73750, 0.70100…  #BELGIUM
# $ `10CAY.B` <dbl> 2.0675, 2.1235, 2.1775, 2.2110, 2.1785, 2.1760, 2.1800, 2.178…  #CANADA
# $ `10CHY.B` <dbl> -0.0965, -0.0955, -0.0895, -0.0600, -0.0340, 0.0050, -0.0025,…  #SWITZERLAND
# $ `10CNY.B` <dbl> 3.9290, 3.9365, 3.9220, 3.9250, 3.9440, 3.9915, 4.0195, 4.027…  #CHINA
# $ `10CZY.B` <dbl> 1.6800, 1.6690, 1.6510, 1.6890, 1.6805, 1.7335, 1.7540, 1.771…  #CZECH
# $ `10DEY.B` <dbl> 0.4425, 0.4430, 0.4500, 0.4760, 0.4865, 0.5160, 0.5660, 0.560…  #GERMANY
# $ `10ESY.B` <dbl> 1.5700, 1.5315, 1.4980, 1.5355, 1.5430, 1.5195, 1.5030, 1.502…  #SPAIN
# $ `10FIY.B` <dbl> 0.6080, 0.6035, 0.6085, 0.6275, 0.6525, 0.6890, 0.6650, 0.656…  #FINLAND
# $ `10FRY.B` <dbl> 0.80170, 0.79715, 0.80300, 0.82650, 0.83300, 0.86450, 0.85350…  #FRANCE
# $ `10GRY.B` <dbl> 3.9240, 3.7770, 3.6635, 3.6945, 3.7850, 3.8810, 3.8195, 3.757…  #GREECE
# $ `10HUY.B` <dbl> 2.070, 2.000, 2.000, 2.020, 2.025, 2.030, 2.050, 2.040, 2.075…  #HUNGARY
# $ `10IDY.B` <dbl> 6.1900, 6.1015, 6.0745, 6.1185, 6.1445, 6.0980, 6.0785, 6.080…  #INDONESIA
# $ `10INY.B` <dbl> 7.3300, 7.3155, 7.3570, 7.4060, 7.4400, 7.4460, 7.5020, 7.488…  #INDIA
# $ `10ITY.B` <dbl> 2.0395, 2.0105, 2.0095, 2.0385, 2.0390, 2.0170, 1.9695, 1.984…  #ITALY
# $ `10JPY.B` <dbl> 0.0520, 0.0565, 0.0635, 0.0830, 0.0730, 0.0725, 0.0775, 0.082…  #JAPAN
# $ `10KRY.B` <dbl> 2.5145, 2.5240, 2.5550, 2.6275, 2.6035, 2.6145, 2.6485, 2.638…  #SOUTH KOREA
# $ `10MYY.B` <dbl> 3.9230, 3.9050, 3.8810, 3.8945, 3.8845, 3.8685, 3.8535, 3.864…  #MALAYSIA
# $ `10NLY.B` <dbl> 0.5420, 0.5420, 0.5475, 0.5745, 0.5925, 0.6245, 0.6020, 0.596…  #NETHERLANDS
# $ `10PLY.B` <dbl> 3.2780, 3.2320, 3.2665, 3.3265, 3.3305, 3.3375, 3.2910, 3.280…  #POLAND
# $ `10PTY.B` <dbl> 1.9455, 1.9220, 1.8570, 1.8495, 1.8200, 1.8070, 1.7770, 1.791…  #PORTUGAL
# $ `10ROY.B` <dbl> 4.4050, 4.3950, 4.3800, 4.3350, 4.3100, 4.3150, 4.3050, 4.310…  #ROMANIA
# $ `10RUY.B` <dbl> 7.520, 7.505, 7.505, 7.485, 7.500, 7.455, 7.420, 7.425, 7.420…  #RUSSIA
# $ `10SGY.B` <dbl> 2.0305, 2.0265, 2.0690, 2.1000, 2.1050, 2.1140, 2.0785, 2.087…  #SINGAPORE
# $ `10TRY.B` <dbl> 11.465, 11.445, 11.400, 11.450, 11.470, 11.460, 11.665, 11.73…  #TURKEY
# $ `10UKY.B` <dbl> 1.2405, 1.2355, 1.2690, 1.2875, 1.2855, 1.3270, 1.3100, 1.304…  #UNITED KINGDOM
# $ `10USY.B` <dbl> 2.4589, 2.4690, 2.5238, 2.5605, 2.5440, 2.5515, 2.5390, 2.573…  #USA

plot(x = df_bonds$DATE,
     y = df_bonds$`10PLY.B`,
     type = 'l')

corr_M <- cor(df_bonds %>% select(!DATE))
corrplot(corr_M, method = "number")

## WIODACE WALUTY ======================================================================================================
df_curr <-
  load_data(path = "./data/stooq/world/currencies/major",
            date = "2018-01-01")
df_curr %>% colnames()
# [1] "DATE"   "AUDCAD" "AUDCHF" "AUDEUR" "AUDGBP" "AUDJPY" "AUDPLN" "AUDUSD"
# [9] "CADAUD" "CADCHF" "CADEUR" "CADGBP" "CADJPY" "CADPLN" "CADUSD" "CHFAUD"
# [17] "CHFCAD" "CHFEUR" "CHFGBP" "CHFJPY" "CHFPLN" "CHFUSD" "EURAUD" "EURCAD"
# [25] "EURCHF" "EURGBP" "EURJPY" "EURPLN" "EURUSD" "GBPAUD" "GBPCAD" "GBPCHF"
# [33] "GBPEUR" "GBPJPY" "GBPPLN" "GBPUSD" "JPYAUD" "JPYCAD" "JPYCHF" "JPYEUR"
# [41] "JPYGBP" "JPYPLN" "JPYUSD" "NZDUSD" "USDAUD" "USDCAD" "USDCHF" "USDEUR"
# [49] "USDGBP" "USDJPY" "USDPLN" "XAGAUD" "XAGCAD" "XAGCHF" "XAGEUR" "XAGGBP"
# [57] "XAGJPY" "XAGPLN" "XAGUSD" "XAUAUD" "XAUCAD" "XAUCHF" "XAUEUR" "XAUGBP"
# [65] "XAUJPY" "XAUPLN" "XAUUSD"

# AUD - DOLAR AUSTRALIJSKI
# CAD - DOLAR KANADYJSKI
# CHF - FRANK SZWAJCARSKI
# EUR - EURO
# GBP - FUNT BRYTYJSKI
# JPY - JEN
# PLN - ZLOTOWKA
# USD - DOLAR AMERYKANSKI
# NZD - DOLAR NOWOZELANDZKI
# XAU - ZLOTO
# XAG - SREBRO

plot(x = df_curr$DATE,
     y = df_curr$XAUPLN,
     type = 'l')

plot(x = df_curr$DATE,
     y = df_curr$XAGPLN,
     type = 'l')

## KRYPTOWALUTY ========================================================================================================
# https://www.bankrate.com/investing/types-of-cryptocurrency/
df_crypto <-
  load_data(path = "./data/stooq/world/cryptocurrencies/major",
            date = "2018-01-01")
df_crypto %>% colnames()
# [1] "DATE"   "ADA.V"  "BNB.V"  "BTC.V"  "DOGE.V" "ETH.V"  "USDT.V" "XRP.V"
#ADA.V - Cardano
#BNB.V - BNB
#BTC.V - Bitcoin
#DOGE.V - Dogecoin
#ETH.V - Ethereum
#USDT.V - Tether
#XRP.V - XRP

plot(x = df_crypto$DATE,
     y = df_crypto$BTC.V,
     type = 'l')

plot(x = df_crypto$DATE,
     y = df_crypto$ETH.V,
     type = 'l')

corr_M <- cor(df_crypto %>% select(!DATE))
corrplot(corr_M, method = "number")

## INDEKSY =============================================================================================================
df_indices <-
  load_data(path = "./data/stooq/world/indices",
            date = "2018-01-01")
df_indices %>% colnames()
# [1] "DATE"   "^AEX"   "^AOR"   "^ATH"   "^BEL20" "^BET"   "^BUX"   "^BVP"   "^CAC"   "^CDAX" 
# [11] "^CRY"   "^DAX"   "^DJC"   "^DJI"   "^DJT"   "^DJU"   "^FMIB"  "^FTM"   "^HEX"   "^HSI"  
# [21] "^IBEX"  "^ICEX"  "^IPC"   "^IPSA"  "^JCI"   "^KLCI"  "^KOSPI" "^MDAX"  "^MOEX"  "^MRV"  
# [31] "^MT30"  "^NDQ"   "^NDX"   "^NKX"   "^NOMUC" "^NZ50"  "^OMXR"  "^OMXS"  "^OMXT"  "^OMXV" 
# [41] "^OSEAX" "^PSEI"  "^PSI20" "^PX"    "^RTS"   "^SAX"   "^SDXP"  "^SET"   "^SHBS"  "^SHC"  
# [51] "^SMI"   "^SNX"   "^SOFIX" "^SPX"   "^STI"   "^TASI"  "^TDXP"  "^TSX"   "^TWSE"  "^UKX"  
# [61] "^UX"    "^XU100"

plot(x = df_indices$DATE,
     y = df_indices$`^UKX`,
     type = 'l')

## INDEKSY (main stooq.pl) =============================================================================================
# Stooq [...] All Stocks Price Index
df_indices_stooq <-
  load_data(path = "./data/stooq/world/stooq stocks indices",
            date = "2018-01-01") %>% 
  select(!c("^_PL20", "^_PLNC", "^_PLWS"))
df_indices_stooq %>% colnames()
# [1] "DATE"   "^_DE"   "^_HK"   "^_HU"   "^_JP"   "^_PL"   "^_UK"   "^_US"  
# [9] "^_USNM" "^_USNQ" "^_USNS"
# ^_DE - Germany
# ^_HK- ?
# ^_HU - Hungary
# ^_JP - Japan
# ^_PL - Poland
# ^_UK - United Kingdom
# ^_US - United States
# ^_USNM - ?
# ^_USNQ - US Nasdaq (Nasdaq Stock Market)
# ^_USNS - US NYSE (The New York Stock Exchange)

plot(x = df_indices_stooq$DATE,
     y = df_indices_stooq$`^_UK`,
     type = 'l')

## STOPY PROCENTOWE ====================================================================================================
# https://stats.oecd.org
df_intrate <-
  read.csv(file = "data/oecd/interest rate/MEI_FIN_23042023220434720.csv") %>%
  filter(Subject == "Long-term interest rates, Per cent per annum") %>%
  transmute(Country,
            Date = as.Date(paste0(TIME, "-01")),
            Value = round(Value / 100, 4)) %>%
  pivot_wider(names_from = 'Country', values_from = 'Value')

plot(x = df_intrate$Date,
     y = df_intrate$Poland,
     type = 'l')

## INFLACJA ============================================================================================================
# https://stats.oecd.org
df_inflation <-
  read.csv(file = "data/oecd/inflation/KEI_23042023223303307.csv") %>%
  transmute(Country,
            Measure,
            Date = as.Date(paste0(TIME, "-01")),
            Value = round(Value / 100, 4)) %>%
  pivot_wider(names_from = 'Country', values_from = 'Value')

df_inflation_MAM <-
  df_inflation %>%
  filter(Measure == "Growth previous period") %>% 
  select(!Measure)

df_inflation_YOY <-
  df_inflation %>%
  filter(Measure == "Growth on the same period of the previous year") %>% 
  select(!Measure)

plot(x = df_inflation_YOY$Date,
     y = df_inflation_YOY$Poland,
     type = 'l')
