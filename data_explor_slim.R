library(dplyr)
library(tidyr)
library(readxl)
library(openxlsx)
library(ggplot2)

df_all_slim <-
  read_excel("data/prepared/df_all_slim.xlsx") %>%
  mutate(DATE = as.Date(DATE))

# sprawdzmy korelacje - metoda Pearsona (moze byc mylaca jezeli wartosci indeksow sa na roznych skalach)
df_corr_pearson <-
  df_all_slim %>%
  # "odwybierzmy" kolumne dat
  select(!DATE) %>%
  # usunmy puste wartosci
  drop_na() %>%
  cor(method = "pearson")

# sprawdzmy korelacje - metoda Spearmana (metoda rang, tj. sprawdza czy wystepuja podobne wzrosty i spadki)
df_corr_spearman <-
  df_all_slim %>%
  # "odwybierzmy" kolumne dat
  select(!DATE) %>%
  # usunmy puste wartosci
  drop_na() %>%
  cor(method = "spearman")

# porownanie wizualne
colnames(df_all_slim)
# [1] "DATE"                                       "^BTC"                                       "^ETH"
# [4] "^WIG"                                       "^WIG20"                                     "^PX"
# [7] "^OMXT"                                      "^BUX"                                       "^DAX"
# [10] "^CAC"                                       "^UKX"                                       "^SPX"
# [13] "^HEX"                                       "^UX"                                        "^RTS"
# [16] "^XU100"                                     "^SHC"                                       "intrate_Poland"
# [19] "intrate_Germany"                            "intrate_France"                             "intrate_United Kingdom"
# [22] "intrate_United States"                      "intrate_Finland"                            "intrate_Czech Republic"
# [25] "intrate_Estonia"                            "intrate_Hungary"                            "intrate_China (People's Republic of)"
# [28] "inflation_yy_Poland"                        "inflation_yy_Czech Republic"                "inflation_yy_Estonia"
# [31] "inflation_yy_Hungary"                       "inflation_yy_Germany"                       "inflation_yy_France"
# [34] "inflation_yy_United Kingdom"                "inflation_yy_United States"                 "inflation_yy_Finland"
# [37] "inflation_yy_Russia"                        "inflation_yy_Türkiye"                       "inflation_yy_China (People's Republic of)"
# [40] "inflation_acc_Poland"                       "inflation_acc_Czech Republic"               "inflation_acc_Estonia"
# [43] "inflation_acc_Hungary"                      "inflation_acc_Germany"                      "inflation_acc_France"
# [46] "inflation_acc_United Kingdom"               "inflation_acc_United States"                "inflation_acc_Finland"
# [49] "inflation_acc_Russia"                       "inflation_acc_Türkiye"                      "inflation_acc_China (People's Republic of)"

# poniewaz sprawdzamy wartosci nominalne, dla realnego poziomu rynkow, nalezy je zdyskontowac (poza kryptowalutami)
# realny wskaznik = (nominalny wskaznik)*(1 - % wartosc skumulowanej inflacji)
df_all_slim_real <-
  df_all_slim %>%
  transmute(
    DATE,
    `^BTC`,
    `^ETH`,
    `^WIG_real` = `^WIG` * (1 - inflation_acc_Poland),
    `^WIG20_real` = `^WIG20` * (1 - inflation_acc_Poland),
    `^PX_real` = `^PX` * (1 - `inflation_acc_Czech Republic`),
    `^OMXT_real` = `^OMXT` * (1 - inflation_acc_Estonia),
    `^BUX_real` = `^BUX` * (1 - inflation_acc_Hungary),
    `^DAX_real` = `^DAX` * (1 - inflation_acc_Germany),
    `^CAC_real` = `^CAC` * (1 - inflation_acc_France),
    `^UKX_real` = `^UKX` * (1 - `inflation_acc_United Kingdom`),
    `^SPX_real` = `^SPX` * (1 - `inflation_acc_United States`),
    `^HEX_real` = `^HEX` * (1 - inflation_acc_Finland),
    # `^UX_real` = `^UX` * (1 - inflation_acc_Ukraine), # brak danych
    `^RTS_real` = `^RTS` * (1 - inflation_acc_Russia),
    `^XU100_real` = `^XU100` * (1 - inflation_acc_Türkiye),
    `^SHC_real` = `^SHC` * (1 - `inflation_acc_China (People's Republic of)`)
  )

ggplot(df_all_slim_real, aes(x = DATE)) +
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
  labs(title = "Porównanie wskaźników (przy uwzglednieniu inflacji)",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = df_all_slim$`^WIG`), color = "black") +
  geom_line(aes(y = `^WIG_real`), color = "blue") # jest roznica ...
# Na oko, rynek zdecydowanie odczul covid, a nastepnie wojne

# Jezeli chcemy porownywac tempo zmian rynkow to musimy znormalizowac dane do przedzialu [0,1]
# norm_minmax() https://medium.com/swlh/data-normalisation-with-r-6ef1d1947970
min_max <- function(x) {
  return((x - min(x, na.rm = TRUE)) /
           (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

df_all_slim_norm <-
  df_all_slim_real %>%
  mutate_if(is.numeric, min_max)

ggplot(df_all_slim_norm, aes(x = DATE)) +
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
  labs(title = "Porównanie realnych wskaźników (znormalizowanych do przedziału [0,1])",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^WIG_real`), color = "black") +
  geom_line(aes(y = `^DAX_real`), color = "blue") +
  geom_line(aes(y = `^SPX_real`), color = "violet")

ggplot(df_all_slim_norm, aes(x = DATE)) +
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
  labs(title = "Porównanie realnych wskaźników (znormalizowanych do przedziału [0,1])",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^BTC`), color = "black") +
  geom_line(aes(y = `^ETH`), color = "blue")

# zapis do pliku
write.xlsx(
  x = df_all_slim_norm,
  file = "./data/prepared/df_all_slim_norm.xlsx",
  overwrite = TRUE,
  rowNames = FALSE
)

write.xlsx(
  x = df_all_slim_real,
  file = "./data/prepared/df_all_slim_real.xlsx",
  overwrite = TRUE,
  rowNames = FALSE
)
