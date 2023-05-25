library(dplyr)
library(tidyr)
library(readxl)
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
# [1] "DATE"                     "^BTC"                     "^ETH"
# [4] "^WIG"                     "^WIG20"                   "^DAX"
# [7] "^CAC"                     "^UKX"                     "^SPX"
# [10] "^HEX"                     "^UX"                      "^RTS"
# [13] "^XU100"                   "^SHC"                     "intrate_Poland"
# [16] "intrate_Germany"          "intrate_France"           "intrate_United Kingdom"
# [19] "intrate_United States"    "intrate_Finland"          "intrate_Russia"
# [22] "inflation_Poland"         "inflation_Germany"        "inflation_France"
# [25] "inflation_United Kingdom" "inflation_United States"  "inflation_Finland"
# [28] "inflation_Russia"         "inflation_Türkiye"

ggplot(df_all_slim, aes(x = DATE)) +
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
  labs(title = "Porównanie wskaźników",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^WIG`), color = "black") +
  geom_line(aes(y = `^WIG20`), color = "blue") # PROBLEM SKAL

# znormalizujmy przed porownaniem - chatGPT
min_max <- function(x) {
  return((x - min(x, na.rm = TRUE)) /
           (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

df_all_slim_norm <-
  df_all_slim %>%
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
  labs(title = "Porównanie wskaźników (znormalizowanych do przedziału [0,1])",
       x = "DATE",
       y = NULL) +
  geom_line(aes(y = `^WIG`), color = "black") +
  geom_line(aes(y = `^WIG20`), color = "blue") +
  geom_line(aes(y = inflation_Poland), color = "dark green")

