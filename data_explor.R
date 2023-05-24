# BIBLIOTEKI ===========================================================================================================
library(dplyr)
library(tidyr)
library(readxl)
library(plotly)


# DANE =================================================================================================================
df_all <- read_excel("data/prepared/df_all.xlsx") %>%
  mutate(DATE = as.Date(DATE)) %>%
  fill(everything(), .direction = c("down"))

min_max <- function(x) {
  return((x - min(x, na.rm = TRUE)) /
           (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
}

df_all_norm <-
  df_all %>%
  mutate_if(is.numeric, min_max)

df_all_norm_smooth <-
  df_all_norm %>%
  mutate_if(is.numeric, ~ zoo::rollmean(., k = 7, fill = NA))

# WIZUALIZACJA =========================================================================================================
# Naiwny sposób doboru najbardziej skorelowanych zmiennych
wskaznik <- "wig_indices_custom"

df_all_norm %>%
  select(!DATE) %>%
  drop_na() %>%
  cor() %>%
  as.data.frame() %>%
  select(as.name(wskaznik)) %>%
  dplyr::arrange(desc(.)) %>%
  head(20)

# Dobranie wskaznikow do porownania
comp1 <- "^_DE_indices_stooq"
comp2 <- "^_UK_indices_stooq"
comp3 <- "^_HU_indices_stooq"

# Interaktywny wykres
plot_ly(
  df_all_norm_smooth,
  x = ~ DATE,
  y = ~ get(as.name(wskaznik)),
  # Unquoting the variable name (chatGPT),
  type = 'scatter',
  mode = 'lines',
  name = wskaznik
) %>% # The World Health Organization declared the COVID-19 outbreak
  add_lines(
    y = range(df_all_norm_smooth[[wskaznik]], na.rm = TRUE),
    x = "2020-01-30",
    line = list(color = "pink", dash = 'dash'),
    inherit = FALSE,
    showlegend = TRUE,
    name = "COVID-19 outbreak start"
  ) %>% # The World Health Organization declared the COVID-19 to be pandemic
  add_lines(
    y = range(df_all_norm_smooth[[wskaznik]], na.rm = TRUE),
    x = "2020-03-11",
    line = list(color = "pink", dash = 'dashdot'),
    inherit = FALSE,
    showlegend = TRUE,
    name = "COVID-19 pandemy start"
  ) %>% # Russian aggression against Ukraine
  add_lines(
    y = range(df_all_norm_smooth[[wskaznik]], na.rm = TRUE),
    x = "2022-02-24",
    line = list(color = "red", dash = 'dash'),
    inherit = FALSE,
    showlegend = TRUE,
    name = "Russian invasion"
  ) %>% # Bankruptcy of FTX
  add_lines(
    y = range(df_all_norm_smooth[[wskaznik]], na.rm = TRUE),
    x = "2022-11-07",
    line = list(color = "grey", dash = 'dash'),
    inherit = FALSE,
    showlegend = TRUE,
    name = "FTX bankruptcy"
  ) %>% # wskazniklapse of Silicon Valley Bank
  add_lines(
    y = range(df_all_norm_smooth[[wskaznik]], na.rm = TRUE),
    x = "2023-03-10",
    line = list(color = "grey", dash = 'dot'),
    inherit = FALSE,
    showlegend = TRUE,
    name = "SVB bankrupcy"
  ) %>%
  add_trace(
    y = ~ get(as.name(comp1)),
    type = 'scatter',
    mode = 'lines',
    showlegend = TRUE,
    name = comp1
  )  %>%
  add_trace(
    y = ~ get(as.name(comp2)),
    type = 'scatter',
    mode = 'lines',
    showlegend = TRUE,
    name = comp2
  )  %>%
  add_trace(
    y = ~ get(as.name(comp3)),
    type = 'scatter',
    mode = 'lines',
    showlegend = TRUE,
    name = comp3
  ) %>%
  layout(
    showlegend = TRUE,
    yaxis = list(visible = FALSE),
    legend = list(orientation = "h"),
    title = list(text = "Porównanie wskaźników (wygładzonych oraz znormalizowanych do przedziału [0,1])")
  )

