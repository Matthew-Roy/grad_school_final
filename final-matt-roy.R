library(tidyverse)
library(sf)
library(rje)
library(tidycensus)
library(pdftools)
library(tidytext)
library(scales)
library(ggpmisc)
library(stargazer)

csv_files <- list.files(pattern = "1.csv$")

raw_data <- list()

for (i in seq_along(csv_files)) {
  raw_data[[i]] <- read_csv(csv_files[i])
}

file_names <- c("charger_data", "ev_data", "seattle_zips")

for (i in seq_along(file_names)) {
  assign(file_names[i], raw_data[[i]])
}

king_co_shape <- st_read("Zip_Codes.shp")
# Data: https://data-seattlecitygis.opendata.arcgis.com/datasets/zip-codes
# zip codes sourced from http://www.city-data.com/zipmaps/Seattle-Washington.html#:~:text=Zip%20codes%3A%2098101%2C%2098102%2C,%2C%2098178%2C%2098195%2C%2098199.
seattle_shape <- seattle_zips %>%
  left_join(king_co_shape, by = c("zip" = "ZIP"))

zips <- pull(seattle_zips$zip)
seattle_battery_ev <- ev_data %>%
  filter(
    `Electric Vehicle Type` == "Battery Electric Vehicle (BEV)" &
      `ZIP Code` %in% seattle_zips$zip &
      City == "SEATTLE"
  ) %>%
  map_dfr(~ str_remove_all(
    ., pattern = "^POINT \\(|\\)$")
    ) %>%
  separate(
    col = `Vehicle Location`,
    into = c("longitude", "latitude"),
    sep = " "
  ) %>%
  st_as_sf(
    coords = c("longitude", "latitude"),
    crs = 4326
  )

seattle_chargers <- charger_data %>%
  filter(
    ZIP %in% seattle_zips$zip &
      `Groups With Access Code` == "Public"
  ) %>%
  st_as_sf(
    coords = c("Longitude", "Latitude"),
    crs = 4326
  )

distance_matrix <- st_distance(
  seattle_battery_ev$geometry,
  seattle_chargers$geometry
)

seattle_battery_ev$nearest_ev_dist_m <-
  rowMins(distance_matrix)
# Distance Matrix solution sourced from Peter Herman at NORC

zip_evs <- seattle_battery_ev %>%
  group_by(`ZIP Code`) %>%
  summarise(
    avg_dist = mean(nearest_ev_dist_m),
    ev_count = n()
  ) %>%
  tibble(.) %>%
  select(-geometry) %>%
  mutate_at(vars(`ZIP Code`), as.numeric)

CENSUS_KEY <- Sys.getenv("CENSUS_API_KEY")
acs_vars <- load_variables(2019, "acs5", cache = TRUE)
acs_2019_raw <- get_acs(
  geography = "zcta",
  state = "WA",
  year = 2019,
  variables = c(
    total_population = "B01003_001",
    wht_pop = "B01001A_001",
    blck_pop = "B01001B_001",
    med_income = "B06011_001"
  ),
  survey = "acs5",
  geometry = FALSE
)

#write_csv(acs_2019_raw, "wa_acs_2019.csv")
# Use if you wish to skip ACS API above
# acs_2019_raw <- read_csv("wa_acs_2019.csv")

seattle_acs_2019 <- acs_2019_raw %>%
  mutate_at(vars(GEOID), as.numeric) %>%
  semi_join(seattle_zips, by = c("GEOID" = "zip")) %>%
  select(-moe) %>%
  pivot_wider(
    names_from = "variable",
    values_from = "estimate"
  ) %>%
  mutate(
    non_bw_pop = total_population - (wht_pop + blck_pop),
    wht_share = wht_pop / total_population,
    blck_share = blck_pop / total_population,
    non_bw_share = non_bw_pop / total_population
  )

complete_seattle <- seattle_acs_2019 %>%
  left_join(zip_evs, by = c("GEOID" = "ZIP Code")) %>%
  mutate(
    ev_per_k = (ev_count / total_population)*1000
  )

# I used pdftools during my internship this summer, same tools used here.
raw_text <- pdf_text("Seattle-charging infra-jan2021.pdf")
pages <- c(1:21)

seattle_text <- tibble(text = raw_text) %>%
  mutate(page = 1:length(raw_text)) %>%
  filter(page %in% pages)

tokens <- seattle_text %>%
  select(-page) %>%
  unnest_tokens(
    word_token,
    text,
    token = "words"
  ) %>%
  anti_join(
    stop_words,
    by = c("word_token" = "word")
  ) %>%
  group_by(word_token) %>%
  summarise(
    n = n()
  ) %>%
  ungroup()

sentiments <- c("nrc", "afinn", "bing")
sentiment_list <- list()

for (i in seq_along(sentiments)) {
  sentiment_list[[i]] <- get_sentiments(sentiments[i])
}

sentiment_master <- sentiment_list %>%
  reduce(full_join, by = "word") %>%
  rename(
    nrc = sentiment.x,
    afinn = value,
    bing = sentiment.y
  )

token_sentiment <- tokens %>%
  left_join(
    sentiment_master,
    by = c("word_token" = "word")
  ) %>%
  mutate_at(
    vars("nrc", "afinn", "bing"),
    as.character
  ) %>%
  pivot_longer(
    cols = c("nrc", "afinn", "bing"),
    names_to = "sentiment",
    values_to = "val"
  ) %>%
  filter(!is.na(val)) %>%
  mutate(row = row_number()) %>%
  pivot_wider(
    names_from = "sentiment",
    values_from = "val"
  ) %>%
  select(-row) %>%
  distinct() %>%
  mutate_at(
    vars("afinn"),
    as.double
  )

plot_seattle <- seattle_shape %>%
  select(c(zip, geometry)) %>%
  left_join(complete_seattle, by = c("zip" = "GEOID")) %>%
  mutate_at(vars(avg_dist), as.numeric) %>%
  st_as_sf()

build_choro <- function(fill, cap) {
  if (fill == "ev_per_k") {
    title <- "EVs per 1000"
    leg <- "EVs/1000"
  }
  else {
    title <- "Nearest Public Charger (Avg. m)"
    leg <- "Distance (m)"
  }

  plot_seattle %>%
    ggplot(aes_string(
      fill = fill
    )) +
    geom_sf(
      data = king_co_shape$geometry,
      fill = "cornsilk",
      color = "cornsilk",
      inherit.aes = FALSE
    ) +
    geom_sf(color = "white") +
    coord_sf(xlim = c(-122.45, -122.2), ylim = c(47.48, 47.77)) +
    theme(
      panel.background = element_rect(fill = "skyblue2"),
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.ticks = element_blank()
    ) +
    scale_fill_gradient(
      low = "#ABD6B0",
      high = "#1F5925"
    ) +
    labs(
      title = title,
      fill = leg,
      caption = str_c("Data Source: ", cap, sep = "") 
    )
}

choro_vars <- c("ev_per_k", "avg_dist")

for (i in choro_vars) {
  if(i == "ev_per_k"){
    cap <- "State of Washington"
  }
  else{
    cap <- "U.S. Dept. of Energy"
  }
  plot <- build_choro(i, cap)
  print(plot)
}

build_sent_plot <- function(z) {
  token_sentiment %>%
    filter_at(vars(z), any_vars(!is.na(.))) %>%
    ggplot() +
    geom_bar(
      aes_string(
        x = z,
        y = "..prop..",
        group = 1
      ),
      fill = "seagreen4",
      color = "gray44",
      alpha = 0.6
    ) +
    theme(
      axis.title.y = element_blank(),
      panel.background = element_blank(),
      panel.grid.major.y = element_line(
        color = "black",
        linetype = "dashed"
      )
    ) +
    scale_y_continuous(
      labels = label_percent(
        accuracy = 1
      )
    ) +
    labs(
      title = "2021 Seattle ICCT Report",
      subtitle = str_c("Sentiment Analysis with ",
        str_to_title(z),
        sep = ""
      ),
      caption = "Source = International Council on 
      Clean Transportation",
      x = str_to_title(z)
    )
}

for (i in c("bing", "afinn")) {
  plot <- build_sent_plot(i)
  print(plot)
}

# Preserved to show that csv file was written to simplify Shiny
# write_csv(plot_seattle, "shiny_data.csv")

reg_model <- lm(avg_dist ~ blck_share,
  data = plot_seattle
)
stargazer(reg_model, type = "text")
