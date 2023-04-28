library(readr)
library(tigris)
library(leaflet)
library(leaflet.providers)
library(dplyr)
library(janitor)

us_states <- states(cb = TRUE)

senate_les <- read_csv("CELSenate117LES2.csv") %>% 
  clean_names() %>%
  select(c('two_letter_state_abbreviation', 'les_2_0')) %>%
  group_by(two_letter_state_abbreviation) %>%
  mutate(les_avg = mean(les_2_0, na.rm = TRUE)) %>%
  distinct(two_letter_state_abbreviation, les_avg) %>%
  rename(STUSPS = two_letter_state_abbreviation)

states_and_les <- merge(us_states, senate_les, by.x = "STUSPS")

hovertext = paste0(
  "State: ", states_and_les$NAME, "<br/>",
  "Average LES: ", states_and_les$les_avg, "<br/>"
) %>%
  lapply(htmltools::HTML)

colorbin <- ~colorBin("YlOrRd", les_avg)(les_avg)

pal <- colorBin(palette = "YlOrRd", domain = states_and_les$les_avg)

leaflet(states_and_les) %>%
  addTiles() %>%
  setView(-98.5795, 39.8282, zoom=3) %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolygons(fillOpacity = 0.5, smoothFactor = 0.5, fillColor = colorbin,
              color = "black", opacity = 1, weight = 0.7, label = hovertext) %>%
  addLegend("topright", pal = pal, values = ~les_avg, title = "Average LES of the 117th Senate")