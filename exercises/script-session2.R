#remotes::install_github("runapp-aus/strayr")
library(strayr)
library(dplyr)
library(cubble)
library(sf)
library(ozmaps)
library(rmapshaper)
library(ggplot2)

#############Importing COVID data
load(here::here("data/covid.rda"))
head(covid)

#remotes::install_github("runapp-aus/strayr")
lga <- strayr::read_absmap("lga2018") |>
  rename(lga = lga_name_2018) |>
  filter(state_name_2016 == "Victoria")
head(lga)

(cb <- as_cubble(
  list(spatial = lga, temporal = covid),
  key = lga, index = date, coords = c(cent_long, cent_lat)))

(pair <- as_cubble(
  list(spatial = lga, temporal = covid),
  key = lga, index = date, coords = c(cent_long, cent_lat),
  output = "unmatch"))

lga <- lga |>
  mutate(lga = ifelse(lga == "Kingston (C) (Vic.)", "Kingston (C)", lga),
         lga = ifelse(lga == "Latrobe (C) (Vic.)", "Latrobe (C)", lga)) |>
  filter(!lga %in% pair$others$spatial)

covid <- covid |> filter(!lga %in% pair$others$temporal)

(cb <- as_cubble(
  data = list(spatial = lga, temporal = covid),
  key = lga, index = date, coords = c(cent_long, cent_lat)))



#############Making glyph map
stations_sf <- cubble::climate_subset %>% select(-ts) %>% 
  sf::st_as_sf(coords = c("long", "lat"), crs = 4283, remove = FALSE)

ts <- cubble::climate_subset %>% 
  face_temporal() %>% 
  filter(!is.na(tmax), !is.na(tmin)) %>% 
  as_tibble()

oz <- ozmaps::abs_ste %>% filter(NAME != "Other Territories")
oz_simp <- oz %>% rmapshaper::ms_simplify(keep = 0.05) 

cb <- as_cubble(
  list(spatial = stations_sf, temporal = ts),
  key = id, index = date, coords = c(long, lat)
)

set.seed(0927)
cb_glyph <- cb %>% 
  slice_sample(n = 20) %>% 
  face_temporal() %>% 
  mutate(month = lubridate::month(date)) %>% 
  group_by(month) %>% 
  summarise(tmax = mean(tmax, na.rm = TRUE)) %>% 
  unfold(long, lat)

ggplot() + 
  geom_sf(data = oz_simp, fill = "grey90", color = "white") +
  geom_glyph(
    data = cb_glyph,
    aes(x_major = long, x_minor = month, y_major = lat, y_minor = tmax),
    width = 2, height = 0.7) + 
  ggthemes::theme_map() 
