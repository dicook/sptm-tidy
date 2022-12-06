library(cubble)
library(dplyr)
library(crosstalk)
library(leaflet)
library(colorspace)
library(ggplot2)
library(plotly)
library(htmltools)
library(webshot)

clean <- climate_aus |>
  filter(name == "melbourne airport") |>
  bind_rows(climate_subset)  |>
  face_temporal() |>
  mutate(month = lubridate::month(date, label = TRUE, abbr = TRUE)) |>
  group_by(month) |>
  summarise(
    tmax = mean(tmax, na.rm = TRUE),
    tmin = mean(tmin, na.rm = TRUE),
    diff = mean(tmax - tmin, na.rm = TRUE)
  ) |>
  face_spatial() |>
  mutate(temp_diff_var = var(ts$diff, na.rm = TRUE))

nested <- clean %>% SharedData$new(~id, group = "cubble")
long <- clean |>
  face_temporal() |>
  unfold(temp_diff_var) |>
  arrange(temp_diff_var) |>
  SharedData$new(~id, group = "cubble")

domain <- clean$temp_diff_var
pal <- colorNumeric(
  colorspace::sequential_hcl(
    "Rocket",  n = 7, cmax = 90, rev = TRUE, c2 = 40, l2= 85, c1 = 20, l1 = 30),
  domain = domain)

map <- leaflet(nested, width = 300, height = 300) |>
  addTiles() |>
  addCircleMarkers(color = ~pal(domain), group = "a", radius = 0.1,
                   popup = ~name, fillOpacity = 1, opacity = 1)

# make sure the library `plotly` has been loaded before running the following
ts_static <- long |> 
  ggplot(aes(x = month, group = id,
             fill = temp_diff_var, color = temp_diff_var
  )) +
  geom_ribbon(aes(ymin = tmin, ymax = tmax), linewidth = 0.1, alpha = 0.3) +
  geom_point(aes(y = tmax), size = 0.1) +
  geom_point(aes(y = tmin), size = 0.1) +
  colorspace::scale_fill_continuous_sequential(
    "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
    c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
  colorspace::scale_colour_continuous_sequential(
    "Rocket",  n_interp = 7, cmax = 90, rev = TRUE,
    c2 = 40, l2= 85, c1 = 20, l1 = 30, name = "Var. temp. diff.") +
  labs(x = "Month", y = "Temperature") +
  theme_bw() +
  theme(
    panel.grid.major = element_blank(),
    legend.position = "bottom"
  )
ts_interactive <- ggplotly(ts_static, width = 600, height =300) |>
  highlight(on = "plotly_selected", opacityDim = 0.012)

p <- bscols(map, ts_interactive, widths = c(4, 6))

# saving
# as interactive
htmltools::save_html(p, file = PATH_TO_SAVE)
# as static
# you may need `webshot::install_phantomjs()`
webshot::webshot(url = PATH_TO_THE_SAVED_HTML, file = PATH_TO_SAVE)

