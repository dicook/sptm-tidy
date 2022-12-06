library(cubble)
library(dplyr)
library(leaflet)
library(ggplot2)
library(leafpop)

clean <-  climate_subset |>
  face_temporal() |>
  mutate(month = lubridate::month(date)) |>
  group_by(month) |>
  summarise(tmax = mean(tmax, na.rm = TRUE),
            tmin = mean(tmin, na.rm = TRUE),
            diff = mean(tmax - tmin, na.rm = TRUE)) |>
  ungroup(month) |>
  face_spatial() |>
  mutate(temp_diff_var = ts |> pull(diff) |> var(na.rm = TRUE)) |>
  face_temporal() |>
  mutate(dummy_date = as.Date(glue::glue("2021-{month}-01"))) |>
  unfold(name)

df_id <- clean$id |> unique()
p <- purrr::map(1:length(df_id), function(i){
  dt <- clean |> filter(id == df_id[i])
  dt |>
    ggplot(aes(x = dummy_date, ymin = tmin, ymax = tmax, group = id)) +
    geom_ribbon(color = "grey50", fill = "grey50", alpha = 0.5) +
    theme_bw() +
    scale_x_date(date_labels = "%b", date_breaks = "1 month") +
    labs(x = "Month", y = "Temperature") +
    ylim(-10, 50) +
    ggtitle(paste0(stringr::str_to_title(unique(dt$name))))
})

nested <- face_spatial(clean)
domain <- nested$temp_diff_var
pal <- colorNumeric(
  colorspace::sequential_hcl(
    "Rocket",  n = 7, cmax = 90, rev = TRUE,
    c2 = 40, l2= 85, c1 = 20, l1 = 30),
  domain = domain)

leaflet(nested) |>
  addTiles() |>
  addCircleMarkers(color = ~pal(domain), radius = 0.1,
                   fillOpacity = 1, opacity = 1,
                   popup = popupGraph(p, width = 300, height =200))
