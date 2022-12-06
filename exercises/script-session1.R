library(ozmaps)
library(rmapshaper)
library(rnaturalearth)
library(sf)
library(dplyr)
library(ggplot2)
library(tsibble)
library(cubble)

# Exercise 1
vic <- ozmaps::abs_ste %>% filter(NAME == "Victoria") 

vic %>% ggplot() + geom_sf()
vic %>% 
  # by default, keep = 0.05
  rmapshaper::ms_simplify() %>% 
  ggplot() + 
  geom_sf()

vic %>% 
  rmapshaper::ms_simplify(keep = 0.1) %>% 
  ggplot() + 
  geom_sf()

vic %>% 
  rmapshaper::ms_simplify(keep = 0.8) %>% 
  ggplot() + 
  geom_sf()

# Exercise 2
world <- rnaturalearth::ne_countries(returnclass = "sf")
world %>% 
  ggplot() + 
  geom_sf()

world_moll <- world %>% st_transform(crs = "ESRI:54009")
world_moll %>% 
  ggplot() + 
  geom_sf()

# Exercise 3: 
# `climate` contains climate data (prcp, tmax, and tmax) for 5 stations in 2020
# the data itself is clean and here we manually add some perturbation to create 
# some duplication to mimic the data you may get from the wild.
set.seed(123)
raw <- cubble::climate %>% 
  sample_frac(size = 0.1) %>% 
  rowwise() %>% 
  mutate(prcp = prcp + sample(1:100, size = 1),
         tmax = tmax + runif(n = 1, min = 0, max = 5),
         tmin = tmin + runif(n = 1, min = 0, max = 5)) %>% 
  bind_rows( cubble::climate) %>% 
  arrange(id, date) %>% 
  ungroup()

# direct casting to tsibble doesn't work and give the error message: 
# Error in `validate_tsibble()`:
#   ! A valid tsibble must have distinct rows identified by key and index.
# ℹ Please use `duplicates()` to check the duplicated rows.
raw %>% 
  as_tsibble(key = id, index =date)

# use duplicates() to detect the duplication
dups <- raw %>% 
  duplicates(key = id, index = date)

# resolve the duplication
# from the code above can see that some random numbers are added into the 
# original value to create the "fake" values 
# hence for each id x date combination, we take the smaller one:
no_dup <- raw %>% 
  group_by(id, date) %>% 
  filter(prcp == min(prcp))

# cast again
no_dup %>% 
  as_tsibble(key = id, index =date)
