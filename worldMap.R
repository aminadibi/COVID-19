library("ggplot2")
theme_set(theme_bw())
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

ggplot(data = world) +
  geom_sf()

ggplot(data = world) +
  geom_sf(fill = "lightgreen") +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))


# Loading data
library(readr)
time_series_19_covid_Confirmed <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
View(time_series_19_covid_Confirmed)

library(dplyr)
sites <- time_series_19_covid_Confirmed %>% select ("Country/Region", "Lat", "Long", "2/27/20") %>% 
          rename (name = "Country/Region", covidCount =  "2/27/20")


ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = Long, y = Lat), size = 1, 
             shape = 23, fill = "red") 


# merging datasets

covidWorld <- world %>% left_join(sites)

# world map of Corona
ggplot(data = covidWorld) +
  geom_sf(aes(fill=covidCount)) + scale_fill_viridis_c(option = "plasma", trans = "sqrt")

