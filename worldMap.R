library("ggplot2")
library("ggthemes")
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


# Loading and wrangling data
library(readr)
library(dplyr)

time_series_19_covid_Confirmed <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
View(time_series_19_covid_Confirmed)
raw <- time_series_19_covid_Confirmed %>% rename (name = "Country/Region", covidCount =  "2/27/20") %>% group_by(name) %>%
      summarise_at(vars(`1/22/20`:covidCount), sum, na.rm = TRUE) %>% mutate(name = replace(name, name == "US", "United States")) %>%
      mutate(name = replace(name, name == "UK", "United Kingdom")) %>% mutate(name = replace(name, name == "Mainland China", "China"))

iran <- raw %>% filter (name == "Iran")

derivative <- function(sites) {
  for (i in 1:dim(sites)[1]) {
    for (j in length(sites):6){
      sites[i, j] <- as.numeric(sites[i, j] - sites[i, j-1])
    }  
  }
  return(sites)
}  

sites <- derivative(raw)


# merging datasets

covidWorld <- world %>% left_join(sites)

# world map of Corona
ggplot(data = covidWorld) +
  geom_sf(aes(fill=covidCount)) + scale_fill_distiller(palette = "Spectral") +
  theme_tufte()


ggplot(data = world) +
  geom_sf() +
  geom_point(data = sites, aes(x = Long, y = Lat), size = 1, 
             shape = 23, fill = "red") 
