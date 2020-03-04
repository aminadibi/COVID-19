library("ggplot2")
library("ggthemes")
library("sf")

library("rnaturalearth")
library("rnaturalearthdata")

world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)

# ggplot(data = world) +
#   geom_sf()
# 
# ggplot(data = world) +
#   geom_sf(fill = "lightgreen") +
#   xlab("Longitude") + ylab("Latitude") +
#   ggtitle("World map", subtitle = paste0("(", length(unique(world$NAME)), " countries)"))


# Loading and wrangling data
library(readr)
library(dplyr)

#time_series_19_covid_Confirmed <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
time_series_19_covid_Confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
raw <- time_series_19_covid_Confirmed %>% rename (name = "Country/Region") %>% group_by(name) %>%
      summarise_at(vars(5:(length(time_series_19_covid_Confirmed)-1)), sum, na.rm = TRUE) %>% mutate(name = replace(name, name == "US", "United States")) %>%
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

sites <- derivative(derivative(raw))
# write.csv(sites,'covidFirstOrder28-02.csv')
sites[, "lastDayAcceleration"] <- as.vector(sites[length(sites)])
# merging datasets

covidWorld <- world %>% left_join(sites)

# world map of Corona
world_points<- st_centroid(world)
world_points <- cbind(world, st_coordinates(st_centroid(world$geometry)))

# fillVar <- colnames(covidWorld)[104]
# stringr::str_replace_all(fillVar, "\"", "`")
# 
# stringr::str_replace_all(fillVar, "`" = "\"")

ggplot(data = covidWorld) +
  geom_sf(aes(fill=lastDayAcceleration)) + scale_fill_distiller(type = "div", palette = "RdBu", aesthetics = "fill") + theme_tufte()
#  coord_sf(xlim = c(-20, 90), ylim = c(10, 80), expand = FALSE) +
#  geom_text(data= world_points,aes(x=X, y=Y, label=name),
#            color = "darkblue", fontface = "bold", check_overlap = FALSE) +

target <- c("Australia",   
            "Austria",
            "Canada",
            "China",
            "France",
            "Germany",
            "Hong Kong",
            "Iran",
            "Italy",
            "Japan",
            "Kuwait",
            "Malaysia",
            "Netherlands",
            "Singapore",
            "South Korea",
            "Spain",
            "United Kingdom",
            "United States")

barChartData <- covidWorld %>% filter (name %in% target & !is.na(lastDayAcceleration))
ggplot(data = barChartData) +
  geom_col(aes(y = lastDayAcceleration, x = reorder(name, lastDayAcceleration), fill=lastDayAcceleration)) +
  scale_fill_distiller(type = "div", palette = "RdBu", aesthetics = "fill")+
  coord_flip()+ theme_tufte()

# ggplot(data = world) +
#   geom_sf() +
#   geom_point(data = sites, aes(x = Long, y = Lat), size = 1, 
#              shape = 23, fill = "red") 


## Leaflet 

# 
# # Library
# library(leaflet)
# 
# 
# #reading border files
# # Read this shape file with the rgdal library. 
# library(rgdal)
# world_spdf <- readOGR( 
#   dsn= "/world_shape_file/TM_WORLD_BORDERS_SIMPL-0.3.shp" , 
#   layer="TM_WORLD_BORDERS_SIMPL-0.3",
#   verbose=FALSE
# )
# 
# # Clean the data object
# world_spdf@data$POP2005[ which(world_spdf@data$POP2005 == 0)] = NA
# world_spdf@data$POP2005 <- as.numeric(as.character(world_spdf@data$POP2005)) / 1000000 %>% round(2)
# 
# 
# # Create a color palette for the map:
# mypalette <- colorNumeric( palette="viridis", domain=world_spdf@data$POP2005, na.color="transparent")
# mypalette(c(45,43))
# 
# # Basic choropleth with leaflet?
# m <- leaflet(world_spdf) %>% 
#   addTiles()  %>% 
#   setView( lat=10, lng=0 , zoom=2) %>%
#   addPolygons( fillColor = ~mypalette(POP2005), stroke=FALSE )
# 
# m
# 
# # save the widget in a html file if needed.
# # library(htmlwidgets)
# # saveWidget(m, file=paste0( getwd(), "/HtmlWidget/choroplethLeaflet1.html"))
