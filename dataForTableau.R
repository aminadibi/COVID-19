library(dplyr)

#time_series_19_covid_Confirmed <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
time_series_19_covid_Confirmed <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")


raw <- time_series_19_covid_Confirmed 
raw <- time_series_19_covid_Confirmed %>% rename (country = "Country/Region") %>% group_by(country) %>%
  summarise_at(vars(`1/22/20`:`3/2/20`), sum, na.rm = TRUE)  %>%
   mutate(country = replace(country, country == "US", "United States")) %>%
  mutate(country = replace(country, country == "UK", "United Kingdom")) %>% mutate(country = replace(country, country == "Mainland China", "China"))


derivative <- function(sites) {
  for (i in 1:dim(sites)[1]) {
    for (j in length(sites):6){
      sites[i, j] <- as.numeric(sites[i, j] - sites[i, j-1])
    }  
  }
  return(sites)
}  

sites <- derivative(raw)

write.csv(sites,'covidFirstOrder28-02.csv')

sites <- derivative(sites)
write.csv(sites,'covidSecondOrder28-02.csv')
