library(dplyr)

time_series_19_covid_Confirmed <- read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
raw <- time_series_19_covid_Confirmed 

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

sites <- derivative(raw)
write.csv(sites,'covidSecondOrder28-02.csv')
