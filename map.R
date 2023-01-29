# People living with HIV -- 1990-2018-- animation
# Data https://databank.worldbank.org/source/health-nutrition-and-population-statistics#
#

library(rworldmap)
library(dplyr)
library(animation)
library("readxl")
library(RColorBrewer)
library(classInt)

my_data <- read_excel(file.choose())
nc <- dim(my_data)[2]

new_names <-
  my_data %>% 
  names(.) %>% 
  .[5:nc] %>% 
  lapply(., substr, start = 1, stop = 4) %>% 
  unlist(.)

l <- length(new_names)
names(my_data)[5:nc] <- new_names

missing_values <- function(what, substitution, data){
  data[data == what] <- substitution
  return(data)
}

part <- my_data[, 1:4]

my_data_preprocess <-
  my_data %>% 
  missing_values("..", NA, .) %>%
  .[, 5:33] %>% 
  lapply(., function(x) as.numeric(as.character(x))) %>% 
  cbind(part, .)

par(mai = c(0, 0, 0.2, 0),
    xaxs = "i",
    yaxs = "i")

colourPalette <- RColorBrewer::brewer.pal(5, "YlGn")

dataformap <-
  my_data %>% 
  missing_values("..", NA, .) %>% 
  .[, 5:33] %>% 
  lapply(., function(x) as.numeric(as.character(x))) %>% 
  cbind(part, .) %>% 
  joinCountryData2Map(., joinCode = "NAME", nameJoinColumn = "Country Name")

#getting class intervals using a "jenks" classification in classInt package
#classInt <- classInt::classIntervals( dataformap[["1995"]], n=5, style="jenks")
#catMethod = classInt[["brks"]]

catMethod <- c(100, 50000, 100000, 500000, 1000000 , 10000000)
ani.record(reset = TRUE) # clear history before recording

for (i in 1:l) {
  date <- new_names[i]
  mapParams <-
    mapCountryData(
      dataformap,
      nameColumnToPlot = date,
      mapTitle = paste("People living with HIV - ", date),
      addLegend = FALSE ,
      catMethod = catMethod ,
      colourPalette = colourPalette
    )
  do.call(
    addMapLegend ,
    c(
      mapParams ,
      legendLabels = "limits" ,
      legendWidth = 0.7  ,
      legendIntervals = "data" ,
      legendMar = 2,
      horizontal = TRUE,
      labelFontSize = 1
    )
  )
  ani.record() # record the current frame  
}

oopts = ani.options(interval = 0.5)
ani.replay()
