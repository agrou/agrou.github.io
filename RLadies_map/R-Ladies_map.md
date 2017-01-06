---
title: "Interactive world map for R-Ladies"
date: 28/12/2016
output:
  html_notebook: default
  html_document: default
---


This is the first assignment, part of the course 'Developing Data Products' from Coursera's 'Data Science' Specialization. 

### Developing an interactive map with all the R-Ladies cites 

Objective: Develop an interactive map to identify the cities with an R-Ladies community. 



```r
library(leaflet) # Load required libraries
library(dplyr)
```

Prepare the dataset

```r
RLadies_icon <- makeIcon( #save the icon to be used in the map
  iconUrl = "https://github.com/agrou/rladiesLisboa/blob/master/assets/img/R-LadiesGlobal_CMYK_offline_LogoOnly.png?raw=true",
  iconWidth = 30,
  iconAnchorX = 31*215/230/2, iconAnchorY = 16
)

RLadiesLatLong <- data.frame( #build the data frame from rladies.org website
  cities = c("S Francisco", "London", "Research Triangle Park, North Carolina", "Instanbul", "Paris", "Boston", "LA California", "Melbourne, Australia", "Madrid", "Nashville", "New York", "Barcelona", "Columbus", "Izmir", "Berlin", "Taipei", "Warsaw", "Lisbon", "Valencia", "Hartford", "Ames", "Dublin", "Tbilisi", "Washington"),
  lat = c(37.783333, 51.507222, 35.8991678, 41.0082376, 48.856614, 42.3600825, 34.0522342, -37.814107, 40.4167754, 36.1626638, 40.7127837, 41.385064, 39.9611755, 38.423734, 52.5200066, 25.0329636, 52.2296756, 38.7222524, 39.4699075, 41.7637111, 42.034722, 53.3498053, 41.716667, 38.9071923),
  lng = c(-122.416667, -0.1275, -78.86364019999996, 28.9783589, 2.3522219, -71.0588801, -118.2436849, 144.96328, -3.7037902, -86.7816016, -74.0059413, 2.173403, -82.9987942, 27.142826000000014, 13.404954, 121.56542680000007, 21.0122287, -9.1393366, -0.3762881000000107, -72.6850932, -93.62, -6.2603097, 44.783333, -77.0368707))
```


Generate the map 

```r
RLadiesLatLong %>% 
  select(lat, lng) %>% 
  leaflet() %>%
  addTiles() %>%
  addMarkers(icon = RLadies_icon) %>%
  #setView(-9.1393366, 38.72225, zoom = 4) #%>%
  clearBounds() #uncomment for an imediate view of the world map
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png)



