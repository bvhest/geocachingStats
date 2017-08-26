# 
# Plotje van de route langs mijn geocache-locaties o.b.v. de kortste afstanden
# tussen deze locaties, m.a.w. o.b.v. zogenaamde grootcirkels.
# 
# gebruikte documentatie/voorbeelden:
#   - http://www.r-graph-gallery.com/2017/06/14/how-to-draw-connecting-routes-on-map-with-r-and-great-circles/
#   - European Map with ggplot2 using R (http://egallic.fr/european-map-using-r/)
# 
# BvH, 16-06-2017
# 

library(XML)
library(lubridate)
library(tidyverse)
library(maps)
library(geosphere)

Buenos_aires=c(-58,-34)
Paris=c(2,49)
Melbourne=c(145,-38)
data=rbind(Buenos_aires, Paris, Melbourne) %>% as.data.frame()
colnames(data)=c("long","lat")

#####################################################################################
# load the data:
#####################################################################################
pfile <- htmlTreeParse("./data/geocachesUden_20161119085502.gpx",
                       error = function (...) {}, 
                       useInternalNodes = TRUE)

#####################################################################################
# Parse the GPX file and create a dataframe with locations:
#####################################################################################
# Get all elevations, times and coordinates via the respective xpath
times <- ymd_hms(xpathSApply(pfile, path = "//wpt/time", xmlValue)) # note: no real time-stamp.
names <- xpathSApply(pfile, path = "//wpt/urlname", xmlValue)
lats <- as.numeric(xpathSApply(pfile, path = "//wpt/@lat"))
lons <- as.numeric(xpathSApply(pfile, path = "//wpt/@lon"))
urls <- xpathSApply(pfile, path = "//wpt/url", xmlValue)

# Put everything in a dataframe and get rid of old variables
df.locations <- data.frame(time = times, name = names, lat = lats, lon = lons, url = urls)

head(df.locations)

rm(list=c("names", "lats", "lons", "pfile", "times", "urls"))


#####################################################################################
# plot (obv base-plot):
#####################################################################################
par(mar = c(-20, 59, 35, 71))
map("world", 
    mercator = "mercator",
    col = "#f2f2f2", fill = TRUE, bg = "white", lwd = 0.05, 
    mar = rep(0,4), 
    border = 0, 
    ylim = c(-80,80) )

# plot locaties
points(x = df.locations$lon, y = df.locations$lat, col = "slateblue", cex = 1, pch = 20)


#####################################################################################
# plot (obv ggplot):
#
# zie https://developers.google.com/maps/documentation/static-maps/styling#features
#####################################################################################
library(ggplot2)
library(ggmap)
library(mapproj)

map <- get_map(location = 'Europe', 
               maptype = "toner-lite",
               zoom = 4)

# kaart zonder labels
map <- get_googlemap(location = 'Europe', 
                     maptype = "terrain",
                     style = 'feature:road|element:all|visibility:simplified&style=feature:administrative.locality|element:labels|visibility:off',
                     zoom = 4)

map <- get_googlemap(center = c(7, 51.11484), 
                     zoom = 6, 
                     maptype = "terrain", 
#                     style = 'feature:road|element:all|visibility:off&style=feature:administrative.locality|element:labels|visibility:simplified' 
                     style = 'administrative.country|element:labels|visibility:off' 
)

 


