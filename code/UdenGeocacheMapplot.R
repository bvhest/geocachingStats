# plot geocaching locaties als een animatie van karten met grootcirkels van de 
# ene locatie naar de volgende.
#
# inspiratie: https://blog.snap.uaf.edu/2016/11/15/mapmate-0-2-0/
#
#
# BvH, november 2016
#

library(XML)
library(tibble)
library(dplyr)
library(stringr)
library(lubridate)

# plotting libraries
devtools::install_github("leonawicz/mapmate")
library(mapmate)
library(animation)

#library(ggplot2)
#library(scales)
# maps libraries
#library(googleVis)

#library(choroplethrAdmin1)
#library(choroplethr)
#library(ggmap)
#library(mapproj)
#library (plotGoogleMaps)

Sys.getlocale("LC_TIME")
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")

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
# Example with great-circles:
#####################################################################################
# Obtain great circle arcs

set.seed(192)
data(network)
network

endpoints <- gc_endpoints(network, "lon", "lat") # df.locations was network
endpoints

# take a weighted sample, e.g., favoring larger averaged populations and shorter distances
distFun <- function(x) 1 - x/max(x)  # simple inverse distance weighting
endpoints <- mutate(endpoints, Dist_wts = distFun(Dist))
endpoints <- sample_n(endpoints, 500, replace = TRUE, weight = (Pop_wts0 + Pop_wts1)/2 + 
                        Dist_wts)

# expand data frame from endpoints to arcs, each composed of a sequence of
# points
arcs_flat <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1", breakAtDateLine = TRUE)
arcs_globe <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1")
arcs_globe

# Obtain great circle arc path sequences
paths_flat <- gc_paths(arcs_flat, "group", size = 5)
paths_globe <- gc_paths(arcs_globe, "group", size = 5)
paths_globe

# Flat map network animation
n <- max(paths_globe$id)
png.args <- list(width = 600, height = 300, bg = "black")
clrs <- c("#1E90FF50", "#FFFFFF50", "#FFFFFF", "#1E90FF75")
ylm <- range(paths_globe$lat)  # trimming empty southern map region


gglist <- save_seq(paths_globe, id = "id", n.frames = n, ortho = FALSE, type = "network", 
                   ylim = ylm, suffix = "2D", png.args = png.args, save.plot = FALSE, return.plot = TRUE)

library(animation)
# you may need to specify a different path on your Windows machine you may
# also need to ensure convert.exe is part of your particular installation
ani.options(convert = "convert")
saveGIF(for (i in seq_along(gglist)) print(gglist[[i]]), "network2D.gif", interval = 1/20, 
        ani.width = 600, ani.height = 300)

save_seq(paths_globe, id = "id", n.frames = n, ortho = FALSE, type = "network", 
         suffix = "2D", png.args = png.args)
# Next, do whatever you want with the files, such as import them to a video
# editing program


#####################################################################################
# Plot the locations as a sequence of great-circles:
#####################################################################################
# Obtain great circle arcs
df.rows <- nrow(df.locations)
endpoints <- data.frame(df.locations[1:df.rows-1,c("lon", "lat")], 
                        df.locations[2:df.rows,c("lon", "lat")],
                        df.locations[1:df.rows-1,c("time", "name")], 
                        df.locations[2:df.rows,c("time", "name")])
colnames(endpoints) <- c("lon0", "lat0", "lon1", "lat1", "time0", "name0", "time1", "name1")
endpoints <- as_tibble(endpoints)
endpoints
glimpse(endpoints)
summary(endpoints)

# alternatief
endpoints <- gc_endpoints(df.locations[1:df.rows-1,], 
                          lon="lon", lat="lat",
                          distance = TRUE, 
                          keep = TRUE) # df.locations was network
# take a weighted sample, e.g., favoring larger averaged populations and shorter distances
distFun <- function(x) 1 - x/max(x)  # simple inverse distance weighting
endpoints <- mutate(endpoints, Dist_wts = distFun(Dist))
endpoints <- sample_n(endpoints, 500, replace = TRUE, weight = Dist_wts)

# expand data frame from endpoints to arcs, each composed of a sequence of points
arcs_flat  <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1", breakAtDateLine = TRUE, addStartEnd = TRUE)
arcs_flat
arcs_globe <- gc_arcs(endpoints, "lon0", "lat0", "lon1", "lat1", addStartEnd = TRUE)
arcs_globe

# Obtain great circle arc path sequences
paths_flat  <- gc_paths(arcs_flat, group = "group", size = 5)
paths_flat
paths_globe <- gc_paths(arcs_globe, group = "group", size = 5)
paths_globe

# Flat map network animation
n <- max(paths_flat$id)
png.args <- list(width = 600, height = 300, bg = "white")
clrs <- c("#1E90FF50", "#00FFFF50", "#000000", "#1E90FF75")
xlm <- range(paths_flat$lon)  # trimming empty southern map region
ylm <- range(paths_flat$lat)  # trimming empty southern map region

xlm <- c(3.0, 8.1)
ylm <- c(49.46, 53.7)

gglist <- save_seq(paths_flat,
#                   style = "map",
                   id = "id", 
                   n.frames = n, 
                   ortho = TRUE, 
                   col = clrs,
                   type = "points", 
                   contour = "none",
                   xlim = xlm, 
                   ylim = ylm, 
                   suffix = "2D", 
                   png.args = png.args, 
                   save.plot = FALSE, 
                   return.plot = TRUE)

# you may need to specify a different path on your Windows machine you may
# also need to ensure convert.exe is part of your particular installation
saveGIF(for (i in seq_along(gglist)) print(gglist[[i]]), 
        movie.name = "network2D_flat.gif", 
        convert = "convert",
        interval = 1/20, 
        ani.width = 1200, 
        ani.height = 600)

# save_seq(paths_globe, id = "id", n.frames = n, ortho = FALSE, type = "network", 
#          suffix = "2D", png.args = png.args)
#
# Next, do whatever you want with the files, such as import them to a video
# editing program

# Globe map network animation
n <- max(paths_globe$id)
png.args <- list(width = 600, height = 300, bg = "black")
clrs <- c("#1E90FF50", "#FFFFFF50", "#FFFFFF", "#1E90FF75")
xlm <- range(paths_globe$lon)  # trimming empty southern map region
ylm <- range(paths_globe$lat)  # trimming empty southern map region

gglist <- save_seq(paths_globe, 
                   id = "id", 
                   n.frames = n, 
                   ortho = TRUE, 
                   type = "network", 
                   xlim = xlm, 
                   ylim = ylm, 
                   suffix = "2D", 
                   png.args = png.args, 
                   save.plot = FALSE, 
                   return.plot = TRUE)

saveGIF(for (i in seq_along(gglist)) print(gglist[[i]]), 
        movie.name = "network2D_globe.gif", 
        convert = "convert",
        interval = 1/20, 
        ani.width = 600, 
        ani.height = 300)

# save_seq(paths_globe, id = "id", n.frames = n, ortho = FALSE, type = "network", 
#          suffix = "2D", png.args = png.args)


