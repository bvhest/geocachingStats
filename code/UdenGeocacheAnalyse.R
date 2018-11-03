# geochacing statistieken
#
# BvH, oktober 2016

#setwd("/media/hestbv/Windows/Projecten/R/geocachingStats/code")

library(RCurl)
library(XML)
library(stringr)
require(dplyr)
# plotting libraries
library(ggplot2)
library(scales)
library(gtable)
library(grid)

# maps libraries
library(googleVis)

#library(choroplethrAdmin1)
#library(choroplethr)
#library(ggmap)
#library(mapproj)
#library (plotGoogleMaps)

Sys.getlocale("LC_TIME")
Sys.setlocale(category = "LC_TIME", locale = "en_US.UTF-8")

dit_jaar <- 2018

#####################################################################################
# load the data:
#####################################################################################
# build the URL
#url <- "file:///media/hestbv/Windows/Projecten/R/geocachingStats/data/UdenGeocaching.html"
url <- "./data/UdenGeocaching.html"
# read the tables and select the one that has the most rows
tables <- readHTMLTable(url)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
# select the table we need (the "ledenlijst") - read as a dataframe
my.table1 <- tables[[which.max(n.rows)]]

url <- "./data/UdenGeocaching_attended.html"
# read the tables and select the one that has the most rows
tables <- readHTMLTable(url)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
# select the table we need (the "ledenlijst") - read as a dataframe
my.table2 <- tables[[which.max(n.rows)]]

url <- "./data/UdenGeocaching_webcam.html"
# read the tables and select the one that has the most rows
tables <- readHTMLTable(url)
n.rows <- unlist(lapply(tables, function(t) dim(t)[1]))
# select the table we need (the "ledenlijst") - read as a dataframe
my.table3 <- tables[[which.max(n.rows)]]

# combineer resultaten
my.table <- rbind(my.table1, my.table2, my.table3)

remove(my.table1, my.table2, my.table3)
#####################################################################################
# data cleaning:
#####################################################################################
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
trim2 <- function (x) gsub("\\s", "", x)

# delete first (photo) column and keep data rows
my.table <- my.table[, c(3:5) ]
colnames(my.table) <- c("datum", "naam", "regio")

# first some conversion:
my.table$datum <- as.Date(my.table$datum, "%d/%b/%Y")
my.table$naam <- as.character(my.table$naam)
my.table$regio <- as.character(my.table$regio)
# remove dirt from the data
my.table$regio <- gsub("\n","",my.table$regio)

# separate region and country (if both are present)
my.table$land <- NA
maxRows <- length(my.table$naam)
for (i in 1:maxRows) {
   countryRegions <- unlist(strsplit(my.table$regio[i], ","))
   print(i)
   print(countryRegions)
   if (is.na(countryRegions[2])) {
      print("only country available")
      my.table$regio[i] <- NA
      my.table$land[i] <- trim(countryRegions[1])
   } else {
      print("country and region available")
      my.table$regio[i] <- trim(countryRegions[1])
      my.table$land[i] <- trim(countryRegions[2])
   }
}

my.table$jaar <- as.character(my.table$datum, "%Y")
my.table$maand <- as.character(my.table$datum, "%b")

# sorteer op oplopende datum:                                                            
my.table <- my.table[order(my.table$datum),] 

# my.table <- my.table[complete.cases(my.table),]

#####################################################################################
# bereken cumulatieven per jaar
#####################################################################################
df <- my.table
df$count <- 1
df$jaar_som <- 0
df <- within(df, tot_som <- cumsum(count))
for (jaar in min(df$jaar):max(df$jaar)) {
   df[df$jaar==jaar,] <- within(df[df$jaar==jaar,], jaar_som <- cumsum(count))
}

class(df$datum)
class(df$jaar_som)
class(df$tot_som)

#####################################################################################
# bereken cumulatieven per maand
#####################################################################################
df$mnd <- as.numeric(as.character(my.table$datum, "%m"))
#df$jaar <- as.numeric(df$jaar)

#####################################################################################
# bereken cumulatieven per dag
#####################################################################################
df$dvw <- as.character(my.table$datum, "%a")

#####################################################################################
# toon data
#####################################################################################
# hoeveel caches per dag?
df %>% 
  group_by(dvw) %>%
  summarise(total = n()) %>%
  arrange(total)

# hoeveel caches per maand?
df %>% 
  group_by(mnd) %>%
  summarise(total = n()) %>%
  arrange(mnd)

# hoeveel caches per jaar?
df %>% 
  group_by(jaar) %>%
  summarise(total = n())%>%
  arrange(jaar)

# hoeveel caches per land?
df %>% 
  group_by(land) %>%
  summarise(total = n()) %>%
  arrange(total)

# totaal aantal caches?
df %>% 
  summarise(total = n())
# ... er ontbreken er een paar (3 op 23/12/2016)

#####################################################################################
# plot cumulatieven per jaar
# check: http://zevross.com/blog/2014/08/04/beautiful-plotting-in-r-a-ggplot2-cheatsheet-3/
# note: geam_ribbon works, but geom_area does not...
#####################################################################################
ggplot(df, aes(x = datum), show.legend = FALSE) +
  geom_ribbon(aes(ymin=0, ymax=tot_som), fill="#92C94D", color="#35520F") +
  geom_point(aes(x = datum, y = 4*jaar_som, colour = "#F8766D"), show.legend = FALSE) +
  theme_bw() +
  labs(title="Cumulatief aantal gevonden caches per jaar en totaal", x="jaar", y="aantal") +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", labels=date_format("%Y")) +
  scale_y_continuous(breaks = round(seq(0, 1000, by = 100),1), 
                     expand = c(0, 0), 
                     limits = c(0, 1000))

pathname <- "./images"
printfile <- "geocachesJaartotalen.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")


# één ggplot met twee y-assen (zie http://drawar.github.io/posts/dual-y-axis-ggplot2/)
# p1 <- ggplot(df, aes(x = datum), show.legend = FALSE) +
#       geom_ribbon(aes(ymin=0, ymax=tot_som), fill="#92C94D", color="#35520F") +
# #      geom_point(aes(x = datum, y = jaar_som, colour = "#F8766D"), show.legend = FALSE) +
#       theme_bw() +
#       labs(title="Cumulatief aantal gevonden caches per jaar en totaal", x="jaar", y="aantal") +
#       scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", labels=date_format("%Y")) +
#       scale_y_continuous(breaks = round(seq(0, 800, by = 100),1), 
#                          expand = c(0, 0), 
#                          limits = c(0,800))
# p1
# 
# p2 <- ggplot(df, aes(x = datum), show.legend = FALSE) +
#       geom_point(aes(x = datum, y = jaar_som, colour = "#F8766D"), show.legend = FALSE) +
#       theme_bw() +
#       labs(title="Cumulatief aantal gevonden caches per jaar en totaal", x="jaar", y="aantal") +
#       scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", labels=date_format("%Y")) +
#       scale_y_continuous(breaks = round(seq(0, 200, by = 10),1), 
#                          expand = c(0, 0), 
#                          limits = c(0, 200))
# p2
# ... en hierna volgt nog veel meer code ...

# optie twee (zie https://rpubs.com/kohske/dual_axis_in_ggplot2)
grid.newpage()

p1 <- ggplot(df, aes(x = datum), show.legend = FALSE) +
  geom_ribbon(aes(ymin=0, ymax=tot_som), fill="#92C94D", color="#35520F") +
  scale_x_date(date_breaks = "1 year", date_minor_breaks = "1 month", labels = date_format("%Y")) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 900)) +
  theme_bw() +
  labs(title="Cumulatief aantal gevonden caches per jaar en totaal", x = "jaar", y = "totaal aantal")
  
p2 <- ggplot(df, aes(x = datum), show.legend = FALSE) +
  geom_point(aes(x = datum, y = jaar_som, colour = "#F8766D"), show.legend = FALSE) +
  scale_y_continuous(expand = c(0, 0), 
                     limits = c(0, 215)) + 
  theme_bw() %+replace% 
  theme(panel.background = element_rect(fill = NA))

# extract gtable
g1 <- ggplot_gtable(ggplot_build(p1))
g2 <- ggplot_gtable(ggplot_build(p2))

# overlap the panel of 2nd plot on that of 1st plot
pp <- c(subset(g1$layout, name == "panel", se = t:r))
g <- gtable_add_grob(g1, g2$grobs[[which(g2$layout$name == "panel")]], pp$t, 
                     pp$l, pp$b, pp$l)

# axis tweaks
ia <- which(g2$layout$name == "axis-l")
ga <- g2$grobs[[ia]]
ax <- ga$children[[2]]
ax$widths <- rev(ax$widths)
ax$grobs <- rev(ax$grobs)
ax$grobs[[1]]$x <- ax$grobs[[1]]$x - unit(1, "npc") + unit(0.15, "cm")
g <- gtable_add_cols(g, g2$widths[g2$layout[ia, ]$l], length(g$widths) - 1)
g <- gtable_add_grob(g, ax, pp$t, length(g$widths) - 1, pp$b)

# draw it
grid.draw(g)

pathname <- "./images"
printfile <- "geocachesJaartotalen.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")

ppi <- 300
png(paste0(pathname,'/',printfile), width=10.5*ppi, height=6.5*ppi, res=ppi)
print(grid.draw(g))
dev.off()

#####################################################################################
# plot staafdiagram van totalen per jaar
#####################################################################################
df$land <- as.factor(trim(df$land))
df$regio <- as.factor(trim(df$regio))

glimpse(df)

jaarTotalen <- df %>%
   group_by(jaar) %>%
   summarize(jaar_som = sum(count))

jaarTotalenPerLand <- df %>%
  group_by(jaar,land) %>%
  summarize(totaal = sum(count))

jaarTotalenPerLand$totaal <- as.integer(jaarTotalenPerLand$totaal)
glimpse(jaarTotalenPerLand)

# plot de resultaten: histogram met aantal gevonden caches per maand:
p <- ggplot(jaarTotalenPerLand, aes(x = jaar,y = totaal, fill = land)) +
# + stat_summary(fun.y=sum,geom="bar")
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(title = "Aantal gevonden caches per jaar", x = "jaar", y = "aantal") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

print(p)
printfile <- "geocachesTotalenPerJaarEnLand.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")

p <- p + coord_polar(start = pi/2)
print(p)
printfile <- "geocachesTotalenPerJaarEnLand_polar.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")

#####################################################################################
# bereken totalen per land
#
# example: http://ggplot2.org/book/qplot.pdf
#####################################################################################
# plot de resultaten: histogram met aantal gevonden caches per maand:
p <- ggplot(jaarTotalenPerLand, aes(x = land, y = totaal, fill = jaar)) +
  geom_bar(stat = "identity") +
  theme_bw() +
  labs(title = "Aantal gevonden caches per land", x = "land", y = "aantal") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0))

print(p)
printfile <- "geocachesTotalenPerLandEnJaar.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")

p <- p + coord_polar()
print(p)
printfile <- "geocachesTotalenPerLandEnJaar_polar.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")

#####################################################################################
# plot totalen per maand
#####################################################################################
maandTotalen <- df %>%
  group_by(mnd) %>%
  summarize(maand_som = sum(count))

# merge maand terug in resultaat:
maandTotalen <- unique(merge(maandTotalen, df[,c("mnd","maand")], by="mnd"))
# en fix de sortering van de maanden:
maandTotalen$maand <- factor(maandTotalen$maand, levels=unique(maandTotalen$maand))

# plot de resultaten: histogram met aantal gevonden caches per maand:
p <- ggplot(maandTotalen, aes(x=maand,y=maand_som)) + 
  stat_summary(fun.y=sum,geom="bar", fill="#00A6FF") +
#  scale_y_discrete(breaks=seq(0, 110, by=10), labels=seq(0, 110, by=10)) +
  theme_bw() +
  labs(title="Aantal gevonden caches per maand", x="maand", y="aantal") 

print(p)
printfile <- "geocachesMaandtotalen.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")

p <- p + coord_polar()
print(p)
printfile <- "geocachesMaandtotalen_polar.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")

#####################################################################################
# bereken totalen per dag-van-de-week
#####################################################################################
dagTotalen <- df %>%
   group_by(dvw) %>%
   summarize(dvw_som = sum(count))

# fix de sortering van de dagen:
dagTotalen$dagNum <- c(5,1,6,7,4,2,3)
dagTotalen <- dagTotalen[order(dagTotalen$dagNum),] 

dagTotalen$dvw <- factor(dagTotalen$dvw, levels=unique(dagTotalen$dvw))

# plot de resultaten: histogram met aantal gevonden caches per maand:
p <- ggplot(dagTotalen, aes(x=dvw,y=dvw_som)) + 
  stat_summary(fun.y=sum,geom="bar", fill="#00A6FF") +
#  scale_y_discrete(breaks=seq(0, 200, by=20), labels=seq(0, 200, by=20)) +
  theme_bw() +
  labs(title="Aantal gevonden caches per weekdag", x="weekdag", y="aantal")

print(p)
printfile <- "geocachesWeekdagtotalen.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")

p <- p + coord_polar()
print(p)
printfile <- "geocachesWeekdagtotalen_polar.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")


#####################################################################################
# toon de aantallen per jaar en maand in een heatmap
#####################################################################################

df.hm <- df %>% 
  group_by(jaar, mnd) %>%
  summarise(total = n()) %>%
  arrange(jaar,mnd)

glimpse(df.hm)

df.hm$jaar <- as.numeric(df.hm$jaar)
df.hm$mnd <- as.numeric(df.hm$mnd)
df.hm$total <- as.numeric(df.hm$total)

# tonen
ggplot(data = df.hm, aes(x = mnd, y = jaar)) +
  geom_tile(aes(fill = total)) +
  scale_fill_gradient(low = "light green", high = "dark green") +
  scale_x_continuous(breaks = seq(1, 12, by = 1), labels = seq(1, 12, by = 1)) +
  scale_y_continuous(breaks = seq(2007, dit_jaar, by = 1), labels = seq(2007, dit_jaar, by = 1)) +
  theme_bw() + 
  labs(title="Aantal gevonden caches per jaar en maand", x="maand", y="jaar")

printfile <- "geocachesJaarEnMaand.png"
ggsave(filename = printfile, device = "png", path = pathname, scale = 4, width = 68, height = 43, units = "mm")


#############################################################################################
# toon de aantallen per dag in een calender heatmap
##############################################################################################
source("../tools/calendarHeat.R")

library(RColorBrewer)
myColors <- brewer.pal(9,"OrRd")

dagTotalen <- df %>%
  group_by(datum) %>%
  summarize(totaal = sum(count))

dagTotalen <- transform(dagTotalen,
                        week = as.POSIXlt(datum)$yday %/% 7 + 1,
                        wdag = as.POSIXlt(datum)$wday,
                        jaar = as.POSIXlt(datum)$year + 1900,
                        dvw  = as.character(datum, "%a"))

# max. 3 jaren per 'pagina', dus de data opsplitsen:
jaar <- min(year = as.POSIXlt(dagTotalen$datum)$year + 1900)
for (i in 0:4) {
  jaarMin <- jaar + (i*3)
  jaarMax <- jaarMin + 3
  view <- dagTotalen[dagTotalen$jaar >= jaarMin & dagTotalen$jaar < jaarMax,]
  if (i == 0) {
    titel <- "Gevonden caches per dag" 
  } else {
    titel <- ""
  }
  calendarHeat(view$datum,
               view$totaal,
               colors = myColors,
               ncolors = 9,
               title = titel)
  
  dev.copy(png, paste("./images/kalenderHeatmap_",jaarMin,".png",sep=""), width=1000, height=496)
  dev.off()
}
# probleem: nu zijn de maximum waarden, en daarmee de kleurenschaal, per groep 
# van jaren verschillend.

# nu met ggplot:
ggplot(dagTotalen, aes(week, wdag, fill = totaal)) + 
  geom_tile(colour = "white") + 
  scale_fill_gradientn(colours = myColors) + 
  facet_wrap(~ jaar, ncol = 1) +
  theme_bw() + 
  labs(title = "Aantal gevonden caches per kalendardag", x="week", y="weekdag")

#####################################################################################
# toon de totalen per land in een landkaart
#
# example: http://cran.r-project.org/web/packages/googleVis/vignettes/googleVis_examples.html
#####################################################################################
landTotalen <- df %>%
  group_by(land) %>%
  summarize(totaal = sum(count))

cacheCountries <- gvisGeoChart(data=landTotalen, 
                               locationvar="land", colorvar="totaal",
                               options=list(region=150, # 150 - Europe
                                            displayMode="regions", 
                                            resolution="countries",
                                            width=1200, height=800))
plot(cacheCountries)
