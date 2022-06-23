library(sf)
library(dplyr)
library(rgdal) 
library(ggplot2) 
library(magick)
library(tidyverse)
library(gganimate)
library(gifski)
library(maps)
library(maptools)
library(sp)
library(lattice)
library(latticeExtra)
library(colorspace)
library(data.table)
library(ggplot2)

setwd("C:/Users/Rocco/Desktop/My Files/NYRENTS/New/Data")

#loading data
rents <- read.csv(file = "Data_Frame/rents_forrocco_071219.csv")

#Removing rows with missing date information in month and day columns.
rentsdateclean<-rents[!is.na(rents$m),]

#Removing rows with NA values in the rents column for symbology and analysis.
rentsvalueclean <- rentsdateclean[!is.na(rentsdateclean$mrent),]

#Saving as new csv
write.csv(rentsvalueclean, file = "Data_Frame/rentsclean.csv")

#I then concatenated the year, month, and day into an new column "YYYYMMDD" in
#Excel for use in GIS programs.

rentsconcat <- read.csv(file = "Data_frame/rentscleanconcat.csv")

#I know want to convert he date from YYYYMMDD, which could not be retained in the csv
#to a readable date format like YYYY-MM-DD

#rentsconcat$YYYYMMDD <- as.Date(as.character(rentsconcat$YYYYMMDD),"%Y%m%d")

#extracting sampled months from the data set (March, April, May, August, September, October)
rents_sampled <- subset(rentsconcat, m == 3 | m == 4 | m == 5 | m == 8 | m == 9 | m == 10, select=X:distcore)

#extract final cleaned and sampeld dataset
write.csv(rents_sampled, file = "Data_Frame/rentssampled.csv")

#mapping in QGIS to attach neighborhood identities to the rent observation points

rent_points <- st_read("C:/Users/Rocco/Desktop/NYRENTS/GIS/rent_obs_newcode.shp")
back <- st_read("C:/Users/Rocco/Desktop/NYRENTS/GIS/manhattan_neighborhoods.shp")
#shp <- readOGR(dsn = "GIS", layer="neighborhoods_WGS1984")
background <- fortify(back, region = "NTAName")

#This code splits the master rent point file into three parts
#making it easier to visualize them into seperate facet grids
part_one <- subset(rent_points, y < 1890)
part_two <- subset(rent_points, y < 1900 & y > 1889)
part_three <- subset(rent_points, y > 1900)

#creating facet grid maps to visualize the data
first_grid <- ggplot() +
  geom_sf(data = back, size = 0.5, color = "black", fill = "grey69")+
  geom_sf(
    data = part_one,
    color = 'black', alpha = .5, size = 1,
    show.legend = 'point', inherit.aes = F)

first_grid +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~y, ncol = 5)

second_grid <- ggplot() +
  geom_sf(data = back, size = 0.5, color = "black", fill = "grey69")+
  geom_sf(
    data = part_two,
    color = 'black', alpha = .5, size = 1,
    show.legend = 'point', inherit.aes = F)

second_grid +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~y, ncol = 5)

third_grid <- ggplot() +
  geom_sf(data = back, size = 0.5, color = "black", fill = "grey69")+
  geom_sf(
    data = part_three,
    color = 'black', alpha = .5, size = 1,
    show.legend = 'point', inherit.aes = F)

third_grid +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  facet_wrap(~y, ncol = 5)

#This code visualizes all data together in one facet grid
full_grid <- ggplot() +
  geom_sf(data = back, size = 0.5, color = "black", fill = "grey69")+
  geom_sf(
    data = rent_points,
    color = 'blue', alpha = .5, size = 2,
    show.legend = 'point', inherit.aes = F)

#This code animates the full facet grid, one map at a time
anim <- full_grid +
  transition_manual(y) +
  labs(title = "Manhattan Rent Observations in {current_frame}") +
  theme(plot.title = element_text(hjust = 0.5),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  labs(caption="Credit: Rowena Gray, Rocco Bowman")

gganimate::animate(anim)
anim_save("Images/new_point_animation.gif", anim)


#demo joins by neighborhood

origin_all <- st_read("C:/Users/Rocco/Desktop/My Files/NYRENTS/GIS/prop_merge_final.shp")

sub_originall <- origin_all[9:21]
sub_originall <-sub_originall[ , -which(names(sub_originall) %in% c("w_newyorke","w_nativeot"))]
sub_originall$geometry <- NULL
sub_originall <- as.data.table(sub_originall)

sub_originall$predom <- colnames(sub_originall)[max.col(sub_originall,ties.method="first")]
sub_originall$predom <- str_extract(sub_originall$predom, "[a-z]{5,10}")

origin_all$predom <- sub_originall$predom
#origin_all <- origin_all[!is.na(origin_all$british_su),]

st_geometry(origin_all) <- NULL
data <- origin_all[ , -which(names(origin_all) %in% c("layer","path","geometry"))]
proportional_data <- as.data.frame(data)
class(data)

write.csv(data, file = "Data_Frame/proportional_data_neighborhood.csv")

demo_map <- ggplot() +
  #geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = origin_all, color = "black", size = 0.6,
    aes(fill = predom),
    alpha = 1,
    show.legend = TRUE, inherit.aes = F) +
    scale_fill_discrete(name="Nationality",
    breaks=c("british", "irish", "german","italian","polish","russiaba", "other"),
    labels=c("British", "Irish", "German","Italian","Polish","Russian/Baltic","Other"))
  
grid_demo <- demo_map + ggtitle("Predominant Non-native Nationality by Neighborhood \n 1880-1910") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_wrap(~year, ncol = 3)

grid_demo

pop_map <- ggplot() +
  geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = origin_all,
    aes(fill = pop_sum),
    alpha = 0.7,
    show.legend = TRUE, inherit.aes = F) +
    scale_fill_gradient(low="slategray2", high="blue")

grid_pop <- pop_map + ggtitle("Total Population by Neighborhood \n 1880-1910") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_wrap(~year, ncol = 3)


newyorker <- ggplot() +
  geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = origin_all,
    aes(fill = newyorker_),
    alpha = 0.7,
    show.legend = TRUE, inherit.aes = F) +
  scale_fill_gradient(low="lightsalmon1", high="red")

grid_ny <- newyorker + ggtitle("Total Native New Yorker Population \n 1880-1910") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_wrap(~year, ncol = 3)

#This combines data calculated proportionally in QGIS 
early <- read.csv(file = "Data_Frame/1880_proportional_neighborhood.csv")
mid <- read.csv(file = "Data_Frame/1900_proportional_neighborhood.csv")
late <- read.csv(file = "Data_Frame/1910_proportional_neighborhood.csv")

total <- rbind(early_prop,mid_prop,late_prop)

#Mapping proportional population

#loading shapefiles

population <- st_read("C:/Users/Rocco/Desktop/My Files/NYRENTS/GIS/pop_merge.shp")
population$sqmile <- population$Shape_Area/27878400
population$sqkm <- population$sqmile*2.59
population$popdens <- population$w_populati/population$sqkm
population$rel_pop <- population$w_populati/max(population$w_populati)
#quantile

no_classes <- 5
labels <- c()

quantiles <- quantile(population$rel_pop, 
                      probs = seq(0, 1, length.out = no_classes + 1))

# here I define custom labels (the default ones would be ugly)
labels <- c()
for(idx in 1:length(quantiles)){
  labels <- c(labels, paste0(round(quantiles[idx], 2), 
                             " - ", 
                             round(quantiles[idx + 1], 2)))
}
# I need to remove the last label 
# because that would be something like "66.62 - NA"
labels <- labels[1:length(labels)-1]

# here I actually create a new 
# variable on the dataset with the quantiles
population$quant <- cut(population$rel_pop, 
                                     breaks = quantiles, 
                                     labels = labels, 
                                     include.lowest = T)

pop_map <- ggplot() +
  #geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = population,
    aes(fill = quant),
    alpha = 0.7,
    show.legend = TRUE, inherit.aes = F)+
  scale_fill_grey(start=1, end=0.2)


grid_pop <- pop_map + ggtitle("Total Population by Neighborhood \n 1880-1910") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_wrap(~year, ncol = 3)
grid_pop

#natural jenks
library(classInt)
library(formattable)
library(BAMMtools)
library(stringr)

part_one <- subset(population, year == 1880)
#part_one$analytic <- (part_one$w_populati/part_one$Shape_Area)/3280.84
#part_one$total <- round((part_one$w_populati)/1000)

classes <- getJenksBreaks(part_one$popdens, 6, subset = NULL)


#classes <- classIntervals(part_one$rel_pop, n = 5, style = "jenks")
part_one$percent_class = cut(part_one$popdens, classes, include.lowest = T)

#part_one$percent_class <- gsub("\\[|\\]", "", part_one$percent_class)
#part_one$percent_class <- gsub("\\(|\\)", "", part_one$percent_class)
#part_one$percent_class <- gsub(",", "-", part_one$percent_class)

pop_map_1880 <- ggplot() +
  #geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = part_one,
    aes(fill = percent_class),
    alpha = 1,
    show.legend = TRUE, inherit.aes = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  scale_fill_grey(start=1, end=0.2, name = "Population\n(Thousands)", labels = c("0-12","12-35","35-68","68-86", "86-142"))+
  ggtitle("1880")

pop_map_1880 <- ggplot() +
  #geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = part_one,
    aes(fill = popdens),
    alpha = 1,
    show.legend = TRUE, inherit.aes = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
        ggtitle("1880")

eighteighty <- pop_map_1880 + scale_fill_gradient(low = "white", high = "black", name = expression(paste("Population/km"^"2")))
eighteighty


#####1900

part_two <- subset(population, year == 1900)
part_two$total <- round((part_two$w_populati)/1000)

classes <- getJenksBreaks(part_two$popdens, 6, subset = NULL)


#classes <- classIntervals(part_one$rel_pop, n = 5, style = "jenks")
part_two$percent_class = cut(part_two$popdens, classes, include.lowest = T)

#part_two$percent_class <- gsub("\\[|\\]", "", part_two$percent_class)
#part_two$percent_class <- gsub("\\(|\\)", "", part_two$percent_class)
#part_two$percent_class <- gsub(",", "-", part_two$percent_class)

pop_map_1900 <- ggplot() +
  #geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = part_two,
    aes(fill = percent_class),
    alpha = 1,
    show.legend = TRUE, inherit.aes = F)+
    scale_fill_grey(start=1, end=0.2, name = "Population\n(Thousands)", labels = c("2-51","51-135","135-205","205-292","292-408"))+
    ggtitle("1900")+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())

pop_map_1900 <- ggplot() +
  #geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = part_two,
    aes(fill = popdens),
    alpha = 1,
    show.legend = TRUE, inherit.aes = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  ggtitle("1900")

ninehundred <- pop_map_1900 + scale_fill_gradient(low = "white", high = "black", name = expression(paste("Population/km"^"2")))
  

pop_map_1900

#####1910

part_three <- subset(population, year == 1910)
#part_three$total <- round((part_three$w_populati)/1000)

classes <- getJenksBreaks(part_three$popdens, 6, subset = NULL)


#classes <- classIntervals(part_one$rel_pop, n = 5, style = "jenks")
part_three$percent_class = cut(part_three$popdens, classes, include.lowest = T)

#part_one$percent_class <- gsub("\\[|\\]", "", part_one$percent_class)
#part_one$percent_class <- gsub("\\(|\\)", "", part_one$percent_class)
#part_one$percent_class <- gsub(",", "-", part_one$percent_class)

pop_map_1910 <- ggplot() +
  #geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = part_three,
    aes(fill = popdens),
    alpha = 1,
    show.legend = TRUE, inherit.aes = F)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  #scale_fill_grey(start=1, end=0.2, name = "Population\n(Thousands)", labels = c("3-70","70-144","144-217","217-329", "329-456"))+
  ggtitle("1910")

Nineten <- pop_map_1910 + scale_fill_gradient(low = "white", high = "black", name = expression(paste("Population/km"^"2")))

part_three <- subset(population, year == 1910)

classes <- classIntervals(part_one$w_populati, n = 5, style = "jenks")
population <- part_one %>%
  mutate(percent_class = cut(w_populati, classes$brks, include.lowest = T))

#population$breaks <- if_else(population$percent_class == "[0.00254]","361-51311","Other")

pop_map_1880 <- ggplot() +
  #geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  geom_sf(
    data = population,
    aes(fill = percent_class),
    alpha = 1,
    show.legend = TRUE, inherit.aes = F)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
    panel.background = element_blank(),
    plot.title = element_text(hjust = 0.5), 
    axis.title.x=element_blank(),
    axis.text.x=element_blank(),
    axis.ticks.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.y=element_blank(),
    axis.ticks.y=element_blank())+
    scale_fill_grey(start=1, end=0.2, name = "Population")+
    ggtitle("1880")

pop_map_1880

#pop_map <- ggplot() +
  #geom_sf(data = back, size = 0.4, color = "black", fill = "grey69")+
  #geom_sf(
   # data = population,
   # aes(fill = percent_class),
   # alpha = 1,
   # show.legend = TRUE, inherit.aes = F)+
 # panel.background = element_blank(), axis.line = element_line(colour = "black"))+
#  scale_fill_grey(start=1, end=0.2, name = "Population", labels = c("Low", "Low-Medium", "Medium", "Medium-High", "High"))

grid_pop <- pop_map + ggtitle("Relative Population by Neighborhood, 1880-1910") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank())+
  facet_wrap(~year, ncol = 3)
grid_pop


library(ggpubr)

mosaic <- ggarrange(eighteighty,ninehundred, Nineten + rremove("x.text"), 
                    ncol = 3, nrow = 1)
mosaic
