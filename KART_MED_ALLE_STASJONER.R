PRØVE KART

Raet_2023 <- read.csv("Raett_2023.csv", comment.char="#", stringsAsFactors=TRUE)

Raet_2023$Lat
library(ggplot2)
library(dplyr)
library(patchwork)
library(sf)
library(jsonlite)



#Coordinates of the lobster MPA in Tvedestrand and how to make a simple polygon for plotting
bevaringpoly<-data.frame(Long=c(9.122616667,9.129366667,9.15295,9.135733333,9.0999,9.095316667,9.098866667,9.099233333,9.11855,9.119083333,9.122616667),
                         Lat=c(58.62268333,58.61806667,58.60163333,58.59153333,58.6108,58.61325,58.6143,58.61453333,58.6196,58.62058333,58.62268333))

pol2 = st_polygon(
  list(
    cbind(
      bevaringpoly$Long[c(1:11,1)],
      bevaringpoly$Lat[c(1:11,1)])
  )
)
pol2

MPA <-st_sfc(pol2, crs=4326)
st_crs(MPA)

plot(MPA)


#Plot av kystlinje fra Tvedestrand til Grimstad

kommuner<-read_sf("kommuner2021.json")
plot(st_geometry(kommuner))
st_crs(kommuner)



Map_Raet <- subset(kommuner, kommuner$Kommunenavn =="Arendal" |kommuner$Kommunenavn == "Tvedestrand"|kommuner$Kommunenavn == "Grimstad")
plot(st_geometry(Map_Raet))


#Raet_boundaries contains all the info in the part
Raet_boundaries<- read_sf("Natur_Vern_42_agder_4326.json")
plot(st_geometry(Raet_boundaries))
str(Raet_boundaries)
table(list(Raet_boundaries$navn))

#we want to plot the  boundaries of the part
Raet_park <- subset(Raet_boundaries, Raet_boundaries$navn=="Raet")
plot(st_geometry(Raet_park))

View(Raett_2023)

#Lese inn koordinater til stajonene, og sørge for at dere leser det inn i samme coordinate reference system eller crs til dea andre filer  (4326):

Habitat <- read_delim("C:/Users/Saray/Desktop/Rstudio_Master_Graphs/Master/TMOBs_prensence_absence_2023.csv", delim = ";", escape_double = FALSE, trim_ws = TRUE)

Fish <- read_delim("C:/Users/Saray/Desktop/Rstudio_Master_Graphs/Master/Raett_2023.csv",delim = ";", escape_double = FALSE, trim_ws = TRUE)

filtered_fish <- Fish %>%
  filter(!Species %in% c("pagurus", "gammarus"))  # Exclude the two species

#Excluding cancridae and humurus
merged_data_U <- Habitat %>%
  left_join(filtered_fish %>% filter(!is.na(Cluster_ID)), by = c("Station_ID", "Cluster_ID"))
str(merged_data_U)

merged_data_U <- merged_data_U %>%
  filter(!is.na(Long) & !is.na(Lat))


library(dplyr)


merged_data_U_unique <- merged_data_U %>%
  group_by(Station_ID) %>%                    # Group by station
  summarise(Dominant_habitat = names(which.max(table(Dominant_habitat))), 
            Long = mean(Long, na.rm = TRUE),  # Keep coordinates
            Lat = mean(Lat, na.rm = TRUE))  

merged_data_U_unique <- merged_data_U_unique %>%
  filter(!is.na(Long) & !is.na(Lat))

# Convert to sf object
merged_data_U_unique <- st_as_sf(merged_data_U_unique, coords = c("Long", "Lat"), crs = 4326)


getwd()
Raett_2023 <- read.csv2("Raett_2023.csv", header = TRUE, stringsAsFactors = FALSE)

str(Raett_2023)

Raett_2023 <- st_as_sf(Raett_2023,coords= c("Long", "Lat"),crs=4326)

#Nå kan vi flette sammen kommune og park vectorer til en ggplot

View(Raet)


  ggplot() +
  geom_sf(data = Map_Raet, fill = "lightgray", color = "black") +  # Land areas
    geom_sf(data = Raet_park, fill = "blue", alpha = 0.4, color = "black") +  # MPA with transparency
    geom_sf(data = Raett_2023, cex = 1, color = "red") +  # Stations on top
    theme_classic()
  
  ggplot() +
    geom_sf(data = Map_Raet, fill = "lightgray", color = "black") +  # Land areas
    geom_sf(data = Raet_park, fill = "blue", alpha = 0.4, color = "black") +  # MPA with transparency
    geom_sf(data = Raett_2023, size = 2, color = "red") +  # Stations on top
    geom_text(aes(x = 15, y = 59, label = "Raet National Park, Norway"), color = "black", size = 5, fontface = "bold") +  # Adding text label
    theme_classic()
  
  
  
ggplot()+
  geom_sf(data=Raett_2023, cex=0.4, color = "red")+
  geom_sf(data=Map_Raet)+
  geom_sf(data=Raet_park)+
  theme_classic()

#### Cluster_ID Map ####


ggplot() +
  geom_sf(data = Map_Raet, fill = "lightgray", color = "black") +  # Land areas
  geom_sf(data = Raet_park, fill = "blue", alpha = 0.4, color = "black") +  # MPA with transparency
  geom_sf(data = Raett_2023, aes(color = as.factor(Cluster_ID)), size = 2) +  # Color by Cluster_ID
  scale_color_viridis_d() +  # Automatic color scale
  theme_classic()


### Stronger colors
ggplot() +
  geom_sf(data = Map_Raet, fill = "lightgray", color = "black") +  
  geom_sf(data = Raet_park, fill = "blue", alpha = 0.4, color = "black") +  
  geom_sf(data = Raett_2023, aes(color = as.factor(Cluster_ID)), size = 2) +  
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "yellow", "brown", "cyan")) +  
  labs(color = "Cluster ID") +  
  theme_classic()



### Prøver å se om jeg kan adde de forskjellige habitatene inn på kartet... men jeg får det ikke til

str(merged_data_U)
merged_data_U$Dominant_habitat <- as.factor(merged_data_U$Dominant_habitat)
str(merged_data_U)
merged_data_U_unique$Dominant_habitat <- as.factor(merged_data_U_unique$Dominant_habitat)


library(dplyr)
library(sf)

merged_data_U_unique <- merged_data_U_unique %>%
  group_by(Station_ID) %>%                    
  summarise(Dominant_habitat = names(which.max(table(Dominant_habitat))), 
            geometry = st_union(geometry)) %>%  # Keep spatial data
  st_as_sf()  # Ensure it's still an sf object


merged_data_U_unique$Dominant_habitat <- as.factor(merged_data_U_unique$Dominant_habitat)


library(sf)

st_crs(merged_data_U) <- 4326
library(sf)

merged_data_U <- st_as_sf(merged_data_U, coords = c("Long", "Lat"), crs = 4326)
st_crs(merged_data_U)
str(merged_data_U)
View(merged_data_U_unique)
st_geometry_type(merged_data_U_unique)
table(st_geometry_type(merged_data_U_unique))




ggplot() +
  geom_sf(data = Map_Raet, fill = "lightgray", color = "black") +  
  geom_sf(data = Raet_park, fill = "blue", alpha = 0.4, color = "black") +  
  geom_sf(data = merged_data_U_unique, aes(color = Dominant_habitat), size = 2) +  
  scale_color_manual(values = c("red", "blue", "green", "purple", "orange", "pink", "yellow", "brown", "cyan", "#d12379", "lightgreen", "black", "grey")) +  
  labs(color = "Dominant Habitat") +  
  theme_classic()

view(merged)