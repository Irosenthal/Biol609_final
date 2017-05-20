library(dplyr)
library(readr)
library(tidyr)
library(sp)

full_data <- read_csv("./full_data.csv")




#General code, but not relevant here due to multiple utm zones
#lonlatdf <- data.frame(full_data$longitude,full_data$latitude)
#lonlatsp <- SpatialPoints(lonlatdf, proj4string=CRS("+proj=longlat +datum=WGS84"))
#scenesp_utm <- spTransform(scenesp, ("+proj=utm +zone=11  +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
#sceneUTM <- as.data.frame(scenesp_utm)

#San Francisco is in UTM zone 10S
#pull out everything north of santa maria and call it zone 10 for reprojection
utm_zone_10S <- filter(full_data, latitude > 34.963623)
lonlatdf_zone10 <- data.frame(utm_zone_10S$longitude,utm_zone_10S$latitude)
lonlat_10_sp <- SpatialPoints(lonlatdf_zone10, proj4string=CRS("+proj=longlat +datum=WGS84"))
utm_zone_10S_utm <- spTransform(lonlat_10_sp, ("+proj=utm +zone=10  +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
utm_zone_10S_utm <- as.data.frame(utm_zone_10S_utm)
zone_10 <- cbind(utm_zone_10S, utm_zone_10S_utm)
#cleanup
rm(lonlat_10_sp)
rm(lonlatdf_zone10)
rm(utm_zone_10S_utm)
write_csv(zone_10, "zone_10.csv")
rm(utm_zone_10S)

#everything else is zone 11
utm_zone_11S <- filter(full_data, latitude < 34.963623)
lonlatdf_zone11 <- data.frame(utm_zone_11S$longitude,utm_zone_11S$latitude)
lonlat_11_sp <- SpatialPoints(lonlatdf_zone11, proj4string=CRS("+proj=longlat +datum=WGS84"))
utm_zone_11S_utm <- spTransform(lonlat_11_sp, ("+proj=utm +zone=11  +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
utm_zone_11S_utm <- as.data.frame(utm_zone_11S_utm)
zone_11 <- cbind(utm_zone_11S, utm_zone_11S_utm)
#cleanup
rm(lonlat_11_sp)
rm(lonlatdf_zone11)
rm(utm_zone_11S_utm)
write_csv(zone_11, "zone_11.csv")
write_csv(utm_zone_11S, "zone_11_small.csv")
rm(utm_zone_11s)


#now filter out cities and their bounding boxes. pixels are 30m, need 1667 to get ~25 km. 
#take each city's coordinates
zone_11 <- read_csv("./zone_11.csv")

SB_x <- 252030
SB_y <- 3812120

box_max_x <- SB_x + 1667
box_min_x <- SB_x - 1667
box_max_y <- SB_y 
box_min_y <- SB_y 


#find them in the table and filter out all rows where x and y = the city +/- 1667 pixels



SB <- zone_11 %>%
  filter(utm_zone_11S.longitude > box_min_x) %>%
  filter(utm_zone_11S.longitude < box_max_x) %>%
  filter(biomass > -1) #remove clouds

write_csv(SB, "SB.csv")




#tangent for ggmap to see what this looks like becfause the histogram isn't promising
#ggmap makes it look like everything is a-ok - prefiltered! 
  
library(ggmap)
base_map <- get_map("santa barbara", zoom =10, source = "stamen")  

ggmap(base_map) +
  geom_point(data = SB, aes(x = longitude, y = latitude, color = biomass), alpha = 0.1) +
  geom_point(data = SB2, aes(x = longitude, y = latitude, color = biomass), alpha = 0.1)+ 
  facet_wrap(~date_utc)

#now make boxes for all cities
#LA

#convert
lall <- data.frame(longitude = -118.253734,latitude = 33.717393)
lallsp <- SpatialPoints(lall, proj4string=CRS("+proj=longlat +datum=WGS84"))
la_utm <- spTransform(lallsp, ("+proj=utm +zone=11  +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
la_utm <- as.data.frame(la_utm)


LA_x <- 383834
LA_y <- 3731527 
box_max_x <- LA_x 
box_min_x <- LA_x 
box_max_y <- LA_y + 1667
box_min_y <- LA_y - 1667
#find them in the table and filter out all rows where x and y = the city +/- 1667 pixels
LA <- zone_11 %>%
  filter(utm_zone_11S.latitude > box_min_y) %>%
  filter(utm_zone_11S.latitude < box_max_y) %>%
  filter(biomass > -1) #remove clouds

write_csv(LA, "LA.csv")

#San Diego
SD_x <- 475877
SD_y <- 3633875
box_max_x <- SD_x 
box_min_x <- SD_x 
box_max_y <- SD_y + 1667
box_min_y <- SD_y - 1667
#find them in the table and filter out all rows where x and y = the city +/- 1667 pixels
SD <- zone_11 %>%
  filter(utm_zone_11S.latitude > box_min_y) %>%
  filter(utm_zone_11S.latitude < box_max_y) %>%
  filter(biomass >-1) #remove clouds

write_csv(SD, "SD.csv")


#San Francisco
zone_10 <- read_csv("./zone_10.csv")

SF_x <- 550083
SF_y <- 4180889

box_max_x <- SF_x + 1667
box_min_x <- SF_x - 1667
box_max_y <- SF_y + 1667
box_min_y <- SF_y - 1667
#find them in the table and filter out all rows where x and y = the city +/- 1667 pixels
SF <- zone_10 %>%
  filter(utm_zone_10S.latitude > box_min_y) %>%
  filter(biomass >-1) #remove clouds

write_csv(SF, "SF.csv")


#then replace biomass with presence/absence


SB <- SB %>%
  mutate(kelp = ifelse(SB$biomass >0, 1, 0)) 
write_csv(SB, "SB.csv")

SD <- SD %>%
  mutate(kelp = ifelse(SD$biomass >0, 1, 0))
write_csv(SD, "SD.csv")

LA <- LA %>%
  mutate(kelp = ifelse(LA$biomass >0, 1, 0))
write_csv(LA, "LA.csv")

SF <- SF %>%
  mutate(kelp = ifelse(SF$biomass >0, 1, 0))
write_csv(SF, "SF.csv")
#add site codes
SB <- SB %>%
  mutate(site = rep("SB", times = nrow(SB)))
write_csv(SB, "SB.csv")

SD <- SD %>%
  mutate(site = rep("SD", times = nrow(SD)))
write_csv(SD, "SD.csv")

LA <- LA %>%
  mutate(site = rep("LA", times = nrow(LA)))
write_csv(LA, "LA.csv")

SF <- SF %>%
  mutate(site = rep("SF", times = nrow(SF)))
write_csv(SF, "SF.csv")

#and create one full dataframe
#first rename columns
names(SB)[5:6] =
  c("UTM_x", "UTM_y")
names(SD)[5:6] =
  c("UTM_x", "UTM_y")
names(LA)[5:6] =
  c("UTM_x", "UTM_y")
names(SF)[5:6] =
  c("UTM_x", "UTM_y")

kelp <- rbind(SB, SD, LA)
write_csv(kelp, "kelp.csv")

#now expand dates to make quarterly averages more straightforward
library(lubridate)

kelp <- kelp %>%
  mutate(date = ymd(kelp$date_utc))

kelp <- kelp %>%
  separate(date, c("year", "month", "day"), sep = "-")

kelp$year <- as.numeric(kelp$year)
kelp$month <- as.numeric(kelp$month)
kelp$day <- as.numeric(kelp$day)

kelp <- kelp %>%
  mutate(qtr = ifelse(kelp$month == 01| kelp$month == 02| kelp$month ==  3, 1, 
         ifelse(kelp$month == 04| kelp$month == 05| kelp$month == 6, 2, 
         ifelse(kelp$month == 07| kelp$month == 08| kelp$month == 9, 3, 
         4))))

kelp <- kelp %>%
  select(-qtr)

#great, this looks good. now do the same for control sites.   
#make control site origins as OG sites - 50 km
#convert

control_latitude <- c(34.047543, 32.240051, 33.307471)
control_longitude <- c(-118.950585,-116.926515, -117.499485)

controls <- data.frame(longitude = control_longitude ,latitude =control_latitude )
controls <- SpatialPoints(controls, proj4string=CRS("+proj=longlat +datum=WGS84"))
controls_utm <- spTransform(controls, ("+proj=utm +zone=11  +ellps=WGS84 +datum=WGS84 +units=m +no_defs"))
controls_utm <- as.data.frame(controls_utm)



SB_control_x <- 319955 
SB_control_y <- 3769144 


LA_control_x <- 453502
LA_control_y <- 3685485

SD_control_x <- 506922
SD_control_y <- 3567046 

SF_control_x <- 550083 - 3334
SF_control_y <- 4180889 - 3334


#now do the filters

#SB_control
box_max_x <- SB_control_x + 1667
box_min_x <- SB_control_x - 1667
box_max_y <- SB_control_y 
box_min_y <- SB_control_y 
SB_control <- zone_11 %>%
  filter(utm_zone_11S.longitude > box_min_x) %>%
  filter(utm_zone_11S.longitude < box_max_x) %>%
  filter(biomass > -1) #remove clouds

write_csv(SB_control, "SB_control.csv")

#LA_control
box_max_x <- LA_control_x 
box_min_x <- LA_control_x 
box_max_y <- LA_control_y + 1667
box_min_y <- LA_control_y - 1667
LA_control <- zone_11 %>%
  filter(utm_zone_11S.latitude > box_min_y) %>%
  filter(utm_zone_11S.latitude < box_max_y) %>%
  filter(biomass > -1) #remove clouds

write_csv(LA_control, "LA_control.csv")

#SD_control
box_max_x <- SD_control_x 
box_min_x <- SD_control_x 
box_max_y <- SD_control_y + 1667
box_min_y <- SD_control_y - 1667
SD_control <- zone_11 %>%
  filter(utm_zone_11S.latitude > box_min_y) %>%
  filter(utm_zone_11S.latitude < box_max_y) %>%
  filter(biomass > -1) #remove clouds

write_csv(SD_control, "SD_control.csv")

#SF_control
box_max_x <- SF_control_x + 1667
box_min_x <- SF_control_x - 1667
box_max_y <- SF_control_y + 1667
box_min_y <- SF_control_y - 1667
SF_control <- zone_10 %>%
  filter(utm_zone_10S.latitude > box_min_y) %>%
  filter(biomass > -1) #remove clouds

write_csv(SF_control, "SF_control.csv")

#now the post processing

SB_control <- read_csv("./SB_control.csv")
LA_control <- read_csv("./LA_control.csv")
SD_control <- read_csv("./SD_control.csv")
SF_control <- read_csv("./SF_control.csv")


SB_control <- SB_control %>%
  mutate(kelp = ifelse(SB_control$biomass >0, 1, 0)) 
SD_control <- SD_control %>%
  mutate(kelp = ifelse(SD_control$biomass >0, 1, 0))
LA_control <- LA_control %>%
  mutate(kelp = ifelse(LA_control$biomass >0, 1, 0))
SF_control <- SF_control %>%
  mutate(kelp = ifelse(SF_control$biomass >0, 1, 0))

#add site codes
SB_control <- SB_control %>%
  mutate(site = rep("SB_control", times = nrow(SB_control)))
SD_control <- SD_control %>%
  mutate(site = rep("SD_control", times = nrow(SD_control)))
LA_control <- LA_control %>%
  mutate(site = rep("LA_control", times = nrow(LA_control)))
SF_control <- SF_control %>%
  mutate(site = rep("SF_control", times = nrow(SF_control)))

#and create one full dataframe
#first rename columns
names(SB_control)[5:6] =
  c("UTM_x", "UTM_y")
names(SD_control)[5:6] =
  c("UTM_x", "UTM_y")
names(LA_control)[5:6] =
  c("UTM_x", "UTM_y")
names(SF_control)[5:6] =
  c("UTM_x", "UTM_y")

controls <- rbind(SB_control, SD_control, LA_control)
write_csv(controls, "controls.csv")

#dates

controls <- controls %>%
  mutate(date = ymd(controls$date_utc))

controls <- controls %>%
  separate(date, c("year", "month", "day"), sep = "-")

controls$year <- as.numeric(controls$year)
controls$month <- as.numeric(controls$month)
controls$day <- as.numeric(controls$day)

controls <- controls %>%
  mutate(qtr = ifelse(controls$month == 01| controls$month == 02| controls$month ==  3, 1, 
                      ifelse(controls$month == 04| controls$month == 05| controls$month == 6, 2, 
                             ifelse(controls$month == 07| controls$month == 08| controls$month == 9, 3, 
                                    4))))





#attach the cities to their controls

kelp_cities <- read_csv("./kelp.csv")
kelp_all_sites <- rbind(kelp, controls)
write_csv(kelp_all_sites, "kelp_all_sites.csv")

kelp_all_sites <- read_csv("./kelp_all_sites.csv")

kelp_all_sites <- kelp_all_sites %>%
  group_by(site,year, qtr) %>%
  mutate(max_extent = sum(kelp)) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(max_extent = max(max_extent))

kelp_all_sites_summary <- kelp_all_sites %>%
  group_by(site, year, qtr) %>%
  summarise(quarterly_extent = sum(kelp), max_extent = mean(max_extent))%>%
  mutate(percent_coverage = (quarterly_extent/max_extent)*100 ) %>%
  ungroup() %>%
  group_by(site) %>%
  mutate(time_point = row_number()) %>%
  ungroup()
  
write_csv(kelp_all_sites_summary, "kelp_all_sites_summary.csv")



kelp_all_sites <- kelp_all_sites %>%
  group_by(site) %>%
  mutate(time_point = row_number())



ggplot(data = kelp_all_sites_summary, aes(x = time_point, y = percent_coverage, color = site)) +
  geom_line() +
  facet_wrap(~site)

