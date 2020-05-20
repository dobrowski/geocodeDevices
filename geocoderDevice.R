

library(tidyverse)
library(tidygeocoder)
library(sf)
library(tmap)
library(tigris)
library(censusapi)
library(lubridate)
library(ggmap)
library(RColorBrewer)
library(mapview)
library(plotly)


# census_api_key( xxx, install = TRUE )
# register_google(key = xxx)

options(tigris_class = "sf")
options(tigris_use_cache = TRUE)




### Get geography for tracts and county of monterey and only use the land areas  ------------

monterey_tracts <- tracts(state = "CA",county =  "Monterey", class = "sf", year = "2019") %>%
    filter(ALAND > 0) %>% 
 #   st_as_sf(monterey_tracts) %>%
    st_transform(4269) %>% 
    st_union()


monterey_county <- counties("CA", class = "sf", year = "2019") %>%
    filter(NAME == "Monterey")  %>%
    st_transform(4269)


plot(monterey_tracts)






dc_addresses <- tribble( ~name,~addr,
                         "White House", "1600 Pennsylvania Ave Washington, DC",
                         "National Academy of Sciences", "2101 Constitution Ave NW, Washington, DC 20418",
                         "Department of Justice", "950 Pennsylvania Ave NW, Washington, DC 20530",
                         "Supreme Court", "1 1st St NE, Washington, DC 20543",
                         "Washington Monument", "2 15th St NW, Washington, DC 20024")



lat_longs <- dc_addresses %>% 
    geocode(addr,lat=latitude,long=longitude)


ggplot(soledad.ll %>% filter(!is.na(longitude)),aes(longitude, latitude),color="grey98") +
    borders("county") + 
    theme_classic() + 
    geom_point() +
    theme(line = element_blank(),text = element_blank(),title = element_blank()) +
#    geom_label_repel(aes(label =name),show.legend=F) +
    scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL)


soledad.ll.2 <- soledad.ll %>%
    filter(!is.na(longitude)) %>%
    st_as_sf(coords = c("longitude", "latitude"), crs= 4269) 


plot(soledad.ll.2)



ggplot() +
    geom_sf(data=monterey_tracts, color = "black", fill=NA, size=0.5) +
    geom_sf(data=soledad.ll.2, size=1, aes(color="red")) +
    labs(x=NULL, y=NULL, 
         title="title",
         subtitle=today(),
         caption="Source: ") +
    theme(plot.title=element_text(face="bold", family="Arial", size=13)) +
    theme(plot.caption=element_text(face="bold",
                                    family="Arial",
                                    size=7,
                                    color="gray",
                                    margin=margin(t=10, r=80))) +
    theme_bw() +
    theme(legend.position="none") +
    coord_sf(xlim = c(-122.2, -120.0), ylim = c(35.8, 36.9), datum = NA) 




tmap_mode("view")

need.map <- tm_shape(monterey_tracts) +
    tm_borders() +
    tm_shape(soledad.ll.2) +
    tm_dots()

tmap_mode("plot")

tmap_save(need.map, "Need-map.html")


### GGmap version ----

monterey_bounds <- c(left = -122.1, bottom = 35.7, right = -120.7, top = 36.9)
soledad_bounds <-  c(left = -121.39, bottom = 36.4, right = -121.28, top = 36.47)

sole.stamen <- get_stamenmap(soledad_bounds, zoom = 14, maptype = "terrain")



ggmap(sole.stamen, legend = "none") + 
     stat_density2d(data = soledad.ll, 
                    aes(x= longitude, y= latitude, fill=..level.., alpha=..level..), 
                    geom = "polygon") +
     scale_fill_gradientn(colours = rev(brewer.pal(7, "Spectral")))


 
 ### Mapview ---
 
 soledad.ll.3 <- soledad.ll %>%
     filter(!is.na(longitude))
 
mapview(soledad.ll.3,
        xcol = "longitude", ycol = "latitude", crs = 4269, grid = FALSE)
    

