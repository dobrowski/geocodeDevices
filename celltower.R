

library(here)

cell <- read_csv(here("data","cell_towers.csv"))

# https://www.opencellid.org/#zoom=16&lat=37.77889&lon=-122.41942

cell_mry <- cell %>%
    filter(lon > -122.2,
           lon < -120.0,
           lat > 35.7,
           lat < 36.9)  %>%
    mutate(range_mi = range/1609.34) %>% 
    as_tibble() %>%
    st_as_sf(coords = c("lon", "lat"), 
             crs = 4326, agr = "constant")
    

buf <- st_buffer(cell_mry, dist = 0.011187907319540488 )   # arc degrees 1km at 36.45 latitude


tmap_mode("view")


 tm_shape(monterey_tracts) +
    tm_borders() +
    tm_shape(buf) +
    tm_polygons()
 tm_shape(cell_mry) +
     tm_dots()
 
