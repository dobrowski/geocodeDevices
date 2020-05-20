#  The purpose of this file is to collate all of the address files into a single file. 


### Load Libraries -----


library(tidyverse)
library(googlesheets4)
library(readxl)
library(here)
library(ggmap)

library(rgeocodio)


### Load data ----


gusd.google <- "16qWp1p03lbTrB7fpW-k7fFSoBqEk_IqEP7V2PrHFZyI"

gusd.1 <- read_sheet(gusd.google, sheet = 1) %>%
    select(`Address:`, Latitude, Longitude)
gusd.2 <- read_sheet(gusd.google, sheet = 2) %>%
    select(`Address:`, Latitude, Longitude)
gusd.3 <- read_sheet(gusd.google, sheet = 3) %>%
    select(`Address:`, `Latitude:`, `Longitude:`)
gusd.4 <- read_sheet(gusd.google, sheet = 4) %>%
    select(`Address:`, Latitude, Longitude)
gusd.5 <- read_sheet(gusd.google, sheet = 5) %>%
    filter(`NO wifi` == "x" | `NO wifi` == "X") %>%
    select(`Address:`, Latitude, Longitude)


ism <- read_sheet("https://docs.google.com/spreadsheets/d/1GT3Y9khI3lu42cqq5o3aJOhaPL0ZL5rY4GuJD1nXYtM")


soledad <- read_excel(here::here("data","SURVEY NO INTERNET ADDRESS Only.xlsx"))

sanlucas <- read_excel(here::here("data","San Lucas Addresses.xlsx"))%>%
    select(addr = `Address`) %>%
    mutate(LEA = "San Lucas")



greenfield <- gusd.5 %>%
    select(addr = `Address:`) %>%
    mutate(LEA = "Greenfield")


soledad2 <- soledad %>%
    transmute(addr = paste0(Address,", ", City, ", " , State, ", ", `Zip Code`),
              LEA = "Soledad")

ism2 <- ism %>% 
    transmute(addr = paste0(Address, ", 93955"),
              LEA = "International School")


all.addresses <- bind_rows(greenfield, soledad2, ism2, sanlucas)

write_csv(all.addresses, paste0("All addresses ", lubridate::today(),".csv"))


all.addresses.ll <- all.addresses %>%
 #   mutate(addr = str_remove_all(addr,"[.]")) %>%
    filter(!str_detect(addr, "apt|Apt|Unit")) %>%
    tidygeocoder::geocode(addr,lat=latitude,long=longitude, method = "cascade")







gio_geocode()


soledad.ll <- soledad %>% 
    mutate(addr = paste0(Address, ", ", City, ", ", `Zip Code`)) %>%
    tidygeocoder::geocode(addr,lat=latitude,long=longitude, method = "cascade")


soledad.without <- soledad.ll %>%
    filter(is.na(longitude)) 

google.text <- ggmap::geocode(all.addresses$addr, output = "latlon", source = "google")


    
