# produces static choropleth maps from excel file coded with LSOA code
# could read boundaries direct from the web, I just don't have permission to do so so have to be downloaded first

library(tmap)
library(dplyr)
library(sf)
library(stringr)
library(readxl)

# LSOA boundaries 2011 (current)
# from https://geoportal.statistics.gov.uk/datasets/lower-layer-super-output-areas-december-2011-boundaries-ew-bgc
# read in LSOA boundaries
  lsoas_2011 <- st_read("Lower_Layer_Super_Output_Areas_December_2011_Boundaries_EW_BGC.shp")

# read in age data
  age_data <- read_xlsx("Risk Factors Workbook.xlsx")

# filter bolton only
  bolton_lsoas <- lsoas_2011 %>%
    # create field for LA from LSOA names which includes LA name
    mutate(borough = str_sub(LSOA11NM, 1, nchar(as.character(LSOA11NM))-5)) %>%
    filter(borough == "Bolton") %>%
    # join age data onto lsoa geogrpahy (doing it this way round keeps the geography)
    left_join(age_data, by = c("LSOA11CD" = "LSOACode"))

  # st_transform(crs = 4326) # transforms to lat/ long from OSGB36 # only need this in web mapping

# get ward boundary data to overlay onto LSOA data  
# from https://geoportal.statistics.gov.uk/datasets/wards-december-2011-boundaries-ew-bgc
wards <- st_read("Wards_December_2011_Boundaries_EW_BGC.shp")
bolton_wards <- wards %>%
  mutate(borough = str_sub(wd11cdo, 1, nchar(as.character(wd11cdo))-2)) %>%
  filter(borough == "00BL")


# maps ##################################################################
# sized for when you save them, looks a bit wierd on screen 

# map of 65+ population
sixtyfive_plus_map <- 
tm_shape(bolton_lsoas) +
  tm_fill("Aged 65+", #palette = "BuPu", 
          title = "Number of residents aged 65+") +
  tm_shape(bolton_wards) +
  tm_polygons(alpha = 0, lwd = 2, border.col = "black") +
  tm_text("wd11nm", 
          fontface = "bold", 
          #size = 0.5,
          bg.color = "white", # bg = background
          bg.alpha = 0.7, 
          auto.placement = 0.1,
          remove.overlap = FALSE
          ) +
  tm_layout(frame = FALSE, scale = 0.5,
            legend.title.size = 1.5,
            legend.text.size = 1)

# save map
  tmap_save(sixtyfive_plus_map, "sixtyfive_plus_map.png")  

# map of 70+ population 
seventy_plus_map <- 
  tm_shape(bolton_lsoas) +
  tm_fill("70+", #palette = "BuPu", 
          title = "Number of residents aged 70+",
          breaks = c(100, 200, 300, 400, 500)) + # change breaks so they're the same on both maps
  tm_shape(bolton_wards) +
  tm_polygons(alpha = 0, lwd = 2, border.col = "black") + # lwd = line width
  tm_text("wd11nm", 
          fontface = "bold", 
          #size = 0.5,
          bg.color = "white", # bg = background
          bg.alpha = 0.7, 
          auto.placement = 0.1,
          remove.overlap = FALSE
  ) +
  tm_layout(frame = FALSE, scale = 0.5,
            legend.title.size = 1.5,
            legend.text.size = 1)

# save map
  tmap_save(seventy_plus_map, "seventy_plus_map.png")  
