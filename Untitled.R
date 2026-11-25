

eez_shp <- readRDS(paste0("/Users/alicepidd/Documents/PhD/data/conn_data/shapefiles/EEZ_shapefile.RDS"))
mpa_shp <- readRDS(paste0("/Users/alicepidd/Documents/PhD/data/conn_data/shapefiles/MPA_joined_shapefile.RDS"))

unique(mpa_shp$IUCN)
hexpal
hexpal <- c("#001219", "#005F73", "#0a9396", "#e9d8a6", "#ee9b00", "#ae2012") # Script 7


ggplot() +
  geom_sf(data = mpa_shp #%>% filter(IUCN != "NA" & IUCN != "VI")
          , 
          aes(fill = IUCN),
          linewidth = 0.01) +
  scale_fill_manual(values = hexpal) +
  theme_void()

# mpa_shp %>% filter(IUCN != "NA" & IUCN != "VI")
