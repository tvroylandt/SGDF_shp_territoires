# ------------------------------- #
# Gestion fond de cartes territoires - CR #
# ------------------------------- #

library(tidyverse)
library(sf)
library(maptools)

# Chargement fonds de cartes ----------------------------------------------
shp_terr <- st_read("Shapefiles/Initial/TERRITOIRE-N.shp") 

# Gestion codes CR --------------------------------------------------------
# Il s'agit d'un endroit dans les Yvelines ou il n'y a pas de groupes et donc pas de territoires
# On le rattache au CR IDF Ouest afin de ne pas avoir de "trou" dans la carte
# On recode aussi Saint-Pierre et Miquelon avec le même code que Outre-Mer Monde
shp_terr <- shp_terr %>%
  mutate(CR = fct_recode(CR,
                         "10" = "0",
                         "80" = "90"))

# Ajout de variables ------------------------------------------------------
shp_terr_r <- shp_terr %>%
  mutate(lon_terr = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat_terr = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]),
         code_cr = as.character(CR),
         lib_cr = fct_recode(code_cr,
                             "Île-de-France Ouest" = "10",
                             "Île-de-France Est" = "11",
                             "Nord" = "20",
                             "Nord-est" = "30",
                             "Centre est" = "40",
                             "Méditerrannée" = "50",
                             "Sud-ouest" = "60",
                             "Ouest" = "70",
                             "Outre-mer Monde" = "80"),
         code_terr_long = as.character(CODE_TER),
         code_terr_court = str_sub(COD_TER, 1, 5)) %>%
  rename(lib_terr = TERR_SGDF) %>%
  select(code_terr_long, code_terr_court, lib_terr, 
         lon_terr, lat_terr, code_cr, lib_cr, geometry)

# Ajout DOM ---------------------------------------------------------------
# Fonction de transformation
transformation_shp <- function(object, rot, scale, shift){
  object %>% elide(rotate = rot) %>%
    elide(scale = scale) %>% 
    elide(shift = shift)     
}

# France metropolitaine
shp_metro <- shp_terr_r %>%
  filter(code_cr != "80") %>%
  st_transform(4326) %>%
  as("Spatial")

# Martinique
shp_martinique <- shp_terr_r %>%
  filter(lib_terr == "MARTINIQUE") %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 1, shift = c(-2, 40.8)) 

proj4string(shp_martinique) <- proj4string(shp_metro)

# Guadeloupe
shp_guadeloupe <- shp_terr_r %>%
  filter(lib_terr == "GUADELOUPE") %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 1, shift = c(0, 41)) 

proj4string(shp_guadeloupe) <- proj4string(shp_metro)

# Saint-Martin
shp_saint_martin <- shp_terr_r %>%
  filter(lib_terr == "SAINT-MARTIN") %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 0.5, shift = c(-1.8, 39.8)) 

proj4string(shp_saint_martin) <- proj4string(shp_metro)

# Guyane
shp_guyane <- shp_terr_r %>%
  filter(lib_terr == "GUYANE") %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 1, shift = c(2, 41)) 

proj4string(shp_guyane) <- proj4string(shp_metro)

# La Réunion
shp_reunion <- shp_terr_r %>%
  filter(lib_terr == "REUNION") %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 1, shift = c(4, 41)) 

proj4string(shp_reunion) <- proj4string(shp_metro)

# Saint-Pierre et Miquelon
shp_saint_pierre_miquelon <- shp_terr_r %>%
  filter(lib_terr == "SAINT-PIERRE ET MIQUELON") %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 0.8, shift = c(0, 39.5)) 

proj4string(shp_saint_pierre_miquelon) <- proj4string(shp_metro)

# Mayotte
shp_mayotte <- shp_terr_r %>%
  filter(lib_terr == "MAYOTTE") %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 1, shift = c(2, 39.5)) 

proj4string(shp_mayotte) <- proj4string(shp_metro)

# Polynésie
shp_polynesie <- shp_terr_r %>%
  filter(lib_terr == "POLYNESIE FRANCAISE") %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 1.2, shift = c(6, 40.2)) 

proj4string(shp_polynesie) <- proj4string(shp_metro)

# Nouvelle-Calédonie
shp_nouvelle_caledonie <- shp_terr_r %>%
  filter(lib_terr == "NOUVELLE CALEDONIE") %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 2.3, shift = c(3.5, 39.8)) 

proj4string(shp_nouvelle_caledonie) <- proj4string(shp_metro)

# Reunification
shp_terr_total <- shp_metro %>%
  rbind(shp_martinique) %>%
  rbind(shp_guadeloupe) %>%
  rbind(shp_saint_martin) %>%
  rbind(shp_guyane) %>%
  rbind(shp_saint_pierre_miquelon) %>%
  rbind(shp_mayotte) %>%
  rbind(shp_nouvelle_caledonie) %>%
  rbind(shp_polynesie) %>%
  rbind(shp_reunion) %>%
  spTransform(CRS("+init=epsg:2154")) %>% 
  st_as_sf()

# Highlight IDF -----------------------------------------------------------
shp_idf <- shp_terr_r %>%
  filter(code_cr %in% c("10", "11")) %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  transformation_shp(rot = 0, scale = 4, shift = c(7.5, 49.5)) 

proj4string(shp_idf) <- proj4string(shp_metro)

shp_terr_total_idf <- shp_terr_total %>%
  st_transform(4326) %>%
  as("Spatial") %>% 
  rbind(shp_idf) %>%  
  spTransform(CRS("+init=epsg:2154")) %>% 
  st_as_sf() 

# Fond de carte CR --------------------------------------------------------
# Version normale
shp_cr <- shp_terr_r %>%
  group_by(code_cr, lib_cr) %>%
  summarise() %>%
  mutate(lon_cr = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat_cr = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

# Version DOM
shp_cr_total <- shp_terr_total %>%
  group_by(code_cr, lib_cr) %>%
  summarise() %>%
  mutate(lon_cr = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat_cr = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))

# Version DOM - IDF
shp_cr_total_idf <- shp_terr_total_idf %>%
  group_by(code_cr, lib_cr) %>%
  summarise() %>%
  mutate(lon_cr = map_dbl(geometry, ~st_point_on_surface(.x)[[1]]),
         lat_cr = map_dbl(geometry, ~st_point_on_surface(.x)[[2]]))


# Encart polygones --------------------------------------------------------
poly_dom <- tibble(x_dom = c(250000, 1100000, 1100000, 250000), 
                   y_dom = c(5780000, 5780000, 6120000, 6120000))

poly_idf <- tibble(x_idf = c(1000000, 1320000, 1320000, 1000000), 
                   y_idf = c(6940000, 6940000, 7200000, 7200000))

poly_encart <- bind_cols(poly_dom, poly_idf)

# Export ------------------------------------------------------------------
# Fonds de cartes Territoires
st_write(shp_terr_r, "Shapefiles/Territoires/shp_terr.shp", delete_dsn = TRUE)
st_write(shp_terr_total, "Shapefiles/Territoires/shp_terr_dom.shp", delete_dsn = TRUE)
st_write(shp_terr_total_idf, "Shapefiles/Territoires/shp_terr_dom_idf.shp", delete_dsn = TRUE)

# Fonds de cartes CR
st_write(shp_cr, "Shapefiles/Centres_ressources/shp_cr.shp", delete_dsn = TRUE)
st_write(shp_cr_total, "Shapefiles/Centres_ressources/shp_cr_dom.shp", delete_dsn = TRUE)
st_write(shp_cr_total_idf, "Shapefiles/Centres_ressources/shp_cr_dom_idf.shp", delete_dsn = TRUE)

# Encarts
write_rds(poly_encart, "Shapefiles/encarts_polygones_dom_idf.rds")
