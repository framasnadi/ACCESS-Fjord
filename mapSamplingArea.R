#############################################################
#                                                           #
#   Author: Francesco Masnadi                               #
#                                                           #
#############################################################

# Install and load necessary packages
#install.packages("ggOceanMaps")
library(sf)
library(ggplot2)
library(terra)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)
library(ggnewscale)
library(ggOceanMaps)
# Load the libraries
library(osmdata)
library(sf)
library(ggplot2)

setwd("C:/Users/frma6502/Desktop/Other/ShinyAPPnorway/ACCESS-FjordAPP")
# Load the NetCDF bathymetric data
# Replace "path_to_netcdf" with the actual path to your NetCDF file
bathymetry <- rast("Mean depth natural colour (with land)/Mean depth natural colour (with land).nc")
bathymetry <- project(bathymetry, "EPSG:4326")
# Check the structure of the raster
print(bathymetry)


# Define the bounding box for the Sørfold area
sorfold_bbox_bat <- ext(15.1,  15.8 , 67.425,67.6)  # Bounding box: xmin, xmax, ymin, ymax
bbox <- c(15.1, 67.425, 15.8, 67.6)

# Query OSM for data within the bounding box 
osm_data_coast <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'natural', value = "coastline") %>%
  osmdata_sf()

osm_data_wt <- opq(bbox = bbox) %>%
  add_osm_feature(key = 'natural', value = "water") %>%
  osmdata_sf()

#unique(land_poly$landuse)
land_polygons <- opq(bbox = bbox) %>%
  add_osm_feature(key = "landuse", value="aquaculture") %>%
  osmdata_sf()

buil_poly <- opq(bbox = bbox) %>%
  add_osm_feature(key = "building") %>%
  osmdata_sf()

# Extract areas
water_areas <- osm_data_wt$osm_multipolygons
coastlines <- osm_data_coast$osm_lines
land_poly <- land_polygons$osm_polygons
buil__pol <- buil_poly$osm_polygons

# Crop the bathymetry data to the Sørfold area
bathymetry_cropped <- crop(bathymetry, sorfold_bbox_bat)

# Convert the cropped raster to a data frame for ggplot2
bathy_df <- as.data.frame(bathymetry_cropped, xy = TRUE, na.rm = TRUE)

# Rename the depth column (assume the first layer contains the depth data)
colnames(bathy_df)[3] <- "Depth"

# Categorize depth values
bathy_df$depth_category <- cut(
  bathy_df$Depth,
  breaks = c( -100, -80, -50, -25, -15, max(bathy_df$Depth)),  # 8 break points
  labels = c( "80-100 m", "50-80 m", "25-50 m", "15-25 m", "0-15 m"),  # 5 labels
  include.lowest = TRUE
)
# Define color palette

# Define color palette
colors <- c(
  "Land" = "palegoldenrod",  # Land in pale yellow
#  "0-5 m" = "#10eeff",       # Light blue
  "0-15 m" = "#cceeff", 
  "15-25 m" = "#99ccff",
  "25-50 m" = "#6699ff",
  "50-80 m" = "#3366cc",
  "80-100 m" = "#003399"
 # "> 100 m" = "red"      # Dark blue
)


# Define the coordinates for the stations
stations <- data.frame(
  id = c("FF1","FF2","FF3","FF4","FF5","FF6","FF7"),
  lat = c(67.575,67.535,67.528,67.531, 67.512, 67.458,67.449 ),
  lon = c(15.1268,15.169,15.35,15.45, 15.568,15.51, 15.485  )
)
stations <- data.frame(
  id = c("Stn.B","Stn.A","Stn.C"),
  lat = c(67.517,67.507,67.532 ),
  lon = c(15.482,15.4,15.615)
)

name <- data.frame(
  id = c("Sørfolda ","Aspfjorden","Leirfjorden"),
  lat = c(67.528,67.47,67.528 ),
  lon = c(15.3,15.51,15.585)
)
  
# Convert stations data frame to sf object
stations_sf <- st_as_sf(stations, coords = c("lon", "lat"), crs = 4326)

ggplot() +
  # Background raster for continuous bathymetry (Depth)
  #geom_sf(data =  water_areas, fill ="lightblue") +
  geom_raster(data = bathy_df, aes(x = x, y = y, fill = Depth), alpha = 0.7) +
  scale_fill_viridis_c(option = "D", name = "Depth (m)", direction = +1) +  # Continuous color scale
  # Add a new fill scale
  new_scale_fill() +
  # Overlay raster for filtered discrete bathymetry (Depth_Category)
 # geom_raster(data = bathy_df %>% dplyr::filter(Depth > -100), aes(x = x, y = y, fill = depth_category)) +
 # scale_fill_manual(values = colors, name = "Depth Categories")+
  # Contour lines based on categorized depth
  geom_contour(data = bathy_df, aes(x = x, y = y, z = Depth), 
               breaks = c(-100, -80, -50, -25, -15, -5, 0), 
               color = "darkblue", size = 0.3) +
 # coord_fixed(xlim = c(sorfold_bbox_bat[1], sorfold_bbox_bat[2]), ylim = c(sorfold_bbox_bat[3], sorfold_bbox_bat[4])) +
  labs(
    #title = "Sampling Area: Sørfold",
    x = "Longitude",
    y = "Latitude",
    caption = "Data source: EMODnet Bathymetry"
  ) +
  #geom_sf(data = countries, fill = "yellow", color = NA)+
  #coord_sf(xlim = c(15.1, 15.8), ylim = c( 67.425, 67.6), expand = T)+
  geom_sf(data =  coastlines) +
  #geom_sf(data =  water_areas, fill ="lightblue", color="black")+
  #geom_sf(data =  buil__pol, fill ="green", color="black")+
  geom_sf(data =  land_poly, fill ="purple", color="black")+
  geom_sf(data = stations_sf, color = "red", size = 2)+
  geom_text(data = stations, aes(x = lon, y = lat, label = id),hjust = 1,vjust = -0.7, color = "black",size = 3, fontface = "bold")+
  geom_text(data = name, aes(x = lon, y = lat, label = id),size = 2.7, color = "black")+
  # Add scale bar (defaults to bottom right)
  annotation_scale(location = "bl", width_hint = 0.15) +
  # Add north arrow
  annotation_north_arrow(location = "br", which_north = "true",
                         style = north_arrow_fancy_orienteering) +
  theme_minimal() + theme(panel.grid = element_blank())+
  theme(
    panel.border = element_rect(color = "black", fill = NA, size = 1),
    legend.position = "right"
  )  



bbox_coords <- c(xmin = 15.1,ymin = 67.425, xmax = 15.8, ymax = 67.6)
bbox_sf <- st_as_sfc(st_bbox(bbox_coords, crs = 4326))  # Create an sf polygon from bbox
# Load country polygons from Natural Earth
countries <- ne_countries(scale = "large", returnclass = "sf")
# Clip to your bounding box (only countries that intersect)
countries_clipped <- st_intersection(countries, bbox_sf)
