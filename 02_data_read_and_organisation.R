
world_map <- ne_countries(returnclass = "sf")

hex_points <- spsample(as_Spatial(world), type = "hexagonal", cellsize = 0.1667)

hex_grid <- st_as_sf(HexPoints2SpatialPolygons(hex_points, dx = 0.1667),
                     crs = 4326)




# Download current WorldClim data
# Parameters:
# var: "bio" for bioclimatic variables, 
# "tmin" for minimum temperature, etc.
# res: resolution in minutes (e.g., 10 = 10 arc-minutes)
current_climate <- worldclim_global(var = "bio", res = 10, 
                                    path = "climate_data")

# Load the bioclimatic variables as a RasterStack
current_stack <- stack(current_climate)

# Parameters:
# model: Climate model (e.g., "ACCESS-ESM1-5")
# ssp: Shared Socioeconomic Pathway (e.g., "370" for SSP3-7.0)
# year: Projection year (e.g., 2040, 2070)
future_climate <- cmip6_world(
  model = "ACCESS-ESM1-5",
  ssp = "370",   # SSP 3-7.0 scenario
  time = "2061-2080", # Late-century time period
  var = "bio",   # Bioclimatic variables
  res = 10,      # Resolution
  path = "climate_data"
)

# Load the future climate data as a RasterStack
future_stack <- stack(future_climate)

names(future_stack) <- names(current_stack)

for(dirs in list.dirs("../Raw_data/"))
{
  flist<-list.files(dirs)
  flist<-flist[grep(c("xlsx|csv"), flist)]
  
  for(file2open in flist)
  {
    cat(dirs, file2open, sep = "/")
    cat("\n")
    
    if(grepl("csv", file2open))
    { dat <- read_delim(paste(dirs, file2open, sep = "/"), delim = "\t",
                                      locale = locale(encoding = "UTF-8"))} 
    else
      {dat<-read.xlsx(paste(dirs, file2open, sep = "/"), sheet = 1)}
    fname<-strsplit(file2open, ".", fixed = T)[[1]][1]
    fname<-gsub(" ", "_", fname)
    
    assign(fname, dat)
    
  }

}

str(Aphis_predators)

gbif_data = Helicoverpa_armigera_distribution

cleaned_data <- gbif_data %>%
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude), # Remove records with missing coordinates
    decimalLatitude >= -90 & decimalLatitude <= 90,    # Latitude range
    decimalLongitude >= -180 & decimalLongitude <= 180, # Longitude range
    !(decimalLatitude == 0 & decimalLongitude == 0) ) |>  # Remove records with 0,0 coordinates 
  filter(is.na(coordinateUncertaintyInMeters) | coordinateUncertaintyInMeters <= 100000) |>  # Remove records with high coordinate uncertainty
  filter(!is.na(species) & !is.na(genus),        # Keep records with valid species and genus
    taxonRank %in% c("SPECIES", "SUBSPECIES")) |>  # Ensure taxon rank is meaningful
  filter(basisOfRecord %in% c("HUMAN_OBSERVATION",
    "MACHINE_OBSERVATION",
    "OBSERVATION",
    "PRESERVED_SPECIMEN")) |> # Remove less reliable or irrelevant records (e.g., fossils, unknowns)
  dplyr::select(gbifID, occurrenceID, order, family, genus, species,
    taxonRank, scientificName, countryCode, stateProvince, 
    locality, decimalLatitude, decimalLongitude,
    coordinateUncertaintyInMeters, eventDate, basisOfRecord, 
    individualCount) |> # Drop unnecessary columns to reduce the size of the dataset
  distinct(occurrenceID, .keep_all = TRUE) # Use `occurrenceID` to remove exact duplicates

cat("Original dataset size: ", nrow(gbif_data), "rows.\n")
cat("Cleaned dataset size: ", nrow(cleaned_data), "rows.\n")
world <- ne_countries(scale = "medium")


# filtering off terrestrial areas
world<- world |> 
  filter(!continent == "Antarctica") |> 
  st_union()




occ_points<-st_as_sf(cleaned_data,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326, # WGS84 CRS
  remove = FALSE)

Sys.time()
intersects <- st_intersects(hex_grid, occ_points)
Sys.time() #35 mins

# Count points in each polygon
polygon_counts <- sapply(intersects, length)

# Add counts back to the polygons
hex_grid$observation_count <- polygon_counts

ggplot() +
  # geom_sf(data = world_map) +
  geom_sf(data = hex_grid, aes(fill = log(observation_count)), 
          color = "black")+
  scale_fill_viridis_c(option = "plasma", na.value = "grey90") +
  #geom_sf(data = occ_points)
  theme_minimal() +
  labs(
    title = "GBIF Observations by Polygon",
    fill = "Observation Count"
  )

