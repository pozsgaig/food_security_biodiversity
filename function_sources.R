# from https://strimas.com/post/hexagonal-grids/

make_grid <- function(x, type, cell_width, cell_area, clip = FALSE) {
  if (!type %in% c("square", "hexagonal")) {
    stop("Type must be either 'square' or 'hexagonal'")
  }
  
  if (missing(cell_width)) {
    if (missing(cell_area)) {
      stop("Must provide cell_width or cell_area")
    } else {
      if (type == "square") {
        cell_width <- sqrt(cell_area)
      } else if (type == "hexagonal") {
        cell_width <- sqrt(2 * cell_area / sqrt(3))
      }
    }
  }
  # buffered extent of study area to define cells over
  ext <- as(extent(x) + cell_width, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate grid
  if (type == "square") {
    g <- raster(ext, resolution = cell_width)
    g <- as(g, "SpatialPolygons")
  } else if (type == "hexagonal") {
    # generate array of hexagon centers
    g <- spsample(ext, type = "hexagonal", cellsize = cell_width, offset = c(0, 0))
    # convert center points to hexagons
    g <- HexPoints2SpatialPolygons(g, dx = cell_width)
  }
  
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(st_as_sf(g))
}

gbif_cleaning<-function(gbif_data){

cleaned_data <- gbif_data %>%
  filter(
    !is.na(decimalLatitude) & !is.na(decimalLongitude),
    # Remove records with missing coordinates
    decimalLatitude >= -90 & decimalLatitude <= 90, # Latitude range
    decimalLongitude >= -180 & decimalLongitude <= 180, # Longitude range!(decimalLatitude == 0 &
    !(decimalLatitude == 0 & decimalLongitude == 0) ) |> # Remove records with 0,0 coordinates
  filter(is.na(coordinateUncertaintyInMeters) |
           coordinateUncertaintyInMeters <= 100000) |>  # Remove records with high coordinate uncertainty
  dplyr::select(species,
    decimalLatitude,
    decimalLongitude) |> 
  distinct()# # Use `occurrenceID` to remove exact duplicates

clened_data<-st_as_sf(
  cleaned_data,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326,
  # WGS84 CRS
  remove = FALSE
)

clened_data
}

observations2grid<-function(grid, occ_points){
  intersects <- st_intersects(grid, occ_points)
  # Count points in each polygon
  polygon_counts <- sapply(intersects, length)
  
  # Add counts back to the polygons
  out_hex_grid<-grid
  out_hex_grid$observation_count <- polygon_counts
  out_hex_grid
}


make_distribution<-function(stack, occ_points){
  
  occurrence_sf <- st_transform(occ_points, crs = crs(stack))
  
  maxent_model <- maxent(
    x = stack,
    p = st_coordinates(occurrence_sf),
    path = "maxent_output"
  )
  
  predictor_values <- terra::extract(stack, st_coordinates(occurrence_sf))
  
  # Identify rows without NA values
  valid_points <- complete.cases(predictor_values)
  
  # Filter occurrence data
  occurrence_filtered <- occurrence_sf[valid_points, ]
  
  # Check the filtered data
  if (nrow(occurrence_filtered)<30)
    
  {cat("\nThe", nrow(occurrence_filtered), "valid data points may give unreliable predictions!\n")}
  
  
  maxent_model <- maxent(
    x = stack,
    p = st_coordinates(occurrence_filtered),
    path = "maxent_output"
  )
  
  distribution <- predict(maxent_model, stack)
  
  distribution
  
}


convert_to_fractional_degrees <- function(deg_min) {
  # Extract degrees and minutes using regex
  matches <- regmatches(deg_min, regexec("([0-9]+)°([0-9]+\\.[0-9]+)'", deg_min))
  
  if (length(matches[[1]]) < 3) {
    stop("Invalid format. Expected format: 05°06.890'")
  }
  
  degrees <- as.numeric(matches[[1]][2])
  minutes <- as.numeric(matches[[1]][3])
  
  # Convert to fractional degrees
  fractional_degrees <- degrees + (minutes / 60)
  
  return(fractional_degrees)
}

# Function to geocode a location
geocode_location <- function(location) {
  result <- tryCatch({
    geocode(location, output = "latlona", source = "google")
  }, error = function(e) {
    return(data.frame(lon = NA, lat = NA))
  })
  return(result)
}


# Function to calculate co-occurrences of parasitoids for a given pest
calculate_parasitoid_overlap <- function(pest_species, pest_hex, parasitoid_names, parasitoid_hex_list) 
{
  parasitoid_count<-lapply(1:nrow(pest_hex), function(n) {
    # n = 3
    hexline<-pest_hex[n, ]
    ID<-hexline[["hex_id"]]
    parasitoid_count<-sum(sapply(parasitoid_names, function(parasitoid) {
      # parasitoid = "Exorista mella"
      pd = as.data.frame(parasitoid_hex_list[[parasitoid]][["current_hex_grid"]])
      ID %in% pd$hex_id 
    }))
    list(hex_id = ID, parasitoid_count = parasitoid_count)
    
  })
  out_df<-as.data.frame(do.call("rbind", parasitoid_count))
  out_df$pest_species<-pest_species
  out_df
}

# Function to calculate co-occurrences of predators for a given pest
calculate_predator_overlap <- function(pest_species, pest_hex, predator_names, predator_hex_list) 
{
  predator_count<-lapply(1:nrow(pest_hex), function(n) {
    # n = 3
    hexline<-pest_hex[n, ]
    ID<-hexline[["hex_id"]]
    predator_count<-sum(sapply(predator_names, function(predator) {
      # parasitoid = "Exorista mella"
      pd = as.data.frame(predator_hex_list[[predator]][["current_hex_grid"]])
      ID %in% pd$hex_id 
    }))
    list(hex_id = ID, predator_count = predator_count)
    
  })
  out_df<-as.data.frame(do.call("rbind", predator_count))
  out_df$pest_species<-pest_species
  out_df
}

# Example Usage:
# Assume `pest_current_grid` is a named list of pest species and their hex grids
# Assume `parasitoid_current_grid` is a named list of parasitoid species and their hex grids


# Computing Cohen's d-s and the accompanying confidence intervals
compare_groups <- function(mean1, sd1, n1, mean2, sd2, n2, conf_level = 0.95) {

  # R struggles with large integers - we convert the number of occupied cells to numeric
  n1 = as.numeric(n1)
  n2 = as.numeric(n2)
  # Ensure valid inputs
  if (is.na(mean1) | is.na(mean2) | is.na(sd1) | is.na(sd2) | n1 < 2 | n2 < 2) {
    return(list(p_value = NA, cohen_d = NA, ci_lower = NA, ci_upper = NA))
  }
  
  # Pooled standard deviation
  pooled_sd <- sqrt(((n1 - 1) * sd1^2 + (n2 - 1) * sd2^2) / (n1 + n2 - 2))
  
  # Compute t-value
  t_value <- (mean2 - mean1) / (pooled_sd * sqrt(1/n1 + 1/n2))
  
  # Degrees of freedom
  df <- n1 + n2 - 2
  
  # Ensure df is valid
  if (df < 1) {
    return(list(p_value = NA, cohen_d = NA, ci_lower = NA, ci_upper = NA))
  }
  
  # Compute p-value
  p_value <- 2 * pt(-abs(t_value), df)
  
  # Compute Cohen’s d
  cohen_d <- (mean2 - mean1) / pooled_sd
  
  # Standard error for Cohen's d
  se_d <- sqrt((n1 + n2) / (n1 * n2) + (cohen_d^2 / (2 * df)))
  if (is.nan(se_d) | is.infinite(se_d)) se_d <- NA  # Handle zero or infinite values
  
  # Compute confidence interval for Cohen’s d
  t_crit <- ifelse(df > 0, qt(1 - (1 - conf_level) / 2, df), NA)
  
  # Compute lower and upper CI (if valid)
  ci_lower <- ifelse(!is.na(se_d), cohen_d - t_crit * se_d, NA)
  ci_upper <- ifelse(!is.na(se_d), cohen_d + t_crit * se_d, NA)
  
  return(list(
    p_value = p_value,
    cohen_d = cohen_d,
    ci_lower = ci_lower,
    ci_upper = ci_upper
  ))
}






