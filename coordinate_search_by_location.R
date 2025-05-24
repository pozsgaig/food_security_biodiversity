
source("google_API.R")

# Load the dataset
file_path <- "../Raw_data/1 BC species/unknown_locations.csv" # Replace with the correct path
data <- read_csv(file_path)


# Function to geocode a location
geocode_location <- function(location) {
  result <- tryCatch({
    geocode(location, output = "latlona", source = "google")
  }, error = function(e) {
    return(data.frame(lon = NA, lat = NA))
  })
  return(result)
}

# Create a location query column
data <- data %>%
  mutate(LocationQuery = paste(Location, Area, Country, sep = ", "))

# Apply the geocoding function to each row
geocoded_data <- data %>%
  rowwise() %>%
  mutate(Geocode = list(geocode_location(LocationQuery))) %>%
  unnest_wider(Geocode, names_sep = "_") %>%
  rename(lon = Geocode_lon, lat = Geocode_lat) |> 
  select(Location, Area, Country, LocationQuery, lat, lon, Geocode_address)



# Save the updated dataset
output_path <- "../Raw_data/1 BC species/updated_dataset.csv" # Replace with the desired output path
write_csv(geocoded_data, output_path)
write.xlsx(geocoded_data, "../Raw_data/1 BC species/updated_dataset.xlsx")

