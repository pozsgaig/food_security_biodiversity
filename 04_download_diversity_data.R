# Load required libraries

library(terra)    # For raster handling
library(ggplot2)  # For visualization
library(raster)   # Additional raster functions

# ------------------------
# STEP 1: Load the Raster
# ------------------------


# https://biodiversitymapping.org/index.php/permissions/

# Define the file path (update if necessary)
file_path <- "../GIS/Richness_10km_Birds_v7_EckertIV_nonbreeding_no_seabirds.tif"

# Load the raster
bird_richness_raster <- rast(file_path)

# Plot original raster (10 km resolution)
plot(bird_richness_raster, main="Original Bird Species Richness (10km resolution)")

# -------------------------------
# STEP 2: Aggregate to 20 km Grid
# -------------------------------

# Aggregate by a factor of 2 (20km = 2x2 of 10km cells), using mean richness per cell
bird_richness_20km <- aggregate(bird_richness_raster, fact=2, fun=mean, na.rm=TRUE)

# Plot aggregated raster (20 km resolution)
plot(bird_richness_20km, main="Bird Species Richness (20km resolution)")

# ---------------------------------------------------
# STEP 3: Convert Raster to Dataframe for Analysis
# ---------------------------------------------------

# Convert to dataframe
bird_df <- as.data.frame(bird_richness_20km, xy=TRUE)

# Rename the richness column
colnames(bird_df) <- c("Longitude", "Latitude", "Richness")

# Display the first few rows
head(bird_df)

# -----------------------------------
# STEP 4: Save the New Raster (20km)
# -----------------------------------

# # Define output file path
# output_file <- "Bird_Richness_20km.tif"
# 
# # Save the new raster
# writeRaster(bird_richness_20km, output_file, overwrite=TRUE)
# 
# print(paste("New raster saved as:", output_file))

# -----------------------------------------
# STEP 5: Visualize with ggplot2 Heatmap
# -----------------------------------------

ggplot(bird_df, aes(x=Longitude, y=Latitude, fill=Richness)) +
  geom_raster() +
  scale_fill_viridis_c(name="Richness") +
  labs(title="Bird Species Richness (20km Grid)", x="Longitude", y="Latitude") +
  theme_minimal()




##### ANts
#####  https://datadryad.org/stash/dataset/doi:10.5061/dryad.wstqjq2pp#citations
#####  


# Define the file path (update if necessary)
file_path <- "../GIS/ant_sp_richness_poly.tif"

# Load the raster
ant_richness_raster <- rast(file_path)

# Plot original raster (10 km resolution)
plot(ant_richness_raster, main="Original Ant Species Richness (10km resolution)")


