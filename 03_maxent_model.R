
occurrence_sf <- st_transform(occ_points, crs = crs(current_stack))

Sys.time()
maxent_model <- maxent(
  x = current_stack,
  p = st_coordinates(occurrence_sf),
  path = "maxent_output"
)
Sys.time()


future_projection <- predict(maxent_model, future_stack)

predictor_values <- extract(current_stack, st_coordinates(occurrence_sf))

# Identify rows without NA values
valid_points <- complete.cases(predictor_values)

# Filter occurrence data
occurrence_filtered <- occurrence_sf[valid_points, ]

# Check the filtered data
cat("Number of points removed due to NA values: ", sum(!valid_points), "\n")

maxent_model <- maxent(
  x = current_stack,
  p = st_coordinates(occurrence_filtered),
  path = "maxent_output"
)


future_projection <- predict(maxent_model, future_stack)

hex_grid$mean_projection <- extract(future_projection, hex_grid, fun = mean, na.rm = F)
# Plot results
ggplot() +
  geom_sf(data = hex_grid, aes(fill = mean_projection), color = "black") +
  scale_fill_viridis_c() +
  theme_minimal() +
  labs(title = "Future Species Distribution",
       fill = "Mean Projection")