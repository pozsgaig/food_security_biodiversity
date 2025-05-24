# Apply function for each pest under current conditions
current_parasitoid_data <- lapply(pest_list, function(pest) {
  # pest = "Spodoptera_frugiperda"
  cat("\n", pest, "\n")
  calculate_parasitoid_overlap(
    pest_species = pest,
    pest_hex = pest_modelled_current_distribution_list[[pest]][["current_hex_grid"]],
    parasitoid_names = parasitoid_list[[pest]], # Pest-specific parasitoid list
    parasitoid_hex_list = parasitoid_modelled_current_distribution_list
  )
})

# Store results in a named list
names(current_parasitoid_data) <- pest_list

save(current_parasitoid_data, file = "../Calculated_data/current_parasitoid_data.RDA")