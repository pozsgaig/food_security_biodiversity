current_predator_data <- lapply(pest_list, function(pest) {
  # pest = "Sitobion_avenae"
  # 
  if(!is.null(predator_list[[pest]])){
    cat("\n", pest, "\n")
    calculate_predator_overlap(
      pest_species = pest,
      pest_hex = pest_modelled_current_distribution_list[[pest]][["current_hex_grid"]],
      predator_names = predator_list[[pest]], # Pest-specific parasitoid list
      predator_hex_list = predator_modelled_current_distribution_list
    )
  } else {NULL}
})

names(current_predator_data) <- pest_list
save(current_predator_data, file = "../Calculated_data/current_predator_data.RDA")