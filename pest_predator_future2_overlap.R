future2_predator_data <- lapply(pest_list, function(pest) {
  # pest = "Sitobion_avenae"
  # 
  if(!is.null(predator_list[[pest]])){
    cat("\n", pest, "\n")
    calculate_predator_overlap(
      pest_species = pest,
      pest_hex = pest_modelled_future_2_distribution_list[[pest]][["future_2_hex_grid"]],
      predator_names = predator_list[[pest]], # Pest-specific parasitoid list
      predator_hex_list = predator_modelled_current_distribution_list
    )
  } else {NULL}
})

names(future2_predator_data) <- pest_list
save(future2_predator_data, file = "../Calculated_data/future2_predator_data.RDA")