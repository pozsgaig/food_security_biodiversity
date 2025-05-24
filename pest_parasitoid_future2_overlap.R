future2_parasitoid_data <- lapply(pest_list, function(pest) {
  cat("\n", pest, "\n")
  calculate_parasitoid_overlap(
    pest_species = pest,
    pest_hex = pest_modelled_future_2_distribution_list[[pest]][["future_2_hex_grid"]],
    parasitoid_names = parasitoid_list[[pest]],
    parasitoid_hex_list = parasitoid_modelled_current_distribution_list
  )
})
names(future2_parasitoid_data) <- pest_list
save(future2_parasitoid_data, file = "../Calculated_data/future2_parasitoid_data.RDA")