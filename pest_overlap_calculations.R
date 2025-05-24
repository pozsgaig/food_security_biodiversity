






##########
##########
##########
##########
##########
##########
##########
##########
# Compute differences between current and future conditions
calculate_difference <- function(current, future) {
  merge(current, future, by = "hex_id", suffixes = c("_current", "_future")) %>%
    mutate(parasitoid_difference = parasitoid_count_future - parasitoid_count_current)
}

# Apply difference calculation for each pest
difference_scenario1 <- lapply(names(current_parasitoid_data), function(pest) {
  calculate_difference(current_parasitoid_data[[pest]], future1_parasitoid_data[[pest]])
})

difference_scenario2 <- lapply(names(current_parasitoid_data), function(pest) {
  calculate_difference(current_parasitoid_data[[pest]], future2_parasitoid_data[[pest]])
})

# Save results
saveRDS(current_parasitoid_data, "../Calculated_data/current_parasitoid_overlap.RDS")
saveRDS(future1_parasitoid_data, "../Calculated_data/future1_parasitoid_overlap.RDS")
saveRDS(future2_parasitoid_data, "../Calculated_data/future2_parasitoid_overlap.RDS")
saveRDS(difference_scenario1, "../Calculated_data/difference_scenario1.RDS")
saveRDS(difference_scenario2, "../Calculated_data/difference_scenario2.RDS")

# Print confirmation
print("Parasitoid co-occurrence data calculated and saved!")


pest_data_list$Aphis_gossypii$pest_hex_grid
