pest_modelled_future_1_distribution_list<-lapply(pest_list, function(x){
  # x = "Aphis_gossypii"
  cat("\n", x, "\n")
  
  future_1_distribution<-make_distribution(future_stack_moderate, 
                                           pest_data_list[[x]][["pest_occ_points"]])
  
  future_1_hex_grid<-hex_grid
  future_1_hex_grid$mean_suitability <- exact_extract(future_1_distribution, 
                                                      future_1_hex_grid, 'mean')
  future_1_hex_grid$ant_diversity <- exact_extract(ant_diversity_raster, 
                                                   hex_grid, 'mean')
  
  future_1_hex_grid<-future_1_hex_grid |> 
    filter(!is.na(mean_suitability)) |> 
    mutate(occupancy = ifelse(mean_suitability>0.5, 1, 0)) |> 
    filter(occupancy>0) |> 
    sf::st_drop_geometry() |> 
    as.data.frame()
  
  
  list(future_1_distribution = future_1_distribution,
       future_1_hex_grid = future_1_hex_grid,
       modelled_future_1_hex_count = 
         nrow(future_1_hex_grid[future_1_hex_grid$mean_suitability>0.5,])
  )
  
})

names(pest_modelled_future_1_distribution_list)<-pest_list

save(pest_modelled_future_1_distribution_list,
     file = "../Calculated_data/pest_modelled_future_1_distribution_list_equal_area.RDA")
