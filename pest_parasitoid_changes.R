load("../Calculated_data/pest_data_list_equal_area.RDA")
load("../Calculated_data/pest_modelled_current_distribution_list_equal_area.RDA") 
load("../Calculated_data/parasitoid_modelled_current_distribution_list_equal_area.RDA")
load("../Calculated_data/pest_modelled_future_1_distribution_list_equal_area.RDA")
load("../Calculated_data/pest_modelled_future_2_distribution_list_equal_area.RDA")

load("../Calculated_data/current_parasitoid_data.RDA")
load("../Calculated_data/future1_parasitoid_data.RDA")
load("../Calculated_data/future2_parasitoid_data.RDA")


sapply(pest_list, 
       function(x) {
         orig_count<-current_NE_data_df[[x]][["parasitoid_count"]]
         new_count<-future1_NE_data_df[[x]][["parasitoid_count"]]
         length(new_count[new_count>0])/
           length(orig_count[orig_count>0])})









colnames(spp_results)

t.test(parasitoid_result_future1_future1cohen_d ~ pest_group, data = spp_results, var.equal = TRUE)
t.test(parasitoid_result_future2_future2cohen_d ~ pest_group, data = spp_results, var.equal = TRUE)


t.test(ant_result_future1_future1cohen_d ~ pest_group, data = spp_results, var.equal = TRUE)
t.test(ant_result_future2_future2cohen_d ~ pest_group, data = spp_results, var.equal = TRUE)





# Parasitoid changes plotting
# pest = "Aphis_gossypii"

ant_div<-as.data.frame(t(sapply(pest_NE_changes_1, function(x) 
  {c(min = min(x[["ant_diversity_current"]], na.rm = T),
     mean = mean(x[["ant_diversity_current"]], na.rm = T),
     max = max(x[["ant_diversity_current"]], na.rm = T))})))

mean(ant_div$mean)
max(ant_div$max)

pred_div<-as.data.frame(t(sapply(pest_NE_changes_1, function(x) 
{c(min = min(x[["predator_count_current"]], na.rm = T),
   mean = mean(x[["predator_count_current"]], na.rm = T),
   max = max(x[["predator_count_current"]], na.rm = T))})))

mean(pred_div$mean)
max(pred_div$max)

para_div<-as.data.frame(t(sapply(pest_NE_changes_1, function(x) 
{c(min = min(x[["parasitoid_count_current"]], na.rm = T),
   mean = mean(x[["parasitoid_count_current"]], na.rm = T),
   max = max(x[["parasitoid_count_current"]], na.rm = T))})))

mean(para_div$mean)
max(para_div$max)

parasitoid_change_1_plotlist<-lapply(pest_list, function(pest){
  
  dat<-pest_NE_changes_1[[pest]]
  dat<-dat |> 
    left_join(hex_grid, by = "hex_id") |> 
    filter(!is.na(parasitoid_count_current) | !is.na(parasitoid_count_F1)) |> 
    mutate(parasitoid_count_scaled = rescale(parasitoid_count, to = c(0,1))) |> 
    st_as_sf() 
 
  bbox<-st_bbox(dat) 
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])
  
  p<-ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="recede"), aes(fill = parasitoid_count), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="spread"), aes(fill = parasitoid_count), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="stable"), aes(fill = parasitoid_count), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 1 climate scenarios"
    )+
    theme(legend.position = "bottom")

  ggsave(paste("../Plots/parasitoid_change", pest, "_F1_plots.png"), p, width = 10, height = 6)
  ggsave(paste("../Plots/parasitoid_change", pest, "_F1_plots.pdf"), p, width = 10, height = 6)
 
  
  # scaled NE pressure for common legend
  ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="recede"), aes(fill = parasitoid_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="spread"), aes(fill = parasitoid_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="stable"), aes(fill = parasitoid_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 1 climate scenarios"
    )+
    theme(legend.position = "right")
  
})


plotpages<-ggarrange(plotlist = parasitoid_change_1_plotlist, ncol = 4, nrow = 4,
                     common.legend = T, legend = "right")
ggsave("../Plots/parasitoid_change_F1_plots.pdf", plotpages, width = 18, height = 12)


parasitoid_change_2_plotlist<-lapply(pest_list, function(pest){
  
  dat<-pest_NE_changes_2[[pest]]
  dat<-dat |> 
    left_join(hex_grid, by = "hex_id") |> 
    filter(!is.na(parasitoid_count_current) | !is.na(parasitoid_count_F2)) |> 
    mutate(parasitoid_count_scaled = rescale(parasitoid_count, to = c(0,1))) |> 
    st_as_sf() 
  
  bbox<-st_bbox(dat) 
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])
  
  p<-ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="recede"), aes(fill = parasitoid_count), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="spread"), aes(fill = parasitoid_count), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="stable"), aes(fill = parasitoid_count), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 2 climate scenarios"
    )+
    theme(legend.position = "bottom")
  
  ggsave(paste("../Plots/parasitoid_change", pest, "_F2_plots.png"), p, width = 10, height = 6)
  ggsave(paste("../Plots/parasitoid_change", pest, "_F2_plots.pdf"), p, width = 10, height = 6)
  
  
  # scaled NE pressure for common legend
  ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="recede"), aes(fill = parasitoid_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="spread"), aes(fill = parasitoid_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="stable"), aes(fill = parasitoid_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 2 climate scenarios"
    )+
    theme(legend.position = "right")
})


plotpages<-ggarrange(plotlist = parasitoid_change_2_plotlist, ncol = 4, nrow = 4,
                     common.legend = T, legend = "right")
ggsave("../Plots/parasitoid_change_F2_plots.pdf", plotpages, width = 18, height = 12)


# Predator changes plotting

predator_change_1_plotlist<-lapply(pest_list, function(pest){
  #pest = "Bactrocera_dorsalis"
  if(!is.null(current_predator_data[[pest]])){
  dat<-pest_NE_changes_1[[pest]]
  dat<-dat |> 
    left_join(hex_grid, by = "hex_id") |> 
    filter(!is.na(predator_count_current) | !is.na(predator_count_F1)) |> 
    mutate(predator_count_scaled = rescale(predator_count, to = c(0,1))) |> 
    st_as_sf() 
  
  bbox<-st_bbox(dat) 
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])
  
  p<-ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="recede"), aes(fill = predator_count), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="spread"), aes(fill = predator_count), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="stable"), aes(fill = predator_count), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 1 climate scenarios"
    )+
    theme(legend.position = "bottom")
  
  ggsave(paste("../Plots/predator_change", pest, "_F1_plots.png"), p, width = 10, height = 6)
  ggsave(paste("../Plots/predator_change", pest, "_F1_plots.pdf"), p, width = 10, height = 6)
  
  ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="recede"), aes(fill = predator_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="spread"), aes(fill = predator_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="stable"), aes(fill = predator_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 1 climate scenarios"
    )+
    theme(legend.position = "bottom")
  }
  
})

predator_change_1_plotlist<-predator_change_1_plotlist[
  sapply(predator_change_1_plotlist, function(x) !is.null(x))]

plotpages<-ggarrange(plotlist = predator_change_1_plotlist, ncol = 1, nrow = 3,
                     common.legend = T, legend = "right")
ggsave("../Plots/predator_change_F1_plots.pdf", plotpages, width = 10, height = 10)


predator_change_2_plotlist<-lapply(pest_list, function(pest){
  
  if(!is.null(current_predator_data[[pest]])){
  dat<-pest_NE_changes_2[[pest]]
  dat<-dat |> 
    left_join(hex_grid, by = "hex_id") |> 
    filter(!is.na(predator_count_current) | !is.na(predator_count_F2)) |> 
    mutate(predator_count_scaled = rescale(predator_count, to = c(0,1))) |> 
    st_as_sf() 
  
  bbox<-st_bbox(dat) 
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])
  
  p<-ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="recede"), aes(fill = predator_count), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="spread"), aes(fill = predator_count), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="stable"), aes(fill = predator_count), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 2 climate scenarios"
    )+
    theme(legend.position = "bottom")
  
  ggsave(paste("../Plots/predator_change", pest, "_F2_plots.png"), p, width = 10, height = 6)
  ggsave(paste("../Plots/predator_change", pest, "_F2_plots.pdf"), p, width = 10, height = 6)
  
  ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="recede"), aes(fill = predator_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="spread"), aes(fill = predator_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="stable"), aes(fill = predator_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 2 climate scenarios"
    )+
    theme(legend.position = "bottom")
  }
})

predator_change_2_plotlist<-predator_change_2_plotlist[
  sapply(predator_change_2_plotlist, function(x) !is.null(x))]

plotpages<-ggarrange(plotlist = predator_change_2_plotlist, ncol = 1, nrow = 3,
                     common.legend = T, legend = "right")
ggsave("../Plots/predator_change_F2_plots.pdf", plotpages, width = 10, height = 10)


ant_change_1_plotlist<-lapply(pest_list, function(pest){
  
  dat<-pest_NE_changes_1[[pest]]
  dat<-dat |> 
    left_join(hex_grid, by = "hex_id") |> 
    filter(!is.na(ant_diversity_current) | !is.na(ant_diversity_F1)) |> 
    mutate(ant_count_scaled = rescale(ant_count, to = c(0,1))) |> 
    st_as_sf() 
  
  bbox<-st_bbox(dat) 
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])
  
  p<-ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="spread"), aes(fill = ant_count), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="recede"), aes(fill = ant_count), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="stable"), aes(fill = ant_count), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 1 climate scenarios"
    )+
    theme(legend.position = "bottom")
  
  ggsave(paste("../Plots/ant_change", pest, "_F1_plots.png"), p, width = 10, height = 6)
  ggsave(paste("../Plots/ant_change", pest, "_F1_plots.pdf"), p, width = 10, height = 6)
  
  ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="spread"), aes(fill = ant_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="recede"), aes(fill = ant_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F1=="stable"), aes(fill = ant_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 1 climate scenarios"
    )+
    theme(legend.position = "bottom")
  
})


plotpages<-ggarrange(plotlist = ant_change_1_plotlist, ncol = 4, nrow = 4,
                     common.legend = T, legend = "right")
ggsave("../Plots/ant_change_F1_plots.pdf", plotpages, width = 18, height = 12)


ant_change_2_plotlist<-lapply(pest_list, function(pest){
  
  dat<-pest_NE_changes_2[[pest]]
  dat<-dat |> 
    left_join(hex_grid, by = "hex_id") |> 
    filter(!is.na(ant_diversity_current) | !is.na(ant_diversity_F2)) |> 
    mutate(ant_count_scaled = rescale(ant_count, to = c(0,1))) |> 
    st_as_sf() 
  
  bbox<-st_bbox(dat) 
  xlim <- c(bbox["xmin"], bbox["xmax"])
  ylim <- c(bbox["ymin"], bbox["ymax"])
  
  p<-ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="recede"), aes(fill = ant_count), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="spread"), aes(fill = ant_count), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="stable"), aes(fill = ant_count), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 2 climate scenarios"
    )+
    theme(legend.position = "bottom")
  
  ggsave(paste("../Plots/ant_change", pest, "_F2_plots.png"), p, width = 10, height = 6)
  ggsave(paste("../Plots/ant_change", pest, "_F2_plots.pdf"), p, width = 10, height = 6)
  
  ggplot() +
    geom_sf(data = world_map, linewidth = 0.1, fill = NA) +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="recede"), aes(fill = ant_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_recede+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="spread"), aes(fill = ant_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_spread+
    new_scale_fill() +
    
    geom_sf(data = dat |> filter(pest_change_C_F2=="stable"), aes(fill = ant_count_scaled), 
            linewidth = 0.001, color = NA) +
    palette_stable+
    new_scale_fill() +
    coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
    theme_minimal() +
    labs(
      title = gsub("_", " ", pest, fixed = T),
      fill = "Changes in pest distribution between current and Future 2 climate scenarios"
    )+
    theme(legend.position = "bottom")
  
})


plotpages<-ggarrange(plotlist = ant_change_2_plotlist, ncol = 4, nrow = 4,
                     common.legend = T, legend = "right")
ggsave("../Plots/ant_change_F2_plots.pdf", plotpages, width = 18, height = 12)

