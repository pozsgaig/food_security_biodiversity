
pest_parasitoid_changes_all<-lapply(pest_list, function(x){
  current<-current_parasitoid_data_df[[x]]
  current$scenario<-"current"
  
  future1<-future1_parasitoid_data_df[[x]]
  future1$scenario<-"Future 1"
  
  future2<-future2_parasitoid_data_df[[x]]
  future2$scenario<-"Future 2"
  
  dat<-data.frame(hex_id = c(current[["hex_id"]],
                                         future1[["hex_id"]],
                                         future2[["hex_id"]]),
                  pest_species =  x,
                  parasitoid_count = c(current[["parasitoid_count"]],
                                       future1[["parasitoid_count"]],
                                       future2[["parasitoid_count"]]),
                  scenario = c(current[["scenario"]],
                                       future1[["scenario"]],
                                       future2[["scenario"]]))
  
  dat
})

pest_parasitoid_changes_all<-do.call("rbind", pest_parasitoid_changes_all)

colnames(pest_parasitoid_changes_all)

pest_groups<-data.frame(pest_species = pest_list, pest_group = c("many NEs", "few NEs", "many NEs",
                                                         "few NEs", "many NEs","few NEs", 
                                                         "many NEs","few NEs", "few NEs",
                                                         "many NEs","many NEs","many NEs",
                                                         "few NEs","few NEs"))

pest_parasitoid_changes_all<-merge(pest_parasitoid_changes_all, pest_groups,
                                   by = "pest_species", all.x = T)
library(ggpirate)
### 1️⃣ Random Sampling (Keeps Distribution, Faster Rendering)
sample_size <- 5000  # Choose an appropriate sample size
sampled_df <- pest_parasitoid_changes_all %>% sample_n(sample_size)



parasitoid_count_differences<-ggplot(sampled_df, aes(x = scenario, y = log(1+parasitoid_count))) +
  geom_pirate(aes(colour = pest_group), bars = FALSE,
              points_params = list(shape = 19, alpha = 0.2),
              lines_params = list(size = 0.8))+
  facet_wrap(~pest_species, scales = "free_y", ncol = 4, nrow = 4)

ggsave("../Plots/parasitoid_count_differences.pdf", parasitoid_count_differences,
       width= 12, height = 16)


colnames(spp_results)


spp_long <- spp_results %>%
  mutate(
    log_current_mean = log1p(current_parasitoid_mean),   # Log-transform parasitoid pressure
    log_occupied = log1p(current_occupied)    # Log-transform occupied cells for bubble size
  ) %>%
  pivot_longer(
    cols = c(parasitoid_result_future1_future1cohen_d, 
             parasitoid_result_future2_future2cohen_d),
    names_to = "Scenario",
    values_to = "Cohen_d"
  ) %>%
  mutate(
    Scenario = recode(Scenario, 
                      "parasitoid_result_future1_future1cohen_d" = "Moderate emission", 
                      "parasitoid_result_future2_future2cohen_d" = "High emission")
  )


# Create the improved faceted bubble plot
ggplot(spp_long, aes(x = log_current_mean, 
                     y = Cohen_d, 
                     size = log_occupied, 
                     color = species)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~ Scenario) +  # Facet by scenario
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +  # Reference line at y=0
  labs(
    x = "Log of Current Mean Parasitoid Pressure",
    y = "Cohen's d (Effect Size)",
    title = "Effect size as a function of parasitoid pressure, separated by climate change scenario",
    color = "Species",
    size = "Area occupied (log-scale)"  # Renamed legend title
  ) +
  scale_color_manual(values = pest_colors) +  # Use distinct colors
  scale_size(range = c(3, 12)) +  # Adjust bubble sizes for better visibility
  guides(
    color = guide_legend(
      override.aes = list(size = 5),  # Bigger symbols for species
      ncol = 1,  # Forces vertical alignment (one column for species)
      position = "right",  # Keeps "Area occupied (log-scale)" clear
      direction = "vertical"  # Ensures the area legend stays vertical
    ),
    size = guide_legend(
      override.aes = list(color = "#2A9D8F"),  # Colored dots in the legend
      position = "bottom",  # Keeps "Area occupied (log-scale)" clear
      direction = "horizontal"  # Ensures the area legend stays horizontal
    )
  ) +
  theme_cleveland()+
  theme(axis.title.y = element_text())


ggsave("../Plots/parasitoid_bubble_chart_Cohens.pdf", width = 14, height = 10)




spp_long <- spp_results %>%
  mutate(
    log_current_mean = log1p(current_ant_mean),   # Log-transform parasitoid pressure
    log_occupied = log1p(current_occupied)    # Log-transform occupied cells for bubble size
  ) %>%
  pivot_longer(
    cols = c(ant_result_future1_future1cohen_d, 
             ant_result_future2_future2cohen_d),
    names_to = "Scenario",
    values_to = "Cohen_d"
  ) %>%
  mutate(
    Scenario = recode(Scenario, 
                      "ant_result_future1_future1cohen_d" = "Moderate emission", 
                      "ant_result_future2_future2cohen_d" = "High emission")
  )


# Create the improved faceted bubble plot
ggplot(spp_long, aes(x = log_current_mean, 
                     y = Cohen_d, 
                     size = log_occupied, 
                     color = species)) +
  geom_point(alpha = 0.8) +
  facet_wrap(~ Scenario) +  # Facet by scenario
  geom_hline(yintercept = 0, linetype = "dashed", color = "black", linewidth = 1) +  
  geom_smooth(method = "loess", se = F, color = "red", linewidth = 1) +
  labs(
    x = "Log of the mean of current ant pressure",
    y = "Cohen's d (Effect size)",
    title = "Effect size as a function of ant pressure, separated by climate change scenario",
    color = "Species",
    size = "Area occupied (log-scale)"  # Renamed legend title
  ) +
  scale_color_manual(values = pest_colors) +  # Use distinct colors
  scale_size(range = c(3, 12)) +  # Adjust bubble sizes for better visibility
  guides(
    color = guide_legend(
      override.aes = list(size = 5),  # Bigger symbols for species
      ncol = 1,  # Forces vertical alignment (one column for species)
      position = "right",  # Keeps "Area occupied (log-scale)" clear
      direction = "vertical"  # Ensures the area legend stays vertical
    ),
    size = guide_legend(
      override.aes = list(color = "#2A9D8F"),  # Colored dots in the legend
      position = "bottom",  # Keeps "Area occupied (log-scale)" clear
      direction = "horizontal"  # Ensures the area legend stays horizontal
    )
  ) +
  theme_cleveland()+
  theme(axis.title.y = element_text())
