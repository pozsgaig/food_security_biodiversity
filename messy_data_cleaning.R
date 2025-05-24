source("google_API.R")

Phenacoccus_manihoti_distribution<-
  read.xlsx("../Raw_data/Data_before_cleaning/1 BC species/Phenacoccus_manihoti_distribution.xlsx", 
                                                  sheet = 1)

Phenacoccus_manihoti_parasitoid_loc<-
  read.xlsx("../Raw_data/Data_before_cleaning/1 BC species/Phenacoccus_manihoti_parasitoid.xlsx", 
            sheet = 1)

Phenacoccus_manihoti_parasitoid_DMS<-
  read.xlsx("../Raw_data/Data_before_cleaning/1 BC species/Phenacoccus_manihoti_parasitoid.xlsx", 
            sheet = 2)

Phenacoccus_manihoti_parasitoid_DD<-
  read.xlsx("../Raw_data/Data_before_cleaning/1 BC species/Phenacoccus_manihoti_parasitoid.xlsx", 
            sheet = 3)

Mononychellus_tanajoa_distribution<-
  read.xlsx("../Raw_data/Data_before_cleaning/1 BC species/Mononychellus_tanajoa_distribution.xlsx")

Mononychellus_tanajoa_parasitoid<-
  read.xlsx("../Raw_data/Data_before_cleaning/1 BC species/Mononychellus_tanajoa_parasitoid.xlsx", 
            sheet = 1)



Phenacoccus_manihoti_distribution$species <- "Phenacoccus manihoti"
Phenacoccus_manihoti_distribution<-Phenacoccus_manihoti_distribution[, c(3,1,2)]
colnames(Phenacoccus_manihoti_distribution)[2:3]<-c("decimalLatitude", "decimalLongitude")

# Phenacoccus manihoti - need to add in all locations of the parasitoid Anagyrus lopezi

Phenacoccus_manihoti_parasitoid_loc<-unique(Phenacoccus_manihoti_parasitoid_loc)
Phenacoccus_manihoti_parasitoid_loc<-as.data.frame(apply(Phenacoccus_manihoti_parasitoid_loc,
                                           2, trimws))

Phenacoccus_manihoti_parasitoid_loc<-Phenacoccus_manihoti_parasitoid_loc |> 
  mutate(LocationQuery = pmap_chr(list(Location, Area, Country), 
                                  ~ str_c(na.omit(c(...)), collapse = ", "))) |> # str_c is used instead of paste to avoid pasting NAs
  rowwise() %>%
  mutate(Geocode = list(geocode_location(LocationQuery))) %>%
  unnest_wider(Geocode, names_sep = "_") %>%
  rename(decimalLongitude = Geocode_lon, decimalLatitude = Geocode_lat) |> 
  select(species, decimalLatitude, decimalLongitude)


Phenacoccus_manihoti_parasitoid_long<-sapply(Phenacoccus_manihoti_parasitoid_DMS$DMS_long, 
                                  function(x){cat(x, "\n")
                                    convert_to_fractional_degrees(x)})

Phenacoccus_manihoti_parasitoid_lat<-sapply(Phenacoccus_manihoti_parasitoid_DMS$DMS_lat, 
                                 convert_to_fractional_degrees)

colnames(Phenacoccus_manihoti_parasitoid_DD)<-c("species", "decimalLatitude", "decimalLongitude")

Phenacoccus_manihoti_parasitoid<-rbind(Phenacoccus_manihoti_parasitoid_loc,
                                       Phenacoccus_manihoti_parasitoid_DD,
  data.frame(species = "Anagyrus lopezi",
             decimalLatitude = Phenacoccus_manihoti_parasitoid_lat,
             decimalLongitude = Phenacoccus_manihoti_parasitoid_long))


Phenacoccus_manihoti_distribution<-rbind(Phenacoccus_manihoti_distribution,
                                         data.frame(species = "Phenacoccus manihoti",
                                                    decimalLatitude = 
                                                      Phenacoccus_manihoti_parasitoid$decimalLatitude,
                                                    decimalLongitude = 
                                                      Phenacoccus_manihoti_parasitoid$decimalLongitude))

Mononychellus_tanajoa_distribution<-unique(Mononychellus_tanajoa_distribution)

Mononychellus_tanajoa_distribution <- Mononychellus_tanajoa_distribution |> 
  mutate(LocationQuery = pmap_chr(list(Location, Area, Country), 
                                  ~ str_c(na.omit(c(...)), collapse = ", "))) |> # str_c is used instead of paste to avoid pasting NAs
  rowwise() %>%
  mutate(Geocode = list(geocode_location(LocationQuery))) %>%
  unnest_wider(Geocode, names_sep = "_") %>%
  rename(decimalLongitude = Geocode_lon, decimalLatitude = Geocode_lat)

# correcting two mistakes manually
Mononychellus_tanajoa_distribution[grep("usa",Mononychellus_tanajoa_distribution$Geocode_address), 
                                   "decimalLongitude"] = 7.78
Mononychellus_tanajoa_distribution[grep("usa",Mononychellus_tanajoa_distribution$Geocode_address), 
                                   "decimalLatitude"] = 2.20

Mononychellus_tanajoa_distribution[grep("portugal",Mononychellus_tanajoa_distribution$Geocode_address), 
                                   "decimalLongitude"] = -7.02
Mononychellus_tanajoa_distribution[grep("portugal",Mononychellus_tanajoa_distribution$Geocode_address), 
                                   "decimalLatitude"] = -42.13


Mononychellus_tanajoa_distribution<-Mononychellus_tanajoa_distribution|> 
  select(species, decimalLatitude, decimalLongitude)


Mononychellus_tanajoa_parasitoid<-unique(Mononychellus_tanajoa_parasitoid)

Mononychellus_tanajoa_parasitoid<-Mononychellus_tanajoa_parasitoid |> 
  mutate(LocationQuery = pmap_chr(list(Location, Area, Country), 
                                  ~ str_c(na.omit(c(...)), collapse = ", "))) |> # str_c is used instead of paste to avoid pasting NAs
  rowwise() %>%
  mutate(Geocode = list(geocode_location(LocationQuery))) %>%
  unnest_wider(Geocode, names_sep = "_") %>%
  rename(decimalLongitude = Geocode_lon, decimalLatitude = Geocode_lat) |> 
  select(species, decimalLatitude, decimalLongitude)

Mononychellus_tanajoa_distribution<-rbind(Mononychellus_tanajoa_distribution,
                                         data.frame(species = "Mononychellus tanajoa",
                                                    decimalLatitude = 
                                                      Mononychellus_tanajoa_parasitoid$decimalLatitude,
                                                    decimalLongitude = 
                                                      Mononychellus_tanajoa_parasitoid$decimalLongitude))


Mononychellus_tanajoa_occ_points<-Mononychellus_tanajoa_distribution |> 
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude),
  # Remove records with missing coordinates
  decimalLatitude >= -90 & decimalLatitude <= 90, # Latitude range
  decimalLongitude >= -180 & decimalLongitude <= 180, # Longitude range!(decimalLatitude == 0 &
  !(decimalLatitude == 0 & decimalLongitude == 0) )

Mononychellus_tanajoa_occ_points<-st_as_sf(Mononychellus_tanajoa_occ_points,
  coords = c("decimalLongitude", "decimalLatitude"),
  crs = 4326, remove = FALSE) |> 
  st_transform(crs = st_crs(hex_grid))

Mononychellus_tanajoa_hex_grid<-observations2grid(hex_grid, Mononychellus_tanajoa_occ_points)
Mononychellus_tanajoa_large_hex_grid<-observations2grid(large_hex_grid, Mononychellus_tanajoa_occ_points)


Mononychellus_tanajoa_parasitoids_occ_points<-Mononychellus_tanajoa_parasitoid |> 
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude),
         # Remove records with missing coordinates
         decimalLatitude >= -90 & decimalLatitude <= 90, # Latitude range
         decimalLongitude >= -180 & decimalLongitude <= 180, # Longitude range!(decimalLatitude == 0 &
         !(decimalLatitude == 0 & decimalLongitude == 0) )

Mononychellus_tanajoa_parasitoids_occ_points<-
  st_as_sf(Mononychellus_tanajoa_parasitoids_occ_points,
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326, remove = FALSE) |> 
  st_transform(crs = st_crs(hex_grid))

Mononychellus_tanajoa_parasitoids_hex_grid<-observations2grid(hex_grid, Mononychellus_tanajoa_parasitoids_occ_points)
Mononychellus_tanajoa_parasitoids_large_hex_grid<-observations2grid(large_hex_grid, Mononychellus_tanajoa_parasitoids_occ_points)




Phenacoccus_manihoti_occ_points<-Phenacoccus_manihoti_distribution |> 
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude),
         # Remove records with missing coordinates
         decimalLatitude >= -90 & decimalLatitude <= 90, # Latitude range
         decimalLongitude >= -180 & decimalLongitude <= 180, # Longitude range!(decimalLatitude == 0 &
         !(decimalLatitude == 0 & decimalLongitude == 0) )

Phenacoccus_manihoti_occ_points<-
  st_as_sf(Phenacoccus_manihoti_occ_points,
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326, remove = FALSE) |> 
  st_transform(crs = st_crs(hex_grid))

Phenacoccus_manihoti_hex_grid<-observations2grid(hex_grid, Phenacoccus_manihoti_occ_points)
Phenacoccus_manihoti_large_hex_grid<-observations2grid(large_hex_grid, Phenacoccus_manihoti_occ_points)


Phenacoccus_manihoti_parasitoids_occ_points<-Phenacoccus_manihoti_parasitoid |> 
  filter(!is.na(decimalLatitude) & !is.na(decimalLongitude),
         # Remove records with missing coordinates
         decimalLatitude >= -90 & decimalLatitude <= 90, # Latitude range
         decimalLongitude >= -180 & decimalLongitude <= 180, # Longitude range!(decimalLatitude == 0 &
         !(decimalLatitude == 0 & decimalLongitude == 0) )

Phenacoccus_manihoti_parasitoids_occ_points<-
  st_as_sf(Phenacoccus_manihoti_parasitoids_occ_points,
           coords = c("decimalLongitude", "decimalLatitude"),
           crs = 4326, remove = FALSE) |> 
  st_transform(crs = st_crs(hex_grid))

Phenacoccus_manihoti_parasitoids_hex_grid<-observations2grid(hex_grid, Phenacoccus_manihoti_parasitoids_occ_points)
Phenacoccus_manihoti_parasitoids_large_hex_grid<-observations2grid(large_hex_grid, Phenacoccus_manihoti_parasitoids_occ_points)

save(Mononychellus_tanajoa_occ_points,
     Mononychellus_tanajoa_parasitoids_occ_points,
     Mononychellus_tanajoa_hex_grid,
     Mononychellus_tanajoa_large_hex_grid,
     Mononychellus_tanajoa_parasitoids_hex_grid,
     Mononychellus_tanajoa_parasitoids_large_hex_grid,
     Phenacoccus_manihoti_occ_points,
     Phenacoccus_manihoti_parasitoids_occ_points,
     Phenacoccus_manihoti_hex_grid,
     Phenacoccus_manihoti_large_hex_grid,
     Phenacoccus_manihoti_parasitoids_hex_grid,
     Phenacoccus_manihoti_parasitoids_large_hex_grid,
     file = "../Raw_data/cleaned_messy_data.RDA")
