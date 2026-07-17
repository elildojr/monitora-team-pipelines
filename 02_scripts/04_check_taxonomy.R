
# clean environment
rm(list=ls())


#----- setup
source("02_scripts/00_setup.R")


#----- read data
data_fixed_dates <- readRDS("01_entrada/dados_processados/data_fixed_dates.rds")
projects <- data_fixed_dates$projects
deployments <- data_fixed_dates$deployments
images <- data_fixed_dates$images
rm(data_fixed_dates)


#----- check taxonomy

# NB! incorrect identifications should be checked and fixed in Wildlife Insights
# however, here we will provisionally fix some stuff here

# NB! lets fix only Mammalia identifications. Lets leave the birds to CEMAVE 

#----- firstly, lets keep only global targets
# discard non-targets as well as congeneric species that
# are difficult to identify
# e.g. Dasypus, Leopardus, Mazama 


global_targets <- images %>%
  filter(class %in% c("Mammalia")) %>%
  #filter(class %in% c("Mammalia", "Aves")) %>%
  filter(family %in% c("Canidae", "Felidae", "Mustelidae", "Procyonidae",
                       "Cervidae",  "Tayassuidae",
                       "Chlamyphoridae", "Dasypodidae",
                       "Didelphidae",
                       "Tapiridae",
                       "Myrmecophagidae",
                       "Cuniculidae", "Dasyproctidae", "Sciuridae")) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf) %>%
  mutate(targets = paste(genus, species, sep = " ")) %>%
  pull(targets) #%>% print()

global_targets

# now filter images files keeping only what is in global_targets
images <- images %>%
  filter(paste(genus, species) %in% global_targets) %>%
  print()



#----- now, check and fix taxonomy of individual projects...

# gurupi
images %>%
  filter(project_id == 2002545) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf)

# use the table above to fix taxonomies in wildlife insights (whenever possible)

target_spp <- images %>%
  filter(project_id == 2002545) %>%
  # filter target species / species with sufficient data
  drop_na(genus, species) %>%
  filter(genus %in% c("Geotrygon", "Mitu", "Odontophorus", "Psophia", 
                      "Panthera", "Eira", "Nasua", "Dicotyles",
                      "Tayassu", "Priodontes", "Didelphis", "Tapirus",
                      "Myrmecophaga", "Tamandua", "Cuniculus", "Dasyprocta")) %>%
  filter(! species %in% c("guttatus", "major")) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf) %>%
  mutate(targets = paste(genus, species, sep = " ")) %>%
  pull(targets) %>%
  print()

# fix taxnomomies
images <- images %>% 
  mutate(species = case_when(project_id == 2002545 & genus == "Geotrygon" ~ "montana",
                             project_id == 2002545 & genus == "Odontophorus" ~ "gujanensis",
                             project_id == 2002545 & genus == "Didelphis" ~ "marsupialis",
                             project_id == 2002545 & genus == "Sciurus" ~ "aestuans",
                             .default = species)) %>%
  print()
  

# now filter images files based on target_spp
images <- images %>%
  filter(
    if_else(
      # condition: target project
      project_id == 2002545,
      # if TRUE: check if genus + species combination exists in target_sp_gurupi
      paste(genus, species) %in% target_spp,
      # if FALSE: keep the row automatically
      TRUE
    )) %>%
  print()



# terra do meio
images %>%
  filter(project_id == 2002554) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf)

target_spp <- images %>%
  filter(project_id == 2002554) %>%
  # filter target species / species with enough data
  drop_na(genus, species) %>%
  filter(genus %in% c("Geotrygon", "Crax", "Mitu", "Odontophorus", 
                      "Psophia", "Tinamus", "Panthera", "Puma",
                      "Eira", "Nasua", "Dicotyles", "Tayassu",
                      "Metachirus", "Tapirus", "Myrmecophaga", "Tamandua",
                      "Cuniculus", "Dasyprocta")) %>%
  filter(! species %in% c("guttatus", "major")) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf) %>%
  mutate(targets = paste(genus, species, sep = " ")) %>%
  pull(targets) %>%
  print()

# fix taxonomies
images <- images %>%
  mutate(species = case_when(project_id == 2002554 & genus == "Geotrygon" ~ "montana",
                             project_id == 2002554 & genus == "Crax" ~ "fasciolata",
                             project_id == 2002554 & genus == "Mitu" ~ "tuberosum",
                             project_id == 2002554 & genus == "Odontophorus" ~ "gujanensis",
                             project_id == 2002554 & genus == "Nasua" ~ "nasua",
                             project_id == 2002554 & genus == "Tayassu" ~ "pecari",
                             project_id == 2002554 & genus == "Metachirus" ~ "nudicaudatus",
                             project_id == 2002554 & genus == "Tapirus" ~ "terrestris",
                             project_id == 2002554 & genus == "Cuniculus" ~ "paca",
                             project_id == 2002554 & genus == "Dasyprocta" ~ "iacki",
                             project_id == 2002554 & species == "viridis" ~ "dextralis",
                             .default = species)) %>%
  print()

# now filter images files based on target_spp
images <- images %>%
  filter(
    if_else(
      # condition: target project
      project_id == 2002554,
      # if TRUE: check if genus + species combination exists in target_sp_gurupi
      paste(genus, species) %in% target_spp,
      # if FALSE: keep the row automatically
      TRUE
    )) %>%
  print()



# jamari
images %>%
  filter(project_id == 2002562) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf)

target_spp <- images %>%
  filter(project_id == 2002562) %>%
  # filter target species / species with enough data
  drop_na(genus, species) %>%
  filter(genus %in% c("Geotrygon", "Mitu", "Eira", "Nasua", 
                      "Dicotyles", "Tayassu", "Priodontes", "Didelphis",
                      "Tapirus", "Myrmecophaga", "Cuniculus", "Dasyprocta")) %>%
  filter(! species %in% c("guttatus", "major")) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf) %>%
  mutate(targets = paste(genus, species, sep = " ")) %>%
  pull(targets) %>%
  print()

# fix taxonomies
images <- images %>%
  mutate(species = case_when(project_id == 2002562 & genus == "Mitu" ~ "tuberosum",
                             project_id == 2002562 & genus == "Psophia" ~ "viridis",
                             project_id == 2002562 & genus == "Didelphis" ~ "marsupialis",
                             project_id == 2002562 & genus == "Cuniculus" ~ "paca",
                             project_id == 2002562 & genus == "Dasyprocta" ~ "fuliginosa",
                             project_id == 2002562 & species == "crepitans" ~ "viridis",
                             project_id == 2002562 & species == "leporina" ~ "fuliginosa",
                             project_id == 2002562 & species == "punctata" ~ "fuliginosa",
                             .default = species)) %>%
  print()


# now filter images files based on target_spp
images <- images %>%
  filter(
    if_else(
      # condition: target project
      project_id == 2002562,
      # if TRUE: check if genus + species combination exists in target_sp_gurupi
      paste(genus, species) %in% target_spp,
      # if FALSE: keep the row automatically
      TRUE
    )) %>%
  print()



# juruena
images %>%
  filter(project_id == 2002576) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf)

target_spp <- images %>%
  filter(project_id == 2002576) %>%
  # filter target species / species with enough data
  drop_na(genus, species) %>%
  filter(genus %in% c("Geotrygon", "Crax", "Mitu", "Psophia", 
                      "Panthera", "Puma", "Eira", "Nasua",
                      "Dicotyles", "Tayassu", "Priodontes", "Didelphis",
                      "Metachirus", "Tapirus", "Myrmecophaga", "Tamandua",
                      "Cuniculus", "Dasyprocta")) %>%
  filter(! species %in% c("guttatus", "major")) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf) %>%
  mutate(targets = paste(genus, species, sep = " ")) %>%
  pull(targets) %>%
  print()


# now filter images files based on target_spp
images <- images %>%
  filter(
    if_else(
      # condition: target project
      project_id == 2002576,
      # if TRUE: check if genus + species combination exists in target_sp_gurupi
      paste(genus, species) %in% target_spp,
      # if FALSE: keep the row automatically
      TRUE
    )) %>%
  print()


# maraca
images %>%
  filter(project_id == 2002584) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf)

target_spp <- images %>%
  filter(project_id == 2002584) %>%
  # filter target species / species with enough data
  drop_na(genus, species) %>%
  filter(genus %in% c("Crax", "Psophia", "Puma", "Eira", 
                      "Dicotyles", "Tayassu", "Tapirus", "Myrmecophaga",
                      "Cuniculus", "Dasyprocta")) %>%
  filter(! species %in% c("guttatus", "major")) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf) %>%
  mutate(targets = paste(genus, species, sep = " ")) %>%
  pull(targets) %>%
  print()


# fix taxonomies
images <- images %>%
  mutate(species = case_when(project_id == 2002584 & genus == "Dasyprocta" ~ "leporina",
                             project_id == 2002584 & genus == "Psophia" ~ "crepitans",
                             project_id == 2002584 & genus == "Crax" ~ "alector",
                             project_id == 2002584 & genus == "Tayassu" ~ "pecari",
                             project_id == 2002584 & genus == "Tapirus" ~ "terrestris",
                             .default = species)) %>%
  print()


# now filter images files based on target_spp
images <- images %>%
  filter(
    if_else(
      # condition: target project
      project_id == 2002584,
      # if TRUE: check if genus + species combination exists in target_sp_gurupi
      paste(genus, species) %in% target_spp,
      # if FALSE: keep the row automatically
      TRUE
    )) %>%
  print()



# save aggregated data
data_fixed_taxonomy <- list(projects=projects,
                                 deployments=deployments,
                                 images=images)

saveRDS(data_fixed_taxonomy, "01_entrada/dados_processados/data_fixed_taxonomy.rds")

