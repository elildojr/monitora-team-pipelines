
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
# discard non-targets

global_targets <- images %>%
  filter(class %in% c("Mammalia")) %>%
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



# terra do meio
images %>%
  filter(project_id == 2002554) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf)



# jamari
images %>%
  filter(project_id == 2002562) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf)



# juruena
images %>%
  filter(project_id == 2002576) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf)


# maraca
images %>%
  filter(project_id == 2002584) %>%
  group_by(class, order, family, genus, species) %>%
  count() %>%
  arrange(class, order, family, genus, species) %>%
  print(n=Inf)



# save aggregated data
data_fixed_taxonomy <- list(projects=projects,
                                 deployments=deployments,
                                 images=images)

saveRDS(data_fixed_taxonomy, "01_entrada/dados_processados/data_fixed_taxonomy.rds")

