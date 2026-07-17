
#----- setup
source("02_scripts/00_setup.R")


#----- read data

# projects
projects <- read_csv("01_entrada/dados_brutos/projects.csv") %>%
  select(project_id, project_short_name) %>%
  print()

# deployments
deployments <- read_csv("01_entrada/dados_brutos/deployments.csv") %>%
  select(project_id, deployment_id, placename,
         longitude, latitude, start_date, end_date,
         camera_functioning, subproject_name, event_name) %>%
  distinct(project_id, deployment_id, placename,
           longitude, latitude, start_date, end_date, .keep_all = TRUE) %>%
  print()

# images
# read and combine all images files at once
images_files <- fs::dir_ls("01_entrada/dados_brutos", regexp = "images")
images_files
images <- read_csv(images_files) %>%
  select(project_id, deployment_id, 
         class, order, family, genus, species,
         timestamp, is_blank) %>%
  filter(is_blank == 0) %>%
  select(-is_blank) %>%
  filter(! is.na(genus)) %>%
  print()

#problems(images)


#----- filter independent events
images <- filter_independent(images, 60) %>%
  print()


# save aggregated data
data_aggregated <- list(projects=projects,
                        deployments=deployments,
                        images=images)

saveRDS(data_aggregated, "01_entrada/dados_processados/data_aggregated.rds")

