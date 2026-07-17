
# clean environment
rm(list=ls())


#----- setup
source("02_scripts/00_setup.R")


#----- read data
data_aggregated <- readRDS("01_entrada/dados_processados/data_aggregated.rds")
projects <- data_aggregated$projects
deployments <- data_aggregated$deployments
images <- data_aggregated$images


#----- check coordinates

# NB! incorrect coordinates should be checked and fixed in Wildlife Insights
# however, here we will provisionally exclude any sites with incorrect coords


# check individual projects

# gurupi
make_map(2002545)
# remove non-team sites from gurupi project
deployments <- deployments %>%
  filter(! (project_id == 2002545 & str_detect(placename, "CT-RBG-E"))) %>%
  print()

# terra do meio
make_map(2002554)

# jamari
make_map(2002562)
# remove non-team sites from jamari project
jamari_non_team_cams <- readRDS("01_entrada/dados_brutos/jamari-non-team-cams.rds")
deployments <- deployments %>%
  filter(! placename %in% jamari_non_team_cams) %>%
  print()

# juruena
make_map(2002576)
# there is an odd "sem numero" site in this project
# this error must be identified and fixed in wildlife insights
# for now lets remove "sem numero" from juruena project
deployments <- deployments %>%
  filter(placename != "Sem-numero") %>%
  print()

make_map(2002576)

# maraca
make_map(2002584)

# tumucumaque
make_map(2003586)

# tapajos
make_map(2004835)

# caxiuana
make_map(2006780)

# tapirape
make_map(2007402)
# tapirape is currently a mess. 
# in wildlife insights, fix coordinates of:
# CT-RBT-08, CT-RBT-09, CT-RBT-19, CT-RBT-28, CT-RBT-29, CT-RBT-36
# there are a lot of additional sites whcih also seem to be out of place 
# we have to check them all and fix in wildlife insights
# for now, lets just remove the entire project from the dataset
deployments <- deployments %>%
  filter(project_id != 2007402) %>%
  print()

# jari
make_map(2007439)

# rio acre
make_map(2009348)

# descobrimento
make_map(2011116)

# bom jesus
#make_map(2011268)

# monte pascoal
make_map(2012253)

# update projects and images based on corrected deployments
projects <- projects %>%
  filter(project_id %in% deployments$project_id) %>%
  print()

images <- images %>%
  filter(deployment_id %in% deployments$deployment_id) %>%
  print()


# save aggregated data
data_fixed_coords <- list(projects=projects,
                          deployments=deployments,
                          images=images)
saveRDS(data_fixed_coords, "01_entrada/dados_processados/data_fixed_coords.rds")

