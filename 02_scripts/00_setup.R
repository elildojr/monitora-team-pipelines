# script de setup para carregar automaticamente pacotes, 
# configurar opcoes do projeto, etc

#----- carregar pacotes
#library(here)
library(tidyverse)
library(leaflet)
library(jagsUI)
library(usethis)

#----- carregar funcoes
source("02_scripts/funcoes/filter_independent.R")
source("02_scripts/funcoes/make_map.R")
source("02_scripts/funcoes/calculate_time_lag.R")
source("02_scripts/funcoes/plot_deployment_operation.R")
source("02_scripts/funcoes/make_rn_object.R")
source("02_scripts/funcoes/fit_BernP.R")

