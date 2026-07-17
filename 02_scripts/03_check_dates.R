
# clean environment
rm(list=ls())


#----- setup
source("02_scripts/00_setup.R")


#----- read data
data_fixed_coords <- readRDS("01_entrada/dados_processados/data_fixed_coords.rds")

projects <- data_fixed_coords$projects %>%
  print()

deployments <- data_fixed_coords$deployments %>%
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(end_date)) %>%
  drop_na(start_date, end_date) %>%
  select(-c(camera_functioning, subproject_name, event_name)) %>%
  print()

images <- data_fixed_coords$images %>%
  drop_na(timestamp) %>%
  # add start_date and end_date to images
  left_join(deployments %>%
              distinct(deployment_id, start_date, end_date),
            by = "deployment_id") %>%
  mutate(start_date = as.Date(start_date),
         end_date = as.Date(end_date),
         photo_date = as.Date(timestamp)) %>%
  drop_na(start_date, end_date, photo_date) %>%
  print()

rm(data_fixed_coords)



#----- check dates

# NB! incorrect dates should be checked and fixed in Wildlife Insights
# however, here we will provisionally exclude any deployments with incorrect dates


# check individual projects and years
# the idea is to compare the lags table with the plot of deployment operation
# use common sense to decide if lags and gaps are ok of if they point to problems
# lets summarily remove any suspicious deployment (provisionally)
# or redefine end_date if this is the case
# hopefuly we can automate this process in the future



#----- gurupi

calculate_time_lag(2002545, 2016)
print(lags)
plot_deployment_operation(2002545, 2016)
# which changes should be made
update_end <-  c("CT-RBG-1-14 2016-10-23")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002545, 2017)
print(lags)
plot_deployment_operation(2002545, 2017)


calculate_time_lag(2002545, 2018)
print(lags)
plot_deployment_operation(2002545, 2018)


calculate_time_lag(2002545, 2019)
print(lags)
plot_deployment_operation(2002545, 2019)


calculate_time_lag(2002545, 2020)
print(lags)
plot_deployment_operation(2002545, 2020)
# which changes should be made
to_remove <- c("CT-RBG-2-85 09/14/2020")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  print()


calculate_time_lag(2002545, 2021)
print(lags)
plot_deployment_operation(2002545, 2021)


calculate_time_lag(2002545, 2022)
print(lags)
plot_deployment_operation(2002545, 2022)
# which changes should be made
update_end <-  c("CT-RBG-2-73 2022-09-22")
to_remove <- c("CT-RBG-2-84 2022-09-24")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()

calculate_time_lag(2002545, 2023)
print(lags)
plot_deployment_operation(2002545, 2023)

calculate_time_lag(2002545, 2024)
print(lags)
plot_deployment_operation(2002545, 2024)
# which changes should be made
update_end <-  c("CT-RBG-1-04 2024-07-26", "CT-RBG-1-21 2024-07-26",
                 "CT-RBG-2-42 2024-07-26", "CT-RBG-2-80 2024-07-24", 
                 "CT-RBG-2-84 2024-07-27")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002545, 2025)
print(lags)
plot_deployment_operation(2002545, 2025)
# which changes should be made
update_end <- c("CT-RBG-2-83 2025-07-26", "CT_RBG-2-85 2025-07-26")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()



#----- terra do meio

calculate_time_lag(2002554, 2016)
print(lags)
plot_deployment_operation(2002554, 2016)
# which changes should be made
update_end <-  c("CT-TDM-1-74 2016-06-12")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002554, 2017)
print(lags)
plot_deployment_operation(2002554, 2017)
# which changes should be made
update_start <- c("CT-TDM-1-121 2017-06-01")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                              .default = start_date)) %>%
  ungroup() %>%
  print()



calculate_time_lag(2002554, 2018)
print(lags)
plot_deployment_operation(2002554, 2018)
# which changes should be made
update_end <- c("CT-TDM-1-17 2018-07-05", "CT-TDM-1-32 2018-07-04", 
                "CT-TDM-1-43 2018-07-04")
to_remove <- c("CT-TDM-1-87 2018-07-02")
update_start <- c( "CT-TDM-1-11 2018-07-06", "CT-TDM-1-86 2018-07-02")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002554, 2019)
print(lags)
plot_deployment_operation(2002554, 2019)
# which changes should be made
update_end <- c("CT-TDM-2-22 2019-09-06", "CT-TDM-2-42 2019-09-08")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002554, 2022)
print(lags)
plot_deployment_operation(2002554, 2022)
# which changes should be made
update_end <- c("CT-TDM-2-05 2022-06-17", "CT-TDM-2-31 2022-06-13", 
                "CT-TDM-2-66 2022-06-20", "CT-TDM-2-25 2022-06-21")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002554, 2023)
print(lags)
plot_deployment_operation(2002554, 2023)
# which changes should be made
update_end <- c("CT-TDM-2-102 2023-06-21")
to_remove <- c("CT-TDM-2-46 2023-06-23")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002554, 2025)
print(lags)
plot_deployment_operation(2002554, 2025)
# which changes should be made
update_end <- c("CT-TDM-2-16 2025-06-27", "CT-TDM-2-91 2025-07-01")
update_start <- c("CT-TDM-2-56 2025-06-29")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



#----- jamari

calculate_time_lag(2002562, 2016)
print(lags)
plot_deployment_operation(2002562, 2016)
# which changes should be made
update_start <- c("CT-669 2016-09-21")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002562, 2017)
print(lags)
plot_deployment_operation(2002562, 2017)
# which changes should be made
update_end <- c( "CT-709 2017-11-04")
to_remove <- c("CT-668 2017-11-12")
update_start <- c("CT-745 2017-11-08")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002562, 2018)
print(lags)
plot_deployment_operation(2002562, 2018)


calculate_time_lag(2002562, 2019)
print(lags)
plot_deployment_operation(2002562, 2019)
# which changes should be made
update_end <- c("CT-561 2019-08-06", "CT-591 2019-07-27")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002562, 2020)
print(lags)
plot_deployment_operation(2002562, 2020)
# which changes should be made
update_end <- c("CT-630 2020-11-06")
#update_start <- c("CT-524 2020-11-03")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002562, 2021)
print(lags)
plot_deployment_operation(2002562, 2021)
# which changes should be made
update_end <- c("CT-524 2021-11-04", "CT-592 2021-11-06", "CT-705 2021-11-07")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002562, 2023)
print(lags)
plot_deployment_operation(2002562, 2023)
# which changes should be made
update_end <- c("CT-143 23/09/2023", "CT-259 11/11/2023", "CT-631 26/09/2023", 
                "CT-745 19/09/2023")
to_remove <- c("CT-144 23/09/2023", "CT-708 20/09/2023", "CT-745 19/09/2023")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002562, 2024)
print(lags)
plot_deployment_operation(2002562, 2024)
# which changes should be made
update_end <- c("CT-182 20/09/2024", "CT-258 20/09/2024", "CT-562 18/09/2024",
                "CT-593 22/09/2024", "CT-630 22/09/2024", "CT-631 22/09/2024",
                "CT-632 22/09/2024", "CT-707 24/09/2024")
to_remove <- c("CT-560 17/09/2024", "CT-670 11/11/2024", "CT-708 19/09/2024", "CT-743 25/09/2024")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()



calculate_time_lag(2002562, 2025)
print(lags)
plot_deployment_operation(2002562, 2025)
# which changes should be made
update_end <- c("CT-144 08/10/2025", "CT-146 08/10/2025", "CT-183 07/10/2025",
                "CT-220 07/10/2025", "CT-221 07/10/2025", "CT-447 01/10/2025",
                "CT-591 04/10/2025", "CT-592 04/10/2025", "CT-593 04/10/2025", 
                "CT-599 01/10/2025", "CT-669 04/10/2025", "CT-670 03/10/2025",
                "CT-783 04/10/2025")
to_remove <- c("CT-257 07/10/2025", "CT-258 07/10/2025", "CT-521 01/10/2025",
               "CT-667 06/10/2025", "CT-706 06/10/2025")
# note: probably add days to photo_date in "CT-521 01/10/2025", check wildlife insights
update_start <- c("CT-744  25/10/2025")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()




#----- juruena

calculate_time_lag(2002576, 2016)
print(lags)
plot_deployment_operation(2002576, 2016)


calculate_time_lag(2002576, 2017)
print(lags)
plot_deployment_operation(2002576, 2017)
# which changes should be made
update_end <- c( "CT-PNJU-2-11 05/31/2017", "CT-PNJU-2-3 2017-06-01", "CT-PNJU-2-9 2017-06-01")
to_remove <- c("CT-PNJU-1-11 2017-05-31", "CT-PNJU-2-37 2017-06-04", "CT-PNJU-2-4 2017-06-03")
update_start <- c( "CT-PNJU-1-26 2017-06-05", "CT-PNJU-1-7 2017-06-05")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002576, 2018)
print(lags)
plot_deployment_operation(2002576, 2018)
# which changes should be made
update_start <- c("CT-PNJU-1-27 2018-06-15")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002576, 2019)
print(lags)
plot_deployment_operation(2002576, 2019)
# which changes should be made
update_end <- c( "CT-PNJU-1-25 2019-06-09")
update_start <- c("CT-PNJU-1-36 2019-06-04")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002576, 2020)
print(lags)
plot_deployment_operation(2002576, 2020)
# which changes should be made
update_end <- c("CT-PNJU-1-13 08/20/2020", "CT-PNJU-1-26 08/18/2020", 
                "CT-PNJU-1-34 08/18/2020", "CT-PNJU-1-4 08/21/2020", 
                "CT-PNJU-1-5 08/17/2020", "CT-PNJU-2-12 08/19/2020")
to_remove <- c("CT-PNJU-1-19 08/18/2020", "CT-PNJU-2-16 08/19/2020")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002576, 2022)
print(lags)
plot_deployment_operation(2002576, 2022)
# which changes should be made
update_end <- c("CT-PNJU-1-25 2022-08-15", "CT-PNJU-1-8 2022-08-13", 
                "CT-PNJU-2-36 2022-08-08")
to_remove <- c("CT-PNJU-2-10 2022-08-11", "CT-PNJU-2-28 2022-08-10")
update_start <- c("CT-PNJU-1-24 2022-08-15", "CT-PNJU-2-8 2022-08-11")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002576, 2024)
print(lags)
plot_deployment_operation(2002576, 2024)
# which changes should be made
update_end <- c("CT-PNJU-1-11 2024-06-19", "CT-PNJU-2-14 2024-07-03", 
                "CT-PNJU-2-16 2024-07-05")
to_remove <- c("CT-PNJU-2-22 2024-06-20", "CT-PNJU-2-34 2024-06-18")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()




#----- maraca

calculate_time_lag(2002584, 2018)
print(lags)
plot_deployment_operation(2002584, 2018)
# which changes should be made
update_end <- c("CT-EEM-1-13 2018-12-14", "CT-EEM-2-39 2018-12-18",
                "CT-EEM-2-40 2018-12-18")
update_start <- c("CT-EEM-2-40 2018-12-18")
# apply these changes to images
images <- images %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002584, 2021)
print(lags)
plot_deployment_operation(2002584, 2021)
# which changes should be made
update_end <- c("CT-EEM-1-5 2021-03-06", "CT-EEM-2-54 2021-03-04")
to_remove <- c( "CT-EEM-2-59 2021-03-05")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2002584, 2025)
print(lags)
plot_deployment_operation(2002584, 2025)
# which changes should be made
update_end <- c("CT-EEM-2-32 2025-11-30", "CT-EEM-2-33 2025-12-01",
                "CT-EEM-2-53 2025-12-01")
to_remove <- c("CT-EEM-2-22 2025-11-29", "CT-EEM-2-60 2025-12-03")
update_start <- c("CT-EEM-2-25 2025-12-02", "CT-EEM-2-26 2025-12-01", 
                  "CT-EEM-2-35 2025-12-01", "CT-EEM-2-36 2025-12-01")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



#----- tumucumaque

calculate_time_lag(2003586, 2021)
print(lags, n=Inf)
plot_deployment_operation(2003586, 2021)
# which changes should be made
update_end <- c("CT-PNMT-1 2021-11-01", "CT-PNMT-12 2021-11-05", 
                "CT-PNMT-19 2021-11-05", "CT-PNMT-21 2021-11-06", 
                "CT-PNMT-29 2021-11-01", "CT-PNMT-39 11/02/2021", 
                "CT-PNMT-40 11/02/2021", "CT-PNMT-45 11/05/2021", 
                "CT-PNMT-47 11/03/2021", "CT-PNMT-48 11/03/2021", 
                "CT-PNMT-59 11/03/2021")
to_remove <- c("CT-PNMT-34 11/01/2021", "CT-PNMT-49 11/03/2021",
               "CT-PNMT-55 11/03/2021", "CT-PNMT-62 11/03/2021")
update_start <- c("CT-PNMT-40 11/02/2021")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



calculate_time_lag(2003586, 2022)
print(lags, n=Inf)
plot_deployment_operation(2003586, 2022)
# which changes should be made
update_end <- c("CT-PNMT--20 2022-10-11", "CT-PNMT-13 2022-10-07",
                "CT-PNMT-23 2022-10-08", "CT-PNMT-24 2022-10-08",
                "CT-PNMT-29 2022-10-11", "CT-PNMT-33 2022-10-08", 
                "CT-PNMT-43 2022-10-10", "CT-PNMT-44A 2022-10-10", 
                "CT-PNMT-47 2022-10-10", "CT-PNMT-49 2022-10-08")
to_remove <- c("CT-PNMT-16 2022-10-07", "CT-PNMT-27 2022-10-07",
               "CT-PNMT-53 2022-10-10", "CT-PNMT-9 2022-10-07",
               "CT-PNMT-3 2022-12-07", "CT-PNMT-60 2022-10-08", 
               "CT-PNMT-61 2022-10-08")
update_start <- c("CT-PNMT-23 2022-10-08", "CT-PNMT-30 2022-10-09")
# crop any photos outside start and end (do it before saving at the end of script)
# check what to do later, delete for now: "CT-PNMT-3 2022-12-07", "CT-PNMT-60 2022-10-08", "CT-PNMT-61 2022-10-08"
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2003586, 2023)
print(lags, n=Inf)
plot_deployment_operation(2003586, 2023)
# which changes should be made
update_end <- c("CT PNMT ", "CT PNMT 0024", "CT PNMT 13", "CT PNMT 29",
                "CT PNMT 31 Oeste", "CT PNMT 37", "CT PNMT 46", "CT PNMT 50",
                "CT PNMT 52", "CT PNMT 53")
to_remove <- c("02", "CT PNMT 0023", "CT PNMT 023", "CT PNMT 11", "CT PNMT 15",
               "CT PNMT 23", "CT PNMT 34", "CT PNMT 38", "CT PNMT 4", 
               "CT PNMT 43", "CT PNMT 45", "CT PNMT 47", "CT PNMT 49",
               "CT PNMT 55", "CT PNMT 0057", "CT PNMT 0059", "CT PNMT 62")
update_start <- c("CT PNMT 48", "CT PNMT 55", "CT PNMT 6")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()


calculate_time_lag(2003586, 2024)
print(lags)
plot_deployment_operation(2003586, 2024)
# which changes should be made
update_end <- c("CT PNMT 2  2024-11-23", "CT PNMT 3  2024-11-23",
                "CT PNMT 38  2024-11-22", "CT PNMT 60  2024-11-21")
to_remove <- c("CT PNMT 14  2024-11-23", "CT PNMT 17  2024-11-23",
               "CT PNMT 20  2024-11-22", "CT PNMT 25  2024-11-23",
               "CT PNMT 4  2024-11-22", "CT PNMT 44B  2024-11-22",
               "CT PNMT 50  2024-11-21", "CT PNMT 54  2024-11-21",
               "CT PNMT 61  2024-11-21")
update_start <- c("CT PNMT 23  2024-11-23")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



#----- tapajos

calculate_time_lag(2004835, 2022)
print(lags)
plot_deployment_operation(2004835, 2022)



calculate_time_lag(2004835, 2023)
print(lags)
plot_deployment_operation(2004835, 2023)# which changes should be made
update_end <- c("CT-FT-M-16 30-11-2023", "CT-FT-M-29 29-11-2023")
to_remove <- c("CT-FT-M-31 29-11-2023")
#update_start <- c()
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



calculate_time_lag(2004835, 2025)
print(lags)
plot_deployment_operation(2004835, 2025)# which changes should be made
update_end <- c("CT-FNT-C-04 11-12-2025", "CT-FNT-C-06 11-12-2025", 
                "CT-FNT-C-19 12-12-2025", "CT-FNT-MAN-14 19/12/2025",
                "CT-FT-C-05 17-03-2025", "CT-FT-C-06 17-03-2025",
                "CT-FT-C-10 18-03-2025")
to_remove <- c("CT-FNT-MAN-04 18/12/2025", "CT-FNT-MAN-16 18-12-2025",
               "CT-FNT-MAN-17 18-12-2025")
#update_start <- c()
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()




#----- caxiuana

calculate_time_lag(2006780, 2023)
print(lags)
plot_deployment_operation(2006780, 2023)
# which changes should be made
update_end <- c("2023_CAX116", "2023_CAX212")
to_remove <- c("2023_CAX203", "2023_CAX208")
#update_start <- c()
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



calculate_time_lag(2006780, 2024)
print(lags)
plot_deployment_operation(2006780, 2024)# which changes should be made
update_end <- c("2024_CAX322", "2024_CAX323")
to_remove <- c("2024_CAX357")
#update_start <- c()
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



calculate_time_lag(2006780, 2025)
print(lags, n=Inf)
plot_deployment_operation(2006780, 2025)
# which changes should be made
update_end <- c("2025_CAX358", "2025_CAX101", "2025_CAX115", "2025_CAX127",
                "2025_CAX129", "2025_CAX305", "2025_CAX313", "2025_CAX318",
                "2025_CAX341", "2025_CAX342", "2025_CAX343", "2025_CAX356")
to_remove <- c("2025_CAX102", "2025_CAX103", "2025_CAX107", "2025_CAX109",
               "2025_CAX118", "2025_CAX210", "2025_CAX213", "2025_CAX215",
               "2025_CAX218", "2025_CAX220", "2025_CAX221", "2025_CAX224", 
               "2025_CAX226", "2025_CAX230", "2025_CAX309", "2025_CAX323",
               "2025_CAX351", "2025_CAX352", "2025_CAX359")
update_start <- c("2025_CAX115", "2025_CAX343")
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



#----- tapirape
#calculate_time_lag(2007402, 2024)
#print(lags)
#plot_deployment_operation(2007402, 2024)


#----- jari

calculate_time_lag(2007439, 2024)
print(lags, n=Inf)
plot_deployment_operation(2007439, 2024)
# which changes should be made
update_end <- c("CT-JARI-03 2024-07-01", "CT-JARI-07 2024-07-02",
                "CT-JARI-10 2024-06-30", "CT-JARI-11 2024-06-30", 
                "CT-JARI-14 2024-07-02", "CT-JARI-18 2024-07-01",
                "CT-JARI-20 2024-07-01", "CT-JARI-21 2024-07-01",
                "CT-JARI-22 2024-07-01", "CT-JARI-23 2024-07-01",
                "CT-JARI-29 2024-07-01", "CT-JARI-33 2024-06-30",
                "CT-JARI-34 2024-07-18", "CT-JARI-42 2024-07-02", 
                "CT-JARI-46 2024-07-06", "CT-JARI-50 2024-06-30", 
                "CT-JARI-55 2024-07-02", "CT-JARI-56 2024-07-02")
to_remove <- c("CT-JARI-27 2024-07-03", "CT-JARI-36 2024-06-29",
               "CT-JARI-12 2024-06-30", "CT-JARI-16 2024-07-02",
               "CT-JARI-17 2024-07-02", "CT-JARI-19 2024-07-01",
               "CT-JARI-26 2024-07-02", "CT-JARI-47 2024-07-06",
               "CT-JARI-51 2024-07-01", "CT-JARI-63 2024-07-02")
#update_start <- c()
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



calculate_time_lag(2007439, 2025)
print(lags)
plot_deployment_operation(2007439, 2025)# which changes should be made
update_end <- c("CT-JARI-05 2025-06-07", "CT-JARI-12 2025-06-06",
                "CT-JARI-30 2025-06-08", "CT-JARI-33 2025-06-05")
to_remove <- c("CT-JARI-11 2025-06-06", "CT-JARI-17 2025-06-10",
              "CT-JARI-57 2025-06-06", "CT-JARI-14 2025-06-07",
              "CT-JARI-16 2025-06-11", "CT-JARI-35 2025-06-08",
              "CT-JARI-38 2025-06-08", "CT-Jari_15 2025-06-08")
update_start <- c("CT-JARI-08 2025-06-08")
# crop first photos of CT-JARI-41 2025-06-05 (in the end of script)
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print() 



#----- rio acre
calculate_time_lag(2009348, 2025)
print(lags, n=Inf)
plot_deployment_operation(2009348, 2025)
# which changes should be made
update_end <- c()
to_remove <- c()
update_start <- c()
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()




#----- descobrimento
calculate_time_lag(2011116, 2025)
print(lags)
plot_deployment_operation(2011116, 2025)
# which changes should be made
update_end <- c()
to_remove <- c()
update_start <- c()
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()



# bom jesus
#


#----- monte pascoal
calculate_time_lag(2012253, 2025)
print(lags)
plot_deployment_operation(2011116, 2025)
# which changes should be made
update_end <- c()
to_remove <- c()
update_start <- c()
# apply these changes to images
images <- images %>%
  filter(! deployment_id %in% c(to_remove)) %>%
  group_by(deployment_id) %>%
  mutate(end_date = case_when(deployment_id %in% c(update_end) ~ max(photo_date) + 2,
                              .default = end_date)) %>%
  mutate(start_date = case_when(deployment_id %in% c(update_start) ~ min(photo_date) - 2,
                                .default = start_date)) %>%
  ungroup() %>%
  print()




#----- update deployments based on corrected dates


# note: lets also remove projects that have only one year of data as well as
# projects with too many problems in dates
projects <- projects %>%
  filter(! project_id %in% c(2003586, 2004835, 2006780, 2007439,
                             2009348, 2011116, 2012253)) %>%
  print()

deployments <- deployments %>%
  filter(project_id %in% projects$project_id) %>%
  print()

images <- images %>%
  filter(project_id %in% projects$project_id) %>%
  print()



#----- crop any photos outside range of start and end dates
images <- images %>%
  filter(photo_date >= start_date,
         photo_date <= end_date) %>%
  print()


#----- update deployments start and end dates using the images updates
deployments <- deployments %>%
  select(-c(start_date, end_date)) %>%
  left_join(images %>%
              distinct(deployment_id, start_date, end_date),
            by = "deployment_id") %>%
  filter(deployment_id %in% images$deployment_id) %>%
  print()

# remove start_date and end_date from images
# now that we updated them we can keep them only in deployments
images <- images %>%
  select(-c(start_date, end_date)) %>%
  print()


#----- remove deployments with < 5 days sampling
deployments <- deployments %>%
  filter(difftime(end_date, start_date, units = "days") >= 5) %>%
  print()

images <- images %>%
  filter(deployment_id %in% deployments$deployment_id) %>%
  print()
  


#----- save aggregated data
data_fixed_dates <- list(projects=projects,
                         deployments=deployments,
                         images=images)
saveRDS(data_fixed_dates, "01_entrada/dados_processados/data_fixed_dates.rds")

