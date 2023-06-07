#Make file


# Load functions and deps ?
devtools::load_all()

#DATA PREPARATION ####

# Read data fat analyses raw dataset

fat_data_raw <- read_fat_data_raw()

# Clean (and save) raw data

clean_fat_data <- clean_raw_fat_data(fat_data_raw,
                                     save = TRUE,
                                     filename = 'cleaned_fat_data')



#-----------------------------------------------------------------------------------------------------------------------------------------#




# DATA EXPLORATION ####

#> I Within sample variability ####

#>> a) Replicate dry mass

# get Coefficient of variation table
CV_dry_mass_table <- get_CV_dry_mass_table(clean_fat_data,
                                           save = T)

  # Get dry mass boxplots (for each )
sample_dry_mass_boxplot <- get_dry_mass_boxplot(clean_fat_data,
                                                save = T)

#>> b) Replicate fat rate

# get Coefficient of variation table
CV_fat_rate_table <- get_CV_fat_rate_table(clean_fat_data,
                                               save = T)

  # Get dry mass boxplots (for each )
fat_rate_boxplot <- get_fat_rate_boxplot(clean_fat_data,
                                             save =  F)

# Histogram

CV_fat_rate_hist <- get_fat_rate_CV_hist(tab = CV_fat_rate_table,
                     save = F)

high_CV_table <- get_high_CV_table(tab = clean_fat_data,
                  CV_fat_rate_table = CV_fat_rate_table,
                  value = 0.25,
                  save = F)

##> II Fat rate analysis ########


# A) 'Gold standard' analyses ####

#> data preparation ####

gs_data <- get_gs_data(clean_fat_data)

#> Carcass number summary tables ####

# per species

get_nb_carcass_table(gs_data,
                    carcass_species) #... = carcass_species

#per cluster month
get_nb_carcass_table(gs_data,
                     cluster_month) #... = cluster_month

#per specie & per month
get_nb_carcass_table(gs_data,
                     carcass_species,cluster_month) #... = cluster_month

#per season 1
get_nb_carcass_table(gs_data,
                     season1) #... = season1

#per season 2
get_nb_carcass_table(gs_data,
                     season2)  #... = season2

#per species & season 1
get_nb_carcass_table(gs_data,
                     carcass_species,season1) #... = carcass_species, season1

#per species & season 2
get_nb_carcass_table(gs_data,
                     carcass_species,season2) #... = carcass_species, season2


#> Boxplots ####

#>> Per month ####

 #fat rate value ~cluster month all sp

boxplot_all_sp (tab = gs_data,
                varx = 'cluster_month' ,
                vary = 'sample_replicate_fat_rate' )


#fat rate value ~cluster month main sp
boxplot_main_sp(tab = gs_data,
                            varx = 'cluster_month' ,
                            vary ='sample_replicate_fat_rate',
                            species_vec = c('buffalo', 'nyala', 'warthog'))



# mean fat rate ~ cluster_month all sp

# data preparation, compute the mean fat rate per sample_ID
gs_data %>%
  dplyr::group_by(samples_ID, cluster_start_date, cluster_month) %>%
  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%

  boxplot_all_sp (tab = .,
                  varx = 'cluster_month' ,
                  vary = 'mean_fat_rate' )

# mean fat rate ~ cluster_month main sp

# data preparation, compute the mean fat rate per sample_ID & per species
gs_data %>%
  dplyr::group_by(samples_ID, cluster_start_date, cluster_month, carcass_species ) %>%
  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%
  #dplyr::filter(carcass_species %in% c('buffalo', 'nyala', 'warthog')) %>%

  boxplot_main_sp( tab = .,
                   varx= 'cluster_month',
                   vary= 'mean_fat_rate',
                   species_vec = c('buffalo', 'nyala', 'warthog'))


###>> Per Season ####

# since season 1 is more balanced than season 2 --> use season 1

#fat rate value ~ season1 (early dry = 2-6 & dry = 7-11), all sp

boxplot_all_sp (tab = gs_data,
                varx = 'season1' ,
                vary = 'sample_replicate_fat_rate' )

# fat rate value ~ season1 (early dry = 2-6 & dry = 7-11), main sp

boxplot_main_sp(tab = gs_data,
                varx = 'season1' ,
                vary ='sample_replicate_fat_rate',
                species_vec = c('buffalo', 'nyala', 'warthog'))


# mean fat rate ~ season 1 all sp

# data preparation, compute the mean fat rate per sample_ID & per season1
gs_data %>%
  dplyr::group_by(samples_ID, cluster_start_date, cluster_month, season1) %>%
  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%

  boxplot_all_sp (tab = .,
                  varx = 'season1' ,
                  vary = 'mean_fat_rate' )



# mean fat rate ~ season 1 main sp

# data preparation, compute the mean fat rate per sample_ID, per season1 & per species

gs_data %>%
  dplyr::group_by(samples_ID, cluster_start_date, cluster_month, carcass_species, season1 ) %>%
  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%
  dplyr::filter(carcass_species %in% c('buffalo', 'nyala', 'warthog')) %>%

  boxplot_main_sp( tab = .,
                   varx= 'season1',
                   vary= 'mean_fat_rate',
                   species_vec = c('buffalo', 'nyala', 'warthog'))


# B) Larger sample analyses ####
#Include sub-adult in addition to adult & old individuals, with bones collected before 6 days and within 6 to 8 days

# > data preparation ####

ls_data <- get_ls_data(tab= clean_fat_data,
                       save = F)

#> carcass number summaries ####


# per species

get_nb_carcass_table(ls_data,
                     carcass_species) #... = carcass_species

#per cluster month
get_nb_carcass_table(ls_data,
                     cluster_month) #... = cluster_month

#per specie & per month
get_nb_carcass_table(ls_data,
                     carcass_species,cluster_month) #... = cluster_month

#per season 1
get_nb_carcass_table(ls_data,
                     season1) #... = season1



#per species & season 1
get_nb_carcass_table(ls_data,
                     carcass_species,season1) #... = carcass_species, season1



#> Boxplots ####

#>> Per month ####

#fat rate value ~cluster month all sp

boxplot_all_sp (tab = ls_data,
                varx = 'cluster_month' ,
                vary = 'sample_replicate_fat_rate' )


#fat rate value ~cluster month main sp
boxplot_main_sp(tab = ls_data,
                varx = 'cluster_month' ,
                vary ='sample_replicate_fat_rate',
                species_vec = c('buffalo', 'nyala', 'warthog'))


# mean fat rate ~ cluster_month all sp

# data preparation, compute the mean fat rate per sample_ID
ls_data %>%
  dplyr::group_by(samples_ID, cluster_start_date, cluster_month) %>%
  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%

  boxplot_all_sp (tab = .,
                  varx = 'cluster_month' ,
                  vary = 'mean_fat_rate' )

# mean fat rate ~ cluster_month main sp

# data preparation, compute the mean fat rate per sample_ID & per species

ls_data %>%
  dplyr::group_by(samples_ID, cluster_start_date, cluster_month, carcass_species ) %>%
  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%
  #dplyr::filter(carcass_species %in% c('buffalo', 'nyala', 'warthog')) %>%

  boxplot_main_sp( tab = .,
                   varx= 'cluster_month',
                   vary= 'mean_fat_rate',
                   species_vec = c('buffalo', 'nyala', 'warthog'))

###>> Per Season ####

# since season 1 is more balanced than season 2 --> use season 1

#fat rate value ~ season1 (early dry = 2-6 & dry = 7-11), all sp

boxplot_all_sp (tab = ls_data,
                varx = 'season1' ,
                vary = 'sample_replicate_fat_rate' )

# fat rate value ~ season1 (early dry = 2-6 & dry = 7-11), main sp

boxplot_main_sp(tab = ls_data,
                varx = 'season1' ,
                vary ='sample_replicate_fat_rate',
                species_vec = c('buffalo', 'nyala', 'warthog'))


# mean fat rate ~ season 1 all sp

# data preparation, compute the mean fat rate per sample_ID & per season1
ls_data %>%
  dplyr::group_by(samples_ID, cluster_start_date, cluster_month, season1) %>%
  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%

  boxplot_all_sp (tab = .,
                  varx = 'season1' ,
                  vary = 'mean_fat_rate' )



# mean fat rate ~ season 1 main sp

# data preparation, compute the mean fat rate per sample_ID, per season1 & per species

ls_data %>%
  dplyr::group_by(samples_ID, cluster_start_date, cluster_month, carcass_species, season1 ) %>%
  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%
  dplyr::filter(carcass_species %in% c('buffalo', 'nyala', 'warthog')) %>%

  boxplot_main_sp( tab = .,
                   varx= 'season1',
                   vary= 'mean_fat_rate',
                   species_vec = c('buffalo', 'nyala', 'warthog'))



# II Body condition analyses ####

# A) Gold standard data ####

#> data preparation ####

bc_gs_data <- get_body_condition_data_gs(tab = clean_fat_data,
                           save =  T)

#> Option 1 - bc ~ season facet with season  ####

bc_season_bp_op1(tab = bc_gs_data,
                 save = T)

# Per species

#buffalo
bc_season_bp_op1_sp(tab = bc_gs_data,
                    sp = 'buffalo',
                    save = FALSE)
#warthog
bc_season_bp_op1_sp(tab = bc_gs_data,
                    sp = 'warthog',
                    save = FALSE)
#warthog
bc_season_bp_op1_sp(tab = bc_gs_data,
                    sp = 'nyala',
                    save = FALSE)


#> Option 2 - bc ~ season facet with bc  ####

 bc_season_bp_op2(tab = bc_gs_data,
                  save = T)

# Per species

bc_season_bp_op2_sp(tab = bc_gs_data,
                    sp = 'buffalo',
                    save = FALSE)

bc_season_bp_op2_sp(tab = bc_gs_data,
                    sp = 'warthog',
                    save = FALSE)

bc_season_bp_op2_sp(tab = bc_gs_data,
                    sp = 'nyala',
                    save = FALSE)


# A) Larger sample analyses ####

#> data preparation ####

bc_ls_data <- get_body_condition_data_ls(tab = clean_fat_data,
                           save = F)
