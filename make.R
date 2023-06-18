#Make file


# Load functions and deps ?
devtools::load_all()

#DATA PREPARATION ####

# Read data fat analyses raw dataset

#fat_data_raw <- read_fat_data_raw()

# Read updated data fat analyses raw dataset (from 8th June 2023)

fat_data_raw_2 <- read_fat_data_raw_2()

# Clean (and save) raw data

clean_fat_data <- clean_raw_fat_data(fat_data_raw_2,
                                     save = FALSE,
                                     filename = 'cleaned_fat_data')

# read body condition of prey available in the landscape
landsc_BC_data <- read_data_land_BC()

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


# > Statistical analyses #####

# All species ####

wilcox_mann_whitney(tab = gs_data,
                    sp = NULL) # necessary argument to run over all species


# Buffalo ####

wilcox_mann_whitney(tab = gs_data,
                sp = 'buffalo')

# Nyala ####

mann_whitney_sp(tab = gs_data,
                sp = 'nyala')


# Warthog ####

mann_whitney_sp(tab = gs_data,
                sp = 'warthog')


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


# > Statistical analyses - Wilcoxon-Mann-Whitney ####

# All species  ####

wilcox_mann_whitney(tab = ls_data,
                    sp = NULL)

# Buffalo ####

wilcox_mann_whitney(tab = ls_data,
                    sp = 'buffalo')

# Nyala   ####

wilcox_mann_whitney(tab = ls_data,
                    sp = 'nyala')

# Wharthog ####

wilcox_mann_whitney(tab = ls_data,
                    sp = 'warthog')

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

#buffalo
bc_season_bp_op2_sp(tab = bc_gs_data,
                    sp = 'buffalo',
                    save = FALSE)


#warthog
bc_season_bp_op2_sp(tab = bc_gs_data,
                    sp = 'warthog',
                    save = FALSE)

#nyala
bc_season_bp_op2_sp(tab = bc_gs_data,
                    sp = 'nyala',
                    save = FALSE)


# > Statistical analysis ####

chi_test(tab = bc_gs_data,
         sp = NULL)

# Buffalo ####

chi_test(tab = bc_gs_data,
         sp = 'buffalo')

# Nyala ####

chi_test(tab = bc_gs_data,
         sp = 'nyala')

# Warthog ####

chi_test(tab = bc_gs_data,
         sp = 'warthog')



# B) Larger sample analyses ####

#> data preparation ####

bc_ls_data <- get_body_condition_data_ls(tab = clean_fat_data,
                           save = F)


#> Option 1 - bc ~ season facet with season  ####

bc_season_bp_op1(tab = bc_ls_data,
                 save = T)

# Per species

#buffalo
bc_season_bp_op1_sp(tab = bc_ls_data,
                    sp = 'buffalo',
                    save = FALSE)
#warthog
bc_season_bp_op1_sp(tab = bc_ls_data,
                    sp = 'warthog',
                    save = FALSE)
#nyala
bc_season_bp_op1_sp(tab = bc_ls_data,
                    sp = 'nyala',
                    save = FALSE)

#> Option 2 - bc ~ season facet with bc  ####

bc_season_bp_op2(tab = bc_ls_data,
                 save = T)

# Per species

#buffalo
bc_season_bp_op2_sp(tab = bc_ls_data,
                    sp = 'buffalo',
                    save = FALSE)

#warthog
bc_season_bp_op2_sp(tab = bc_ls_data,
                    sp = 'warthog',
                    save = FALSE)

#nyala
bc_season_bp_op2_sp(tab = bc_ls_data,
                    sp = 'nyala',
                    save = FALSE)


# > Statistical analysis - Chi square test ####

# All species ####

chi_test(tab = bc_ls_data,
         sp = NULL)

# Buffalo ####

chi_test(tab = bc_ls_data,
         sp = 'buffalo')

# Nyala ####

chi_test(tab = bc_ls_data,
         sp = 'nyala')

# Warthog ####

chi_test(tab = bc_ls_data,
         sp = 'warthog')

# III Fat rate evolution & bone freshness #####

# > Data preparation ####

fat_rate_evol_data <- get_fat_rate_evol_data(tab = fat_data_raw_2, # fat_data_raw_2 : updated fat data from june 2023
                       save = F)

# > Figures ####

#>> Boxplot of fat rate ~ Bone 'freshness'

fat_rate_bone_fresh_bp(tab = fat_rate_evol_data)

#>> Scatterplot of mean fat rare ~ Bone 'freshness'

mean_fat_rate_bone_fresh_bp(tab = fat_rate_evol_data)

############################# END DATA EXPLORATION ###########################################################

########################################## CHAPTER ##################################################################

# Load functions and deps ?
devtools::load_all()

#DATA PREPARATION ####

# Read data fat analyses raw dataset

#fat_data_raw <- read_fat_data_raw()

# Read updated data fat analyses raw dataset (from 8th June 2023)

fat_data_raw_2 <- read_fat_data_raw_2()

# Clean (and save) raw data

clean_fat_data <- clean_raw_fat_data(fat_data_raw_2,
                                     save = TRUE,
                                     filename = 'cleaned_fat_data')

# read body condition of prey available in the landscape
#landsc_BC_data <- read_data_land_BC()

# read body condition of prey available in the landscape
landsc_BC_data_sp <- read_data_land_BC_sp()

# body condition data (larger sample) of prey 'used' by lion
bc_ls_data <- get_body_condition_data_ls(tab = clean_fat_data,
                                         save = F)



# ANALYSES ####

# All species ####

data_use_all_sp <- get_data_use(data_use = bc_ls_data,
                                save = F,
                                species = F) #across all species


data_av_all_sp <- get_data_available(data_av = landsc_BC_data_sp,
                                     save = T,
                                     species = F) #across all species


#combine the two data set
data_all_sp <- combine_data(data_use = data_use_all_sp,
                            data_av = data_av_all_sp,
                            save = F,
                            species = F) #across all species


# Jacob index - all species

ratio_table_all_sp <- get_ratio_table(data_tab = data_all_sp,
                                      save = F,
                                      species = NULL)

# Per species ####

#data preparation

data_use_per_sp <- get_data_use(data_use = bc_ls_data,
                                save = F,
                                species = T)


data_av_per_sp <- get_data_available(data_av = landsc_BC_data_sp,
                                    save = F,
                                    species = T)

#combine data set :

#combine the two data set
data_per_sp <- combine_data(data_use = data_use_per_sp,
                           data_av = data_av_per_sp,
                           save = F,
                           species = T)



# Jacob index - per species

ratio_table_buffalo <- get_ratio_table(data_tab = data_per_sp,
                                      save = F,
                                      species = "buffalo")


ratio_table_nyala <- get_ratio_table(data_tab = data_per_sp,
                                       save = F,
                                       species = "nyala")

ratio_table_warthog <- get_ratio_table(data_tab = data_per_sp,
                                       save = F,
                                       species = "warthog")


# FIGURES ####


# All species

#Figure  - Barplot

# All species

barplot_all_sp <- get_fig_barplot(data_tab = data_all_sp,
                                  save = F,
                                  species = NULL)



# Per species

barplot_buffalo <- get_fig_barplot(data_tab = data_per_sp,
                                  save = F,
                                  species = 'buffalo')


barplot_nyala <- get_fig_barplot(data_tab = data_per_sp,
                                   save = F,
                                   species = 'nyala')

barplot_warthog <- get_fig_barplot(data_tab = data_per_sp,
                                   save = F,
                                   species = 'warthog')


# Figure - Figure ratio

# All species

fig_ratio_all_sp <- get_fig_ratio(ratio_table = ratio_table_all_sp,
                                  save = F,
                                  species = NULL)

# Per species

fig_ratio_buffalo <- get_fig_ratio(ratio_table = ratio_table_buffalo,
                                  save = F,
                                  species = 'buffalo')


fig_ratio_nyala <- get_fig_ratio(ratio_table = ratio_table_nyala,
                                   save = F,
                                   species = 'nyala')
#one value missing

fig_ratio_warthog <- get_fig_ratio(ratio_table = ratio_table_buffalo,
                                   save = F,
                                   species = 'warthog')

# APPENDIX ####

# Appendix 1 - Within sample variability Fat rates CV ####


# Get dry mass boxplots (for each )
get_figA1a_fat_rate_cv(tab = clean_fat_data,
                       save = T)

# get Coefficient of variation table
CV_fat_rate_table <- get_CV_fat_rate_table(clean_fat_data,
                                           save = T)

# Histogram
get_figA1b_CV_hist(tab = CV_fat_rate_table,
                   save =  T)


high_CV_table <- get_high_CV_table(tab = clean_fat_data,
                                   CV_fat_rate_table = CV_fat_rate_table,
                                   value = 0.25,
                                   save = F)


# Appendix 2 - MFR as a function of bone freshness ####

# > Data preparation

fat_rate_evol_data <- get_fat_rate_evol_data(tab = fat_data_raw_2, # fat_data_raw_2 : updated fat data from june 2023
                                             save = F)

# Figure A2

get_figA2_fat_rate_bone_fresh(tab = fat_rate_evol_data,
                              save = T)

