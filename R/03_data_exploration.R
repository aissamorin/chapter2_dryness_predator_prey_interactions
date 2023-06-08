#########################################################################################################################

###########################         Data Exploration                                             ######################

########################################################################################################################

require(ggplot2)



# Ia Within sample variation - replicate dry mass ####


#' Get dry mass coefficient of variation for each samples (i.e. sample_ID)
#'
#' @param clean_fat_data : fat data cleaned with 'clean_raw_fat_data' function
#' @param save whether we want the table to be saved or not
#'
#' @return table with nb of replicates, mean dry masses, SD dry masses and Coefficient of variation (CV) for each sample(i.e. Sample_ID)
#' @export

get_CV_dry_mass_table <- function(tab = clean_fat_data,
                                  save = T){



  CV_dry_mass_table <-

    tab %>%
# group by sample ID to get summary stat per sample_ID
  dplyr::group_by(samples_ID) %>%
  dplyr::summarise(nb_sub_samples = dplyr::n(), # number of replicates for each carcass samples
                   mean_dry_mass = mean(sample_dry_mass),# mean dry mass for each sample_ID
                   sd_dry_mass = sd(sample_dry_mass)) %>%# standard deviation for each sample_ID
  dplyr::mutate(CV_dry_mass = sd_dry_mass/mean_dry_mass) # coefficient of variation = sd/mean for each sample_ID

#CV < 1 i.e. SD < mean --> relatively low variability ?

  #To save table
  if(save == TRUE){
readr::write_csv2(CV_dry_mass_table, file =here::here("output", "data_exploration", "CV_dry_mass_table.csv"))
}

  CV_dry_mass_table

}


# graphical representation

#' Get boxplots of dry masses for each samples (i.e. sample_ID)
#'
#' @param clean_fat_data  fat data cleaned with 'clean_raw_fat_data' function
#' @param save whether we want the figure to be saved or not
#'
#' @return boxtplots of dry masses ~ sample_ID
#' @export

get_dry_mass_boxplot <- function(tab = clean_fat_data,
                                 save = T){


  # Set color scale
  lbls <- c('early', 'intermediate','late')
  vec_color <- c( "#1f78b4", "#f6c200", "#a50f15")# Yolan color code : blue= <6, Yellow = between 6 and 8 days , red = > 8 days


sample_dry_mass_boxplot <-

  tab %>%
  ggplot(aes(x = samples_ID, y= sample_dry_mass, fill = collection_interval)) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = (element_text(color ='black',
                                    angle = 45,
                                    hjust = 1,
                                    size = 6) ))+
  scale_fill_manual( name = "Collection date :",
                     labels = c('< 6 days', 'between 6 and 8 days', '>8 days'),
                     values = setNames(vec_color, lbls)) +
  labs(x = "Samples ID",
       y = "Sample dry mass")


#To save figure
if(save == TRUE){
ggsave(sample_dry_mass_boxplot, file =here::here("output", "data_exploration", "sample_dry_mass_boxplot.jpg"), device = "jpg")
}

sample_dry_mass_boxplot

}


# Ib Within sample variation - replicate fat rate ####

#get fat rate coefficient of variation (for each sample_ID)

#' Get fat rate coefficient of variation for each samples (i.e. per sample_ID)
#'
#' @param tab fat data cleaned with 'clean_raw_fat_data' function, default is clean_fat_data
#' @param save whether we want the table to be saved or not
#'
#' @return return a table with the nb of replicates, mean fat rate, SD fat rate and Coefficient of variation (CV) for each sample(i.e. Sample_ID)
#' @export

get_CV_fat_rate_table <- function(tab = clean_fat_data,
                                  save = T){


CV_fat_rate_table <-

  tab %>%
  dplyr::mutate(sample_replicate_fat_rate = sample_dry_mass/sample_wet_mass) %>%

  # group by sample ID to get summary stat per sample_ID
  dplyr::group_by(samples_ID) %>%
  dplyr::summarise(nb_sub_samples = dplyr::n(), # number of replicates for each carcass samples
                   mean_fat_rate = mean(sample_replicate_fat_rate),# mean fat rate for each sample_ID
                   sd_fat_rate = sd(sample_replicate_fat_rate)) %>%# fat rate standard deviation for each sample_ID
  dplyr::mutate(CV_fat_rate = sd_fat_rate/mean_fat_rate) # coefficient of variation = sd/mean for each sample ID


if(save == TRUE){
#save table
readr::write_csv2(CV_fat_rate_table, file =here::here("output", "data_exploration", "CV_fat_rate_table.csv"))
}

#return
CV_fat_rate_table

}


#graphical representation


#' Get boxplots of fat rates for each samples (i.e. sample_ID)
#'
#' @param tab fat data cleaned with 'clean_raw_fat_data' function, default is clean_fat_data
#' @param save whether we want the figure to be saved or not
#'
#' @return boxtplots of fat rates as a function of/for each sample_ID
#' @export

get_fat_rate_boxplot <- function(tab = clean_fat_data,
                                 save =  TRUE){


# Set colors scale
lbls <- c('early', 'intermediate','late')
vec_color <- c( "#1f78b4", "#f6c200", "#a50f15")# Yolan color code : blue= <6, Yellow = between 6 and 8 days , red = > 8 days


fat_rate_CV_boxplot <-

tab %>%
  # computes fate rate for each repclicate (--> will give several values for each sample ID )
  dplyr::mutate(sample_replicate_fat_rate = sample_dry_mass/sample_wet_mass) %>%

#ggplot boxplot
ggplot(aes(x = samples_ID, y= sample_replicate_fat_rate, fill = collection_interval)) +
  geom_boxplot()+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = (element_text(color ='black',
                                    angle = 45,
                                    hjust = 1,
                                    size = 6) ))+
  scale_fill_manual( name = "Collection date :",
                     labels = c('< 6 days', 'between 6 and 8 days', '>8 days'),
                     values = setNames(vec_color, lbls)) +
  labs(x = "Samples ID",
       y = "Replicate fat rate")

#To save figure
if(save == TRUE){
ggsave(fat_rate_CV_boxplot, file =here::here("output", "data_exploration", "fat_rate_CV_boxplot.jpg"), device = "jpg")
}

# return
fat_rate_CV_boxplot

}


# Histogramme ####

#histogram

#' Get an histogram of fat rate coefficients of variation
#'
#' @param tab table with fat rate coefficients of variation
#' @param save whether we want the figure to be saved or not, default = F
#'
#' @return a histogram of fat rate coeeficient of variation
#' @export

get_fat_rate_CV_hist <- function(tab,
                                 save = F){


  fat_rate_CV_hist <-
    #tab <=> CV_fat_rate_table
    ggplot(tab, aes(CV_fat_rate))+
    geom_histogram(fill = '#ab3329', color = '#65150b')+
    theme_bw()+
    theme(legend.position = "bottom",
          axis.text.x = (element_text(color ='black')))+
    labs(x = "Fat rate coefficient of variation")

  #To save figure
  if(save == TRUE){
    ggsave(fat_rate_CV_boxplot, file =here::here("output", "data_exploration", "fat_rate_CV_hist.jpg"), device = "jpg")
  }


  return(fat_rate_CV_hist)

}

# 3 samples with a CV > 0.25

#' Get a table with the higher coefficient of variation
#'
#' @param tab table with clean fat data
#' @param value the value above which a CV is considered high, default = 0.25
#' @param save whether we want the table to be saved of not, default = F
#'
#' @return a table of the samples with fat rate coefficient of variation (strictly) higher than 'value'
#' @export

get_high_CV_table <- function(tab,
                              value = 0.25,
                              CV_fat_rate_table,
                              save = FALSE){



  high_cv_table <-

    tab %>% #clean data set
    dplyr::select(samples_ID,
                  cluster_start_date,
                  collection_interval,
                  predator_species,
                  carcass_species,
                  sex,
                  age,
                  bone_type,
    ) %>%
    dplyr::left_join(.,CV_fat_rate_table ) %>%
    dplyr::filter(CV_fat_rate > value )


  if(save == TRUE){
    #save table
    readr::write_csv2(high_cv_table, file =here::here("output", "data_exploration", paste("high_cv_table",value,".csv")))

  }



  return(high_cv_table)

}



# --------------------------------------------------------------------------------------------------------------#


# II Fat rate analyses ####

#> "Gold standard" data ####


#>> data preparation ####

#' Get 'gold standard' data, i.e. keep adult and old individuals only, with bones collected before 6 days
#'
#' @param tab table containing (cleaned) fat data, default = clean_fat_data
#' @param save whether we want the table to be saved, default = FALSE
#'
#' @return return a table
#' @export

get_gs_data <- function(tab = clean_fat_data,
                        save = FALSE){



gs_data <-

  tab %>%
  # keep only rows with  adult and old indiv & bones collected before 6 days
  dplyr::filter(collection_interval == 'early',
                age %in% c('old', 'adult')) %>%
  #Select columns of interest
  dplyr::select(samples_ID,
                cluster_start_date,
                collection_interval,
                predator_species,
                carcass_species,
                sex,
                age,
                bone_type,
                sample_wet_mass,
                sample_dry_mass) %>%

  dplyr::mutate(sample_replicate_fat_rate = sample_dry_mass/sample_wet_mass,# compute fat rate
                cluster_month = lubridate::month(cluster_start_date), #get the month the prey was killed (?)
                #compute season from cluster_month
                season1 = dplyr::if_else(cluster_month %in% c(7:10),# lean_season: 7-10, productive season: 1-6 & 11,12
                                         'lean',
                                         'productive'),
                season2 = dplyr::if_else(cluster_month %in% c(1:7),# early_dry: 1-7, dry: 7-12
                                         'early_dry',
                                         'dry')) %>%
  #change factor level orger (for graphical presentation purpose) from 1.dry 2.early-dry to 1.early_dry  2.dry
  dplyr::mutate(season1 = forcats::fct(season1,levels = c("lean","productive")),
                season2 = forcats::fct(season2,levels = c("early_dry","dry")))



if(save == TRUE){

  #To save gs_data table
  readr::write_csv2(gs_data, file =here::here("output", "data_exploration", "gs_data_table.csv"))

}

return(gs_data)

}



# gs_data %>%
#   dplyr::group_by(carcass_species) %>%
#   dplyr::summarise(dplyr::n_distinct(samples_ID))


#>> Carcass number summary table  ####

#' Get carcass number summary table
#'
#' @param tab the table with fat data
#' @param ... variable/column we want to group by i.e. get the number of carcasses (sample size) per (e.g.) per cluster_month, season or species
#'
#' @return return a table with the number of carcasses for each variable of choice
#' @export
#'
get_nb_carcass_table <- function(tab,
                                 ...){



  tab %>%
    #group by the variable for which to count the number of carcasses/sample size must be counted
    dplyr::group_by(...) %>%
    # count
    dplyr::summarise(dplyr::n_distinct(samples_ID)) %>% # to count the number of carcasses which are coded with a distinct sample_ID
    # rename the count column
    dplyr::rename(Count = `dplyr::n_distinct(samples_ID)`)

}


#>> BOXPLOTS ####


#  boxplots of fat rate values (not mean) as a function of cluster_month

# Across all species


#' Get boxplots of fate rate value or mean fat rate as a function of cluster_month or season, all species together
#'
#' @param tab the table with fat data
#' @param varx explanatory variable (e.g. cluster_month, season1, season 2,...), character string !
#' @param vary variable to be explained (?, eg. sample_replicate_fat_rate, mean_fat_rate), character string !
#'
#' @return return a boxplot figure of the variable to explain ~ explanatory variable
#' @export

boxplot_all_sp <- function(tab, varx, vary){

  #Args to run function :
  # tab<- gs_data
  # vary <- 'sample_replicate_fat_rate'
  # varx <- 'cluster_month'



 bp <-  tab %>%
    ggplot(aes(x = as.factor(.data[[varx]]), y = .data[[vary]])) + # must index the '.data' with '[[varX]]' the env-variable is a character vector (& because of tidyverse data masking
    geom_boxplot()+
    theme_bw()+
    theme(legend.position = "bottom",
          axis.text.x = (element_text(color ='black'))) +
    labs(x = varx,
         y = vary)



#save the above plot

  ggsave(
         filename = paste({{vary}},'per',{{varx}},'all_sp.jpg', sep ='_'), # filename : vary_per_varx_all_sp
         device = 'jpg',
         path = here::here("output", "data_exploration"))

  bp


}


#' Get boxplots of fate rate value or mean fat rate as a function of cluster_month or season, for a subset of species
#'
#' @param tab the table with fat data
#' @param varx explanatory variable (e.g. cluster_month, season1, season 2,...), character string !
#' @param vary variable to be explained (?, eg. sample_replicate_fat_rate, mean_fat_rate), character string !
#' @param species_vec vector of species of interest, default = c('buffalo', 'nyala', 'warthog')
#'
#' @return return a boxplot figure of the variable to explain ~ explanatory variable facetted by carcass_species
#' @export

boxplot_main_sp <- function(tab,
                            varx,
                            vary,
                            species_vec = c('buffalo', 'nyala', 'warthog')){



  #Args to run function :
  # tab<- gs_data
  # vary <- 'sample_replicate_fat_rate'
  # varx <- 'cluster_month'
  # species_vec <- c('buffalo', 'nyala', 'warthog')


  bp <-  tab %>%
    dplyr::filter(carcass_species %in% species_vec) %>%
    # must index the '.data' with '[[varX]]' the env-variable is a character vector (& because of tidyverse data masking
    ggplot(aes(x = as.factor(.data[[varx]]), y = .data[[vary]])) +
    geom_boxplot()+
    facet_wrap(~carcass_species) + #facet by carcass_species
    theme_bw()+
    theme(legend.position = "bottom",
          axis.text.x = (element_text(color ='black'))) +
    labs(x = varx,
         y = vary)

  #To save the above plot
  ggsave(filename = paste({{vary}},'per',{{varx}},paste({{species_vec}}, collapse = "_"),'.jpg', sep ='_'),
         device = 'jpg',
         path = here::here("output", "data_exploration"))

  bp

}




# # >> statistical analyses (exploratory) ####
#
# data_tab <-
#
#   gs_data %>%
#   dplyr::group_by(samples_ID, cluster_start_date, cluster_month, season1) %>%
#   dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%
#   dplyr::ungroup() %>%
#   dplyr::select(samples_ID, season1, mean_fat_rate)
#
#
# #Question: is there an effect of the season on the (mean?) fat rate of the prey killed by lions ?
#
# # explanatory variable : season 1 : qualitative
# # Response variable : mean fat rate : quantitative
#
# str(data_tab)
# summary(data_tab)
#
# boxplot(data_tab$mean_fat_rate ~ data_tab$season1 )
# plot(data_tab$mean_fat_rate)
#
#
# # Normality ?
# hist(data_tab$mean_fat_rate, breaks = 26)
# shapiro.test(data_tab$mean_fat_rate)
#
# # data do not follow a normal distribution --> non parametric tests
# # --> Wilcoxon Man Whitney :
#
# wm1<- wilcox.test(data_tab$mean_fat_rate ~ data_tab$season1)
# wm1
#
#
#
# #test ANOVA or 2-sample t-test
#
# #ANOVA model
# lm1 <- lm(data_tab$mean_fat_rate ~ data_tab$season1,data = data_tab ) #Yi = mu + alpha_i
#
# summary(lm1)
#
# # test du modèle
# lm0 <- lm( data_tab$mean_fat_rate ~ 1)
#
# anova(lm0, lm1)
# anova(lm1)
#
#
# # Data transformation ?
#
# hist(log(data_tab$mean_fat_rate), breaks = 26)
# hist(sqrt(data_tab$mean_fat_rate), breaks = 26)
#




#-------------------------------------------------------------------------------------------------------------#





# > Larger sample analyses ####
# sub-adult, adult & old individuals, with bones collected before 6 days and within 6 to 8 days


#'  Get larger sample data, i.e. keep sub-adult, adult & old individuals only, with bones collected before 6 days or between 6  & 8 days
#'
#' @param tab table with cleaned fat data
#' @param save whether we want the data to be saved or not
#'
#' @return a table with larger sample data
#' @export

get_ls_data <- function(tab = clean_fat_data,
                        save = FALSE){



  ls_data <-

    tab %>%
    # keep only rows with old or adult or sub-adult individuals & bones collected before 6 days or between 6 & 8 days
    dplyr::filter(collection_interval %in% c('early','intermediate'),
                  age %in% c('old', 'adult', 'sub-adult')) %>%
    #Select columns of interest
    dplyr::select(samples_ID,
                  cluster_start_date,
                  collection_interval,
                  predator_species,
                  carcass_species,
                  sex,
                  age,
                  bone_type,
                  sample_wet_mass,
                  sample_dry_mass) %>%

    dplyr::mutate(sample_replicate_fat_rate = sample_dry_mass/sample_wet_mass,# compute fat rate
                  cluster_month = lubridate::month(cluster_start_date), #get the month the prey was killed (?)
                  #compute season from cluster_month
                  season1 = dplyr::if_else(cluster_month %in% c(7:10),# lean_season: 7-10, productive season: 1-6 & 11,12
                                           'lean',
                                           'productive'),
                  season2 = dplyr::if_else(cluster_month %in% c(1:7),# early_dry: 1-7, dry: 7-12
                                           'early_dry',
                                           'dry')) %>%
    #change factor level orger (for graphical presentation purpose) from 1.dry 2.early-dry to 1.early_dry  2.dry
    dplyr::mutate(season1 = forcats::fct(season1,levels = c("lean","productive")),
                  season2 = forcats::fct(season2,levels = c("early_dry","dry")))



  if(save == TRUE){

    #To save gs_data table
    readr::write_csv2(ls_data, file =here::here("output", "data_exploration", "ls_data_table.csv"))

  }

  return(ls_data)

}



# III Body condition analyses ####

#> Gold standard data ####

#>> Data preparation ####

#' Get 'gold standard' body condition data, i.e. keep adult and old individuals only, with bones collected before 6 days
#'
#' @param tab table containing (cleaned) fat data, default = clean_fat_data
#' @param save whether we wish the table to be saved or not
#'
#' @return return a table with mean fat rates translated into body condition, 3 levels : emaciated (>0.20), thin (between 0.20 & 0.70) & good (>0.70)
#' @export

get_body_condition_data_gs <- function(tab = clean_fat_data,
                                       save = FALSE){




bc_data_gs <-

  tab %>%
  # keep only rows with  adult and old indiv & bones collected before 6 days
  dplyr::filter(collection_interval == 'early',
                age %in% c('old', 'adult')) %>%
  #Select columns of interest
  dplyr::select(samples_ID,
                cluster_start_date,
                collection_interval,
                predator_species,
                carcass_species,
                sex,
                age,
                bone_type,
                sample_wet_mass,
                sample_dry_mass) %>%

  dplyr::mutate(sample_replicate_fat_rate = sample_dry_mass/sample_wet_mass,# compute fat rate
                cluster_month = lubridate::month(cluster_start_date), #get the month the prey was killed (?)
                #compute season from cluster_month
                season1 = dplyr::if_else(cluster_month %in% c(7:10),# lean: 7-10, productive season: 1-6, 11,12
                                         'lean',
                                         'productive')) %>%
  #change factor level order (for graphical presentation purpose)
  dplyr::mutate(season1 = forcats::fct(season1,levels = c("lean","productive"))) %>%
  # compute mean fat rate
                  dplyr::group_by(samples_ID,
                                  cluster_start_date,
                                  collection_interval,
                                  predator_species,
                                  carcass_species,
                                  sex,
                                  age,
                                  bone_type,
                                  cluster_month,
                                  season1) %>%
                  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%

  dplyr::mutate(body_condition = dplyr::if_else(mean_fat_rate < 0.2,
                                                'emaciated',
                                                dplyr::if_else(mean_fat_rate >0.20 & mean_fat_rate <0.70,
                                                        'thin',
                                                        'good'))) %>%
  #change factor level orger (for graphical presentation purpose) from 1.dry 2.early-dry to 1.early_dry  2.dry
  dplyr::mutate(body_condition = forcats::fct(body_condition,levels = c("emaciated","thin","good")))

if(save == TRUE){

  #To save gs_data table
  readr::write_csv2(bc_data_gs, file =here::here("output", "data_exploration", "bodycondi_data_gs_table.csv"))

}

return(bc_data_gs)

}

# >> Boxplots ####

# option 1 ####

#' Get boxplots of carcasses body condition as a function of season, faceted by season
#'
#' @param tab table with fat rates translated in body condition
#' @param save whether we wish the figure to be saved
#'
#' @return return a boxplot figure
#' @export

bc_season_bp_op1 <- function(tab,
                        save = FALSE){





# option 1
# Set color scale
lbls1 <- c('emaciated', 'thin','good')
vec_color1 <- c( "#a50f15", "#ebc174", "#79ad41")



bp <-

  tab %>%
  ggplot2::ggplot(aes(x = body_condition, fill = body_condition))+
  geom_bar()+
  facet_wrap(~season1)+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = (element_text(color ='black')))+
  scale_fill_manual( name = "Body condition :",
                     labels = c('Emaciated', 'Thin', 'Good'),
                     values = setNames(vec_color1, lbls1)) +
  labs(x = "")

if(save == TRUE){

ggsave(bp, file =here::here("output", "data_exploration", "boxplot_bc_face(season)_op1.jpg"), device = "jpg")

  }

return(bp)

}


# option 2 ####


#' Get boxplots of carcasses body condition as a function of season, faceted by season
#'
#' @param tab tab with fat rates translated into body condition
#' @param savewhether we wish the figure to be saved
#'
#' @return a boxplot figure
#' @export

bc_season_bp_op2  <- function(tab,
                        save = FALSE){





# option 2
# Set color scale
lbls2 <- c('lean', 'productive')
vec_color2 <- c( "#ebc174", "#79ad41" )


bp <-

  tab %>%
  ggplot2::ggplot(aes(x = season1, fill = season1))+
  geom_bar()+
  facet_wrap(~body_condition)+
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = (element_text(color ='black'))) +
  scale_fill_manual( name = "Season:",
                     labels = c('Lean season', 'Productive season'),
                     values = setNames(vec_color2, lbls2)) +
  labs(x = "")


if(save == TRUE){

    ggsave(bp, file =here::here("output", "data_exploration", "boxplot_facet(bc)_season_op2.jpg"), device = "jpg")

  }

return(bp)

}


# + emaciated indiv (<20) during dry season vs early dry --> reverse --> + good indiv (>70) early dry vs dry season



# Main species ####

# > Option 1 ####

#' Get species boxplots of carcasses body condition as a function of season, faceted by season
#'
#' @param tab Table with fat rate converted into a categorical body condition variable
#' @param sp the species of interest
#' @param save whether we want the graph to be saved or not, default = FALSE
#'
#' @return a bar plot faceted per season for the species of interest
#' @export

bc_season_bp_op1_sp <- function(tab,
                                sp,
                                save = FALSE){





  # option 1
  # Set color scale
  lbls1 <- c('emaciated', 'thin','good')
  vec_color1 <- c( "#a50f15", "#ebc174", "#79ad41")



  bp <-

    tab %>%
    dplyr::filter(carcass_species == sp) %>%
    dplyr::group_by(season1, body_condition ) %>%
    dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good'))) %>%
    dplyr::summarise(nb_bc = dplyr::n()) %>%
    dplyr::mutate(percentage = nb_bc/sum(nb_bc)*100) %>%

    # graph
    ggplot2::ggplot(aes(body_condition, percentage, fill = body_condition)) +
    geom_col() +
    facet_wrap(~season1) +
    theme_bw()+
    theme(legend.position = "bottom",
          axis.text.x = (element_text(color ='black')))+
    scale_fill_manual( name = "Body condition :",
                       labels = c('Emaciated', 'Thin', 'Good'),
                       values = setNames(vec_color1, lbls1),
                       drop = FALSE) +
    scale_x_discrete(labels = c('Emaciated', 'Thin', 'Good'),
                     drop = FALSE)+
    labs(x = "")

  if(save == TRUE){

    ggsave(bp, file =here::here("output", "data_exploration", paste("boxplot_bc_facet(season)_op1",sp,".jpg")), device = "jpg")

  }

  return(bp)

}

# option 2 ####


#' Get species boxplot of carcasses body condition as a function of season, faceted by body condition
#'
#' @param tab Table with fat rate converted into a categorical body condition variable
#' @param sp The species of interest
#' @param save Whether you want to save the figure or not
#'
#' @return a barplot faceted per body condition for the species of interest
#' @export

bc_season_bp_op2_sp  <- function(tab,
                              sp,
                              save = FALSE){





  # option 2
  # Set color scale
  lbls2 <- c('lean', 'productive')
  vec_color2 <- c( "#ebc174", "#79ad41" )


  bp <-

    tab %>%
    dplyr::filter(carcass_species == sp) %>%
    dplyr::group_by(body_condition, season1  ) %>%
    dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good'))) %>%
    dplyr::summarise(nb_bc = dplyr::n()) %>%
    dplyr::mutate(percentage = nb_bc/sum(nb_bc)*100) %>%



    ggplot2::ggplot(aes(season1, percentage, fill = season1))+
    geom_col()+
    facet_wrap(~body_condition, drop = FALSE)+
    theme_bw()+
    theme(legend.position = "bottom",
          axis.text.x = (element_text(color ='black'))) +
    scale_fill_manual( name = "Season:",
                       labels = c('Lean season', 'Productive season'),
                       values = setNames(vec_color2, lbls2),
                       drop =  FALSE) +
    scale_x_discrete(labels = c('Lean season', 'Productive season'),
                     drop = FALSE)+
    labs(x = "")



  if(save == TRUE){

    ggsave(bp, file =here::here("output", "data_exploration", paste("boxplot_facet(bc)_season_op2", sp,".jpg")), device = "jpg")

  }

  return(bp)

}


# > Larger sample analyses ####


#' Get 'larger sample' body condition data, i.e. keep subadult, adult & old individuals, with bones collected before 6 days or between 6 & 8 days
#'
#' @param tab table containing cleaned fat data
#' @param save whether we want the outcome to be saved or not
#'
#' @return a table with mean fat rate
#' @export

get_body_condition_data_ls <- function(tab = clean_fat_data,
                                       save = FALSE){




  bc_data_ls <-

    tab %>%
    # keep only rows with old or adult or sub-adult individuals & bones collected before 6 days or between 6 & 8 days
    dplyr::filter(collection_interval %in% c('early','intermediate'),
                  age %in% c('old', 'adult', 'sub-adult')) %>%
    #Select columns of interest
    dplyr::select(samples_ID,
                  cluster_start_date,
                  collection_interval,
                  predator_species,
                  carcass_species,
                  sex,
                  age,
                  bone_type,
                  sample_wet_mass,
                  sample_dry_mass) %>%

    dplyr::mutate(sample_replicate_fat_rate = sample_dry_mass/sample_wet_mass,# compute fat rate
                  cluster_month = lubridate::month(cluster_start_date), #get the month the prey was killed (?)
                  #compute season from cluster_month
                  season1 = dplyr::if_else(cluster_month %in% c(7:10),# lean: 7-10, productive season: 1-6, 11,12
                                           'lean',
                                           'productive')) %>%
    #change factor level order (for graphical presentation purpose)
    dplyr::mutate(season1 = forcats::fct(season1,levels = c("lean","productive"))) %>%
    # compute mean fat rate
    dplyr::group_by(samples_ID,
                    cluster_start_date,
                    collection_interval,
                    predator_species,
                    carcass_species,
                    sex,
                    age,
                    bone_type,
                    cluster_month,
                    season1) %>%
    dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%

    dplyr::mutate(body_condition = dplyr::if_else(mean_fat_rate < 0.2,
                                                  'emaciated',
                                                  dplyr::if_else(mean_fat_rate >0.20 & mean_fat_rate <0.70,
                                                                 'thin',
                                                                 'good'))) %>%
    #change factor level orger (for graphical presentation purpose) from 1.dry 2.early-dry to 1.early_dry  2.dry
    dplyr::mutate(body_condition = forcats::fct(body_condition,levels = c("emaciated","thin","good")))

  if(save == TRUE){

    #To save gs_data table
    readr::write_csv2(bc_data_ls, file =here::here("output", "data_exploration", "bodycondi_data_ls_table.csv"))

  }

  return(bc_data_ls)

}



# III Fat rate evolution & bone freshness #####

#> Data preparation

#' Get data set to explore fat rate change with increasing analysis waiting time
#'
#' @param tab table with raw fat data (i.e. not cleaned yet), should be fat_data_raw
#' @param save whether we want the table to be save or not, default = F
#'
#' @return a table with both sample tested at normal time, and tested according different waiting times
#' @export

get_fat_rate_evol_data <- function(tab ,
                                   save = FALSE ) {




temp <-

 tab %>% # tab = fat_data_raw
  # select column of interest from sample_ID to sample_dry_mass
  dplyr::select(samples_ID:sample_dry_mass) %>%
  # Keep only  replicated samples tested after -Jx, etc... /ex 'Hip_B_046-J12'
  dplyr::filter(stringr::str_detect(samples_ID, "\\-[:upper:]\\d+$"))

# get the carcass(carcass_ID) corresponding to the sample_ID tested after Jx :
 keep <- unique(temp$carcass_ID)


#get the table with the samples tested without waiting corresponding to the carcasses in table 'temp':
  tab2 <- tab %>%
    # select column of interest from sample_ID to sample_dry_mass
    dplyr::select(samples_ID:sample_dry_mass) %>%
    # Remove replicates & samples tested after -Jx, etc... /ex 'Hip_B_046-J12' (--> to avoid duplicates in next steps)
    dplyr::filter(!stringr::str_detect(samples_ID, "\\-[:upper:]\\d+$"),
                  carcass_ID %in% keep) %>% # keep the raws (i.e. samples and sample replicate) corresponding to the carcass tested after Jx (stored in the 'keep' vector)
    # create a J function, contains the number of days before the sample is test, J_base mean normal time (i.e. without waiting <=> bone is fresh), Jx = tested after x days
    dplyr::mutate(J = factor('J_base'))


  # final table :
  fat_rate_evol_data <-

    temp %>%
  # remove all 'mass_j1, 'mass_j2' etc... columns '\\d+$' = any number suits
  dplyr::select(!matches("mass_j\\d+$")) %>%
  #remove columns: "samples_marrow_fat_rate", "samples_marrow_fat_rate_without_residuals", "bone_marrow_fat_rate_mean",
  #"bone_marrow_fat_rate_mean_without_residuals","estimated_bc","estimated_bc_without_residuals",  "comments" and any columns after,
  #Remove empty lines after 233rd row
  dplyr::filter(!is.na(samples_ID)) %>%
  # create a J function, contains the number of days before the sample is test, J_base mean normal time, Jx = tested after x days
  dplyr::mutate(J = stringr::str_extract(samples_ID, "[:upper:]\\d+$")) %>%
    #transform J in factors, and set factors levels (and order)
  dplyr::mutate(J = factor(J, levels = c('J_base','J8', 'J9', 'J12'))) %>%
    # Bind temp (with samples tested after Jx) & tab2 (with corresponding samples tested without waiting )
    dplyr::bind_rows(., tab2) %>%
  #Select columns of interest
  dplyr::select(samples_ID,
                carcass_ID,
                cluster_start_date,
                collection_interval,
                predator_species,
                carcass_species,
                #sex,
                #age,
                #bone_type,
                sample_wet_mass,
                sample_dry_mass,
                J) %>%
  #Compute replicate fat rate (i.e. one per line)
  dplyr::mutate(sample_replicate_fat_rate = sample_dry_mass/sample_wet_mass) %>%
    # Arrange per increasing carcass_ID
  dplyr::arrange(carcass_ID)



# to save
if (save == TRUE) {

  readr::write_csv2(fat_rate_evol_data, here::here("output/data_exploration/fat_rate_evolution_dataset.csv"))
}


return(fat_rate_evol_data)

}


#> Figures ####

#>> Fat rate ~Bone freshness ####

#' Get boxplots comparing replicate fat rates as function of bone 'freshness', facetted per carcass_ID
#'
#' @param tab table with both sample tested at normal time, and tested according different waiting times
#' @param save whether we want to save the figure or not, default = F
#'
#' @return a boxplot comparing replicate fat rates as function of bone 'freshness', facetted per carcass_ID
#' @export

fat_rate_bone_fresh_bp <- function(tab,
                                   save = FALSE){




#setting color scale
lbls <- c('J_base', 'J8', 'J9', 'J12')
vec_color <- c("#a40000","#16317d","#007e2f","#ffcd12")


bp <-

  tab %>% #tab = fat_rate_evol_data
  ggplot2::ggplot(aes(x = J , y = sample_replicate_fat_rate  , fill = J)) +
  geom_boxplot() +
  facet_wrap(~carcass_ID) +
  theme_bw()+
  theme(legend.position = "bottom",
        axis.text.x = (element_text(color ='black'))) +
  scale_fill_manual( name = "Bone freshness :",
                     #labels = c('Emaciated', 'Thin', 'Good'),
                     values = setNames(vec_color, lbls)) +
  labs(x = "Bone 'freshness' (?)",
       y = 'Replicate fat rate')


if(save == TRUE){

  ggsave(bp, file =here::here("output", "data_exploration", "bp_fat_rate_bone_fresh.jpg"), device = "jpg")

}

return(bp)

}

#>> Mean fat rate ~ Bone freshness ####

#' Get scatterplot showing mean fat rates as function of bone 'freshness', facetted per carcass_ID
#'
#' @param tab table with both sample tested at normal time, and tested according different waiting times
#' @param save whether we want the figure to be saved or not, default = F
#'
#' @return a scatterplot of mean fat rate as a function of bone 'freshness', facetted by carcass_ID
#' @export

mean_fat_rate_bone_fresh_bp <- function(tab,
                                        save = F){




#setting color scale
lbls <- c("HiP_C_0474", "HiP_C_0532", "HiP_C_0508")
vec_color <- c("#a40000", "#007e2f","#16317d")


pl <-

  tab %>%  # %>% #tab = fat_rate_evol_data
  dplyr::group_by(samples_ID, carcass_ID, cluster_start_date, carcass_species, J ) %>%
  dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>% #compute mean sample fat rate


  ggplot2::ggplot(aes(x = J , y = mean_fat_rate , group =  carcass_ID, col = carcass_ID)) +
  # to get a scatterplot <=> a geom_point + a geom_line
  geom_point() +
  geom_line() +
  facet_wrap(~carcass_ID) + # need grouping by carcass ID in the aes
  theme_bw()+
  theme(legend.position = "none",
        axis.text.x = (element_text(color ='black'))) +
  scale_color_manual(values = setNames(vec_color, lbls)) +
  labs(x = "Bone 'freshness' (?)",
       y = 'Mean fat rate')



if(save == TRUE){

  ggsave(bp, file =here::here("output", "data_exploration", "plot_mean_fat_rate_bone_fresh.jpg"), device = "jpg")

}

return(pl)

}

#########################################################################################################################################################
######################################## DRAFT ##################################################################
##################################################################################################################



# # per month
#
# nb_sample_per_month <-
#
#   gs_data %>%
#   dplyr::group_by(cluster_month) %>%
#   dplyr::summarise(dplyr::n())

# per season1

# nb_sample_per_season1 <-
#
#   gs_data %>%
#   dplyr::group_by(season1) %>%
#   dplyr::summarise(dplyr::n())
#
#
# # per season2
#
# nb_sample_per_season2 <-
#
#   gs_data %>%
#   dplyr::group_by(season2) %>%
#   dplyr::summarise(dplyr::n())
#
# # per species
#
# nb_sample_per_species <-
#
#   gs_data %>%
#   dplyr::group_by(carcass_species) %>%
#   dplyr::summarise(dplyr::n())
#
# readr::write_csv2(nb_sample_per_species, file =here::here("output", "data_exploration", "nb_sample_per_species.csv"))
#
# # species with the most samples: Buffalo, Nyala & Warthog
#
# # per species & season 1
#
# nb_sample_per_species <-
#
#   gs_data %>%
#   dplyr::group_by(carcass_species, season1) %>%
#   dplyr::summarise(dplyr::n())
#
# # per species & season 2
#
# nb_sample_per_species <-
#
#   gs_data %>%
#   dplyr::group_by(carcass_species, season2) %>%
#   dplyr::summarise(dplyr::n())


# Boxplot
# sample_fat_rate_month <- gs_data %>%
#   ggplot(aes(x = as.factor(cluster_month), y= sample_replicate_fat_rate)) +
#   geom_boxplot() %>%
#
# ggsave(sample_fat_rate_month, file =here::here("output", "data_exploration", "sample_fat_rate_month_boxplot.jpg"), device = "jpg")



###

#per main species, i.e. 'buffalo', 'nyala', 'warthog'
# sample_fat_rate_month_species <-
#
#   gs_data %>%
#   dplyr::filter(carcass_species %in% c('buffalo', 'nyala', 'warthog')) %>%
#
#   ggplot(aes(x = as.factor(cluster_month), y = sample_replicate_fat_rate)) +
#   geom_boxplot()+
#   facet_wrap(~carcass_species)
#
# ggsave(sample_fat_rate_month_species, file =here::here("output", "data_exploration", "sample_fat_rate_month_boxplot_species.jpg"), device = "jpg")


# boxplots of mean fat rate as a function of cluster_month

# data

# #gs_data_mean <-
#
#   gs_data %>%
#   dplyr::group_by(samples_ID, cluster_start_date, cluster_month, carcass_species ) %>%
#   dplyr::summarise(mean_fat_rate = mean(sample_replicate_fat_rate)) %>%
#
#   boxplot_main_sp( tab = .,
#                   varx= 'cluster_month',
#                   vary= 'mean_fat_rate',
#                   species_vec)
#
#
# # all species
# mean_fat_rate_month <-
#
# temp_data_mean %>%
#   ggplot(aes(x = as.factor(cluster_month), y= mean_fat_rate)) +
#   geom_boxplot()
#
# ggsave(mean_fat_rate_month, file =here::here("output", "data_exploration", "mean_fat_rate_month_boxplot.jpg"), device = "jpg")
#
#
# # main species i.e. 'buffalo', 'nyala', 'warthog'
# mean_fat_rate_month_species <- temp_data_mean %>%
#   dplyr::filter(carcass_species %in% c('buffalo', 'nyala', 'warthog')) %>%
#
#   ggplot(aes(x = as.factor(cluster_month), y = mean_fat_rate)) +
#   geom_boxplot()+
#   facet_wrap(~carcass_species)
#
# ggsave(mean_fat_rate_month_species, file =here::here("output", "data_exploration", "mean_fat_rate_month_boxplot_species.jpg"), device = "jpg")
#



