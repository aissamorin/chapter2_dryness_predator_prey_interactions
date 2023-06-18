####################################################### CHAPTER ANALYSES ###################################################


# BODY_CONDITION - Jacob Indices ####

# > Jacob Index ####

#' Compute Jacob's index
#'
#' @param a proportion of prey used (i.e. killed) by lions
#' @param b proportion of prey available (for comparison)
#'
#' @return D, jacob index value indicating of a selection/avoidance of a over b (?), standardized, range from -1 (max avoidance) to +1 (maximum preference)
#' @export

J_I <- function(a,b){


  D = (a-b)/(a+b-2*a*b)

  return(D)

}


# > ALL SPECIES ####


#> data preparation ####



# data prey 'used' (i.e. killed) by lions

#' Get table of prey used (i.e. killed) by lions
#'
#' @param data_use a table with fat data and mean fat rate translated into categorical body conditions of 'used' prey
#' @param save whether we want to save the table or not (if so, will be saved in the 'analyses' folder), default = F
#' @param species TRUE or FALSE, if TRUE : compute number and proportion made by 'used' prey individual per season and body condition category and species
#'
#' @return a table with the number and proportion made by 'used' prey individual per season and body condition category
#' @export

get_data_use <- function(data_use,
                         save = FALSE,
                         species = T){



  data_use %<>% # data_use = bc_ls_data
  #rename season1 into 'season'
  dplyr::rename(season = season1)

  # whether we want the data with details per species (species = TRUE) or not (species = FALSE)
  if(species == TRUE){data_use %<>% dplyr::group_by(season, carcass_species, body_condition ) }
  else { data_use %<>% dplyr::group_by(season, body_condition )}


  data_use %<>%
  dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good')),
                season = factor(season, levels = c('productive', 'lean'))) %>%
  #compute the number of individual prey within each category per season and per body condition
  dplyr::summarise(nb_bc = dplyr::n()) %>%
  #Compute proportion per season ( must equate 100% within each season)
  dplyr::mutate(proportion = nb_bc/sum(nb_bc)) %>%
  dplyr::mutate(prey = 'used') %>%
  dplyr::ungroup()



#To save table
  if(save == TRUE & species == TRUE){

  readr::write_csv2(data_use, file =here::here("output", "data_analyses", "data_use_per_sp.csv"))

  }else if (save == TRUE & species == FALSE){

    readr::write_csv2(data_use, file =here::here("output", "data_analyses", "data_use_all_sp.csv"))}


return(data_use)

}


# data with body condition of prey 'available' in the landscape


#' Get table of body condition of prey available in the landscape
#'
#' @param data_av a table with fat data and mean fat rate translated into categorical body conditions of available prey
#' @param save whether we want to save the table or not (if so, will be saved in the 'analyses' folder), default = F
#' @param species TRUE or FALSE, if TRUE : compute number and proportion made by 'used' prey individual per season and body condition category and species
#'
#' @return a table with the number and proportion made by 'available' prey individual per season and body condition category
#' @export

get_data_available <- function(data_av,
                               save = FALSE,
                               species = FALSE){



  data_available <-

  data_av %>% # data_av = landsc_BC_data_sp
  # Combined fat and good body condition
  dplyr::mutate(good = good + fat) %>%
  #remove the 'fat' body condition column/category (not necessary anymore)
  dplyr::select(!c(fat)) %>%
  #rename season into 'season1'
  #dplyr::rename(season1 = season) %>%
  # Get a table with season, body condition and nb of individual in each category
  tidyr::pivot_longer(cols = c(emaciated, thin, good), names_to= 'body_condition', values_to = "nb_bc")

  # whether we want the data with details per species (species = TRUE) or not (species = FALSE)

  if(species == TRUE){data_available %<>% dplyr::group_by(carcass_species, season)
    } else{

    # get number of individuals in each body conidtion category (across all species)
    data_available %<>%
    dplyr::select(!carcass_species) %>%
    dplyr::group_by(season, body_condition) %>%
    dplyr::summarise(nb_bc = sum(nb_bc)) }

  data_available %<>%
  # Compute the number of individual prey within each category per season and per body condition
  dplyr::mutate(proportion = nb_bc/sum(nb_bc)) %>%
  dplyr::mutate(prey = 'available') %>%
  dplyr::ungroup() %>%
  dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good')),
                season = factor(season, levels = c('productive', 'lean')))



  #To save table
  if(save == TRUE & species == TRUE){
    readr::write_csv2(data_available, file =here::here("output", "data_analyses", "data_available_per_sp.csv"))}
  else if (save == TRUE & species == FALSE){
    readr::write_csv2(data_available, file =here::here("output", "data_analyses", "data_available_all_sp.csv"))}


return(data_available)

}


#Combine data_use and data_available into one dataset for Jacob index calculation and figures

#' Combine data_use and data_available into one dataset for Jacob index calculation and figures
#'
#' @param tab_use table with the number and proportion made by 'used' prey individual per season and body condition category
#' @param tab_av table with the number and proportion made by 'available' prey individual per season and body condition category
#' @param save whether we want to save the table, default = F
#' @param species TRUE or FALSE, TRUE when combining dataset with species details, otherwise FALSE
#'
#' @return a table with the number and proportion made by 'used' and 'available' prey individuals per season and body condition category
#' @export

combine_data <- function(data_use,
                         data_av,
                         save = FALSE,
                         species = FALSE){

# changer pour get_data_analyses/data_ratio

  data_combined <- dplyr::bind_rows(data_use, data_av) # tab_use = data_use_all_sp , tab_av = data_av_all_sp

  #To save table
  if(save == TRUE & species == TRUE){
    readr::write_csv2(data_combined, file =here::here("output", "data_analyses", "data_per_sp_combined.csv"))}
  else if (save == TRUE & species == FALSE){
    readr::write_csv2(data_combined, file =here::here("output", "data_analyses", "data_all_sp_combined.csv")) }



  return(data_combined)

  }


#> RATIO table ####

#' Get table with jacob indices
#'
#' @param data_tab dataset combining used and available prey body condition data and proportion in each catagory
#' @param save whether we want to save the table, default = F
#' @param species must specified the species if ones want ratio table for a given species, otherwise NULL (default) give ratio across species
#'
#' @return a table with proportion of used and available prey, and jacob index J_I(used,av) --> selection/avoidance of used in comparison with the available
#' @export

get_ratio_table <- function(data_tab,
                            save = F,
                            species = NULL){



  ratio_table <-

  data_tab %>% # data_tab = data_per_sp , data_tab = data_all_sp
  #remove the counts column
  dplyr::select(!nb_bc) %>%
  # Get used and available proportion of BC per season as column (instead of rows)
  tidyr::pivot_wider(names_from = prey, values_from = proportion) %>%
  dplyr::relocate(used, .before = available) %>%
  # Replace NA with 0, when there where no individuals in a given body condition category
  dplyr::mutate(used = tidyr::replace_na(used, 0),
                available = tidyr::replace_na(available, 0))

  if(!is.null(species)){
  ratio_table %<>% dplyr::filter(carcass_species == species)}

  #Compute Jacob's index (using function J_I()) :
  ratio_table %<>%
  dplyr::mutate(Jacob_Index = J_I(used,available))


  #To save table
  if(save == TRUE & !is.null(species)){
    readr::write_csv2(ratio_table, file =here::here(paste("output", "data_analyses", "ratio_table",species,".csv", sep = '')))}
  else if (save == TRUE & is.null(species)){
  readr::write_csv2(ratio_table, file =here::here("output", "data_analyses", "ratio_table_all_sp.csv"))
}

return(ratio_table)

}




################ DRAFT #########################

#> Data preparation ####

# chi_square_use_av <- function(tab_use,
#                               tab_av,
#                               season){
#
#   data_use <-
#
#     tab_use %>% # tab_use = bc_ls_data
#     #dplyr::group_by(season1, body_condition) %>%
#     dplyr::filter(season1 == season) %>%
#     dplyr::group_by( body_condition ) %>%
#     dplyr::summarise(nb_bc = dplyr::n()) %>%
#     dplyr::mutate(proportion = nb_bc/sum(nb_bc)) %>%
#     dplyr::select(!nb_bc) %>%
#     dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good', 'fat'))) %>%
#     tidyr::pivot_wider(names_from = body_condition, values_from = proportion) # %>%
#   #dplyr::filter(season1 == season) %>%
#   #dplyr::ungroup() #%>%
#   #dplyr::select(!season1)
#
#
#   data_available <-
#
#     tab_av  %>% # tab_av <- landsc_BC_data
#     dplyr::filter(season != "total") %>%
#     # dplyr::mutate(good = good + fat) %>% # combine fat & good
#     # dplyr::select(!fat) %>%
#     dplyr::rename(season1 = season) %>%
#     tidyr::pivot_longer(cols = c(emaciated, thin, good,fat), names_to= 'body_condition', values_to = "nb_bc") %>%
#     #dplyr::group_by(season1) %>%
#     dplyr::filter(season1 == season) %>%
#     dplyr::mutate(proportion = nb_bc/sum(nb_bc)) %>%
#     dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good', 'fat'))) %>%
#     dplyr::select(!nb_bc) %>%
#     tidyr::pivot_wider(names_from = body_condition, values_from = proportion) %>%
#
#     dplyr::ungroup() %>%
#     dplyr::select(!season1)
#
#
#
#   chi_table<-
#     dplyr::bind_rows(data_use, data_available  ) %>%
#     dplyr::mutate(fat = tidyr::replace_na(fat, 0))
#
#
#   X_test <- chi_table %>%
#     chisq.test()
#
#   result_list <- list(X_test, chi_table )
#
#   return(result_list)
#
# }

# > PER SPECIES ####

#>> Data preparation

#
# get_data_use_per_sp <- function(data_use,
#                                 save = FALSE){
#
#
#
#   data_use_per_sp <-
#
#     data_use %>% # data_use = bc_ls_data
#     #rename season1 into 'season'
#     dplyr::rename(season = season1) %>%
#     dplyr::group_by(season, carcass_species, body_condition  ) %>% ### !
#     dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good')),
#                   season = factor(season, levels = c('productive', 'lean'))) %>%
#     #compute the number of individual prey within each category per season and per body condition
#     dplyr::summarise(nb_bc = dplyr::n()) %>%
#     #Compute proportion per season ( must equate 100% within each season)
#     dplyr::mutate(proportion = nb_bc/sum(nb_bc)) %>%
#     dplyr::mutate(prey = 'used') %>%
#     dplyr::ungroup()
#
#   #dplyr::select(!nb_bc)%>%
#
#
#   #To save table
#   if(save == TRUE){
#     readr::write_csv2(data_use_per_sp, file =here::here("output", "data_analyses", "data_use_per_sp.csv"))
#   }
#
#   return(data_use_per_sp)
#
# }
#
#
#
# # body condition of prey available in the landscape with details per species
#
# get_data_av_per_sp <- function(data_av,
#                                save = FALSE){
#
#
#
#   data_use_av_sp <-
#
#     data_av %>% # data_av = landsc_BC_data_sp
#     # Combined fat and good body condition
#     dplyr::mutate(good = good + fat) %>%
#     #remove the 'fat' body condition column/category (not necessary anymore)
#     dplyr::select(!fat) %>%
#     #rename season into 'season1'
#     #dplyr::rename(season1 = season) %>%
#     # Get a table with season, body condition and nb of individual in each category
#     tidyr::pivot_longer(cols = c(emaciated, thin, good), names_to= 'body_condition', values_to = "nb_bc") %>%
#     dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good')),
#                   season = factor(season, levels = c('productive', 'lean'))) %>%
#     # Compute the number of individual prey within each category per season and per body condition
#     dplyr::group_by(carcass_species, season) %>%
#     dplyr::mutate(proportion = nb_bc/sum(nb_bc)) %>%
#     dplyr::mutate(prey = 'available') %>%
#     dplyr::ungroup()
#
#
#   #To save table
#   if(save == TRUE){
#     readr::write_csv2(data_use_av_sp, file =here::here("output", "data_analyses", "data_use_av_sp.csv"))
#   }
#
#
#   return(data_use_av_sp)
#
# }

