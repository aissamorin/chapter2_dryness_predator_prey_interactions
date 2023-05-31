#########################################################################################################################

###########################           Function(s) to clean and save body_fat_analysis data_set                   ######################

########################################################################################################################


# Clean raw data for fat analyses ####

#' Clean raw data for fat analyses
#'
#' @param fat_data_raw raw data for fat analyses obtain from the read with read_data function
#' @param save whether we want the output to be save or not, default is FALSE
#' @param filename the name we want the dataset to be saved under, default name is 'cleaned_fat_data'
#'
#' @return cleaned fat data and eventually saved the dataset
#' @export

clean_raw_fat_data <- function(fat_data_raw,
                               save = FALSE,
                               filename = cleaned_fat_data) {

 fat_data_raw %>%
  # remove all 'mass_j1, 'mass_j2' etc... columns '\\d+$' = any number suits
  dplyr::select(!matches("mass_j\\d+$")) %>%
  #remove columns: "samples_marrow_fat_rate", "samples_marrow_fat_rate_without_residuals", "bone_marrow_fat_rate_mean",
  #"bone_marrow_fat_rate_mean_without_residuals","estimated_bc","estimated_bc_without_residuals",  "comments" and any columns after,
  dplyr::select(samples_ID:sample_dry_mass) %>%
  # Remove all replicated samples tested after J-9, etc... /ex 'Hip_B_046-J12'
  dplyr::filter(!stringr::str_detect(samples_ID, "\\-[:upper:]\\d+$"))  %>%
  #Remove empty lines after 233rd row
  dplyr::filter(!is.na(samples_ID)) %>%
  #Remove Buffalo_01 -->i.e. remove 5 lines --> 258 --> 253 raws
  dplyr::filter(!carcass_ID == 'Buffalo_01',
                predator_species == 'lion')#only select raws with lion data

  if (save == TRUE) {

    readr::write_csv2(fat_data_raw, here::here(paste("output/clean_fat_data/", filename,".csv",  sep ="")))
  }

  fat_data_raw

} #End of function clean_raw_fat_data


# Save cleaned data for fat analyses ####


#' #' Save cleaned data for fat analyses
#' #'
#' #' @param cleaned_data cleaned raw data for fat analyses (after using clean_raw_fat_data function)
#' #' @param filename the name we want the dataset to be saved under
#' #'
#' #' @return save cleaned fat data
#' #' @export
#'
#' save_cleaned_data <- function(cleaned_data,
#'                               filename){
#'
#' readr::write_csv2(cleaned_data, here::here(paste("output/clean_fat_data/", filename,".csv",  sep ="")))
#'
#' }


