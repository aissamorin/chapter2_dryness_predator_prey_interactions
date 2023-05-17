#########################################################################################################################

###########################           Function(s) to clean and save body_fat_analysis data_set                   ######################

########################################################################################################################


# Clean raw data for fat analyses

#' Clean raw data for fat analyses
#'
#' @param fat_data_raw # raw data for fat analyses obtain from the read with read_data function
#'
#' @return
#' @export

clean_raw_fat_data <- function(fat_data_raw) {

 fat_data_raw %>%
  # remove all 'mass_j1, 'mass_j2' etc... columns '\\d+$' = any number suits
  dplyr::select(!matches("mass_j\\d+$")) %>%
  #remove columns: "samples_marrow_fat_rate", "samples_marrow_fat_rate_without_residuals", "bone_marrow_fat_rate_mean",
  #"bone_marrow_fat_rate_mean_without_residuals","estimated_bc","estimated_bc_without_residuals",  "comments" and any columns after,
  dplyr::select(samples_ID:sample_dry_mass) %>%
  #Remove empty lines after 258th line
  dplyr::filter(!is.na(samples_ID)) %>%
  #Remove Buffalo_01 -->i.e. remove 5 lines --> 258 --> 253 raws
  dplyr::filter(!carcass_ID == 'Buffalo_01')

} #End of function clean_raw_fat_data



