#########################################################################################################################

###########################           Function(s) to read body_fat_analysis raw data                   ######################

########################################################################################################################

# Function aim at reading raw data (i.e. not cleanned yet) for fat data analyses

#' Read raw data for fat analyses, from Yolan (after inclusion of collection_interval column)
#'
#' @return
#' @export

read_fat_data_raw <- function() {

readr::read_csv2(here::here('data','data_fat_analyses_raw.csv'))

}
