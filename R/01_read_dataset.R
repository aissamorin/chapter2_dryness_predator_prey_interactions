#########################################################################################################################

###########################           Function(s) to read body_fat_analysis raw data                   ######################

########################################################################################################################

# Function aim at reading raw data (i.e. not cleanned yet) for fat data analyses

#' Read raw data for fat analyses, from Yolan (after inclusion of collection_interval column)
#'
#' @return raw fat dataset
#' @export

read_fat_data_raw <- function() {

readr::read_csv2(here::here('data','data_fat_analyses_raw.csv'))

}


# Function aim at reading raw data (i.e. not cleanned yet) for fat data analyses

#' Read updated raw data for fat analyses, from Yolan, sent the 8th June 2023 (after inclusion of collection_interval column and removal of unnacessary raws (see read_me, data preparation))
#'
#' @return updated 8th june 2023 raw fat dataset
#' @export

read_fat_data_raw_2 <- function() {

  readr::read_csv2(here::here('data','data_fat_analyses_08_06_23_raw.csv'))

}




#' Function aims at reading available prey body condition data for comparison with body condition used (i.e. preyed) by lions
#'
#' @return a table with the number of individual in each BC catagories (column) per seasons (raws)
#' @export

read_data_land_BC <- function() {

  readr::read_csv2(here::here('data','data_landscape_BC.csv'))

}
