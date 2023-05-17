######################################################################################################################################

###########################           RESEARCH COMPENDIUM                          ##################################################

######################################################################################################################################


# Create research compendium
rrtools::use_compendium("/Chapter2.Dryness.Predator.Prey.Interactions", open = FALSE)

# Packages - dependencies
usethis::use_package("here")
usethis::use_package("readr")
usethis::use_package("dplyr")
usethis::use_package("magrittr")
usethis::use_package("ggplot2")
usethis::use_package("forcats")
usethis::use_package("stringr")
usethis::use_package("sf")
usethis::use_package("readxl")
usethis::use_package("lubridate")

# To use pipe
usethis::use_pipe()

# To modify description
devtools::install_deps()

#Update namespace with new functions"en-tete"
devtools::document()


# Create R files

usethis::use_r("01_read_dataset")
usethis::use_r("02_clean_dataset")



# Load functions (?)
devtools::load_all()
