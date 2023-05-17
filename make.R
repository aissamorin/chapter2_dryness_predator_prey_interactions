#Make file


# Load functions and deps ?
devtools::load_all()


# Read data fat analyses raw dataset

fat_data_raw <- read_fat_data_raw()

# Clean (and save) raw data
