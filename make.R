#Make file


# Load functions and deps ?
devtools::load_all()


# Read data fat analyses raw dataset

fat_data_raw <- read_fat_data_raw()

# Clean (and save) raw data

fat_data <- clean_raw_fat_data(fat_data_raw)

# Save clean data
