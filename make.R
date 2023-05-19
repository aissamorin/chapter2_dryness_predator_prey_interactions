#Make file


# Load functions and deps ?
devtools::load_all()


# Read data fat analyses raw dataset

fat_data_raw <- read_fat_data_raw()

# Clean (and save) raw data

clean_fat_data <- clean_raw_fat_data(fat_data_raw)

# Save clean data

save_cleaned_data(fat_data,
                  'cleaned_fat_data')

# Data exploration

# get Coefficient of variation table
CV_dry_mass_table <- get_CV_dry_mass_table(clean_fat_data)

  # Get dry mass boxplots (for each )
sample_dry_mass_boxplot <- get_dry_mass_boxplot(clean_fat_data)

