#########################################################################################################################

###########################         Data Exploration                                             ######################

########################################################################################################################

# I Within sample variation (dry mass) ####

#View(fat_data)


#get coefficient of variation per 'samples_ID'

get_CV_dry_mass_table <- function(clean_fat_data){

  CV_dry_mass_table <-

  clean_fat_data %>%
# group by sample ID to get summary stat per sample_ID
  dplyr::group_by(samples_ID) %>%
  dplyr::summarise(nb_sub_samples = n(), # number of replicate for each carcass samples
                   mean_dry_mass = mean(sample_dry_mass),# mean dry mass for each sample
                   sd_dry_mass = sd(sample_dry_mass)) %>%# standard deviation for each sample
  dplyr::mutate(CV_dry_mass = sd_dry_mass/mean_dry_mass) # coefficient of variation = sd/mean

#CV < 1 i.e. SD < mean --> relatively low variability ?

  #save table
readr::write_csv2(CV_dry_mass_table, file =here::here("output", "data_exploration", "CV_dry_mass_table.csv"))

}


# graphical representation



get_dry_mass_boxplot <- function(clean_fat_data){


  # Set colors scale
  lbls <- c('early', 'intermediate','late')
  vec_color <- c( "#1f78b4", "#f6c200", "#a50f15")# Yolan color code : blue= <6, Yellow = between 6 and 8 days , red = > 8 days


sample_dry_mass_boxplot <-

  clean_fat_data %>%
  ggplot2::ggplot(aes(x = samples_ID, y= sample_dry_mass, fill = collection_interval)) +
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

ggsave(sample_dry_mass_boxplot, file =here::here("output", "data_exploration", "sample_dry_mass_boxplot.jpg"), device = "jpg")

}

# II "Gold standard" data ####
# adult and indiv only, bones collected before 6 days


# clean_fat_data %>%
#   # selection of row for adult and indiv only & bones collected before 6 days
#   dplyr::filter(collection_interval == 'early',
#                 age == c('old', 'adult'))




