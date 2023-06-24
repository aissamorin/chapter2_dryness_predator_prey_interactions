####################################################### CHAPTER 2 FIGURES ###################################################


require(ggplot2)

# Table 1 - summary table ####

#' Get data summary table of the number of carcasses for each species, per season
#'
#' @param data_use table with the detail of species, season
#' @param save whether we want to save the table or not, default = T, saved in 'figure' file
#'
#' @return return a table with total number of carcasses and the number of carcasses per season, and the totals
#' @export

get_summary_table <- function(data_use,
                              save = T){



summary_table <-

  data_use %>% # tab = bc_ls_data
  #group by the variable for which to count the number of carcasses/sample size must be counted
  dplyr::group_by(carcass_species, season1) %>%
  # count
  dplyr::summarise(dplyr::n()) %>% # to count the number of carcasses which are coded with a distinct sample_ID
  # rename the count column
  dplyr::rename(count = `dplyr::n()`) %>%
  tidyr::pivot_wider(names_from = c(season1), values_from = count) %>%
  dplyr::mutate(Total= sum(lean, productive, na.rm = T)) %>%
  dplyr::relocate(Total, .before = lean) %>%
  dplyr::rename('Species' = carcass_species,
                'Lean season' = lean,
                'Productive season' = productive)



totals <- c("Total",
          sum(summary_table$Total),
          sum(summary_table$`Lean season`, na.rm = T),
          sum(summary_table$`Productive season`, na.rm= T))

totals <- as.data.frame(totals)
totals$Names = c('Species', 'Total', 'Lean season', 'Productive season')


totals %<>%
  tibble::tibble() %>%
  tidyr::pivot_wider(names_from = Names, values_from = totals) %>%
  dplyr::mutate(Total = as.numeric(Total) ,
                `Lean season`= as.numeric(`Lean season`),
                `Productive season` = as.numeric(`Productive season`) )


summary_table %<>%
  dplyr::bind_rows(., totals) # add raw with column totals

#save
if(save == TRUE){

  readr::write_csv2(summary_table, file =here::here("output", "figures", "summary_table.csv"))

}


return(summary_table)

}


# FIGURE 1 : Maps ####

# [FIGURE 2 : Schema introduction ] ####


# BODY_CONDITION ####

# FIGURE 3 : Body condition of prey used vs available as a function of season - All species ####


#' Get barplot figure of proportion of used and available prey in each body condition category per season, for all species or for a given species
#'
#' @param data_tab table with number of individual in each body condition catagory
#' @param save whether we want to save the figure or not (in 'figure' directory)
#' @param species if NULL barplot is draw for all species, if given a species, barplot is draw for this species
#'
#' @return return a barplot of proportion of used and available prey in each body condition category per season
#' @export

get_fig_barplot <-function(data_tab,
                           save = T,
                           species = NULL){



# Data preparation : table with data for figure 3

  if(!is.null(species)){data_tab %<>% dplyr::filter(carcass_species == species )}# get barplot for a given species

  fig_table <-

  data_tab %>% # data_tab = data_all_sp, data_tab = data_per_sp
  dplyr::select(season, body_condition, nb_bc, prey ) %>%
  dplyr::mutate(prey = factor(prey, levels = c('used', 'available')))

#compute sample size
 sample_size <-

    fig_table %>%
    dplyr::group_by(prey,season) %>%
    dplyr::summarise(N= sum(nb_bc)) %>%
   dplyr::mutate(fct_order = dplyr::if_else(prey == 'used', 1,2))



#add sample size to fig_table and create new label
   fig_table %<>%
   dplyr::left_join(., sample_size, by = c("prey","season")) %>%
   transform(., labelx = paste(prey, "\n", 'N = ', N)) %>%
   dplyr::mutate(labelx = forcats::fct(labelx)) %>%
   dplyr::mutate(labelx = forcats::fct_reorder(labelx, fct_order))# for re-ordering the new label and have used column plotted first






# Figure

# color scale

lbls <- c('emaciated', 'thin','good')
vec_color <- c( "#a50f15", "#ebc174", "#79ad41")

# New facet label
season.labs <- c("Productive season", "Lean season")
names(season.labs) <- c("productive", "lean")


barplot <-

  fig_table %>%

  ggplot2::ggplot(aes(fill = body_condition, y = nb_bc, x = labelx))+
  geom_bar(position='fill', stat="identity")+

  #Facetting
  facet_grid(~season,
             scales = 'free', # remove empty factors (scales = 'free')
             labeller = labeller(season = season.labs) )+ #change/assign new facet label names
  #Modifying plot theme
  theme_minimal()+
  theme(strip.text = element_text(face="bold", size = 16),
        panel.border = element_rect(colour = '#5d6174', fill = NA, linewidth = 0.5),
        legend.position = "bottom",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(10,10,10,10)),
        axis.title.y = element_text(margin = margin(10,10,10,10)),
        axis.text = element_text(color ='black',
                                  size = 13.5 ),
        axis.text.x = element_text(size = 15))+

  # Legends + assign new color
  scale_fill_manual( name = "Body condition :",
                     labels = c('Emaciated', 'Thin', 'Good', 'Fat'),
                     values = setNames(vec_color, lbls)) +
  labs(x = "",
       y = "Proportions")






if (save == TRUE & is.null(species)){

  ggsave(barplot, file =here::here("output", "figures", "barplot_all_sp.jpg"), device = "jpg")

} else if (save == TRUE & !is.null(species)){

  ggsave(barplot, file =here::here(paste("output/", "figures/", "barplot_",species,".jpg", sep = '')), device = "jpg")}


return(barplot)

}


# FIGURE 4 : Jacob indices - All species ####


#' Get the graphical representation of Jacob's indices per season
#'
#' @param ratio_table table with Jacob's indices computed across all species or for a given species
#' @param save whether we want to save the figure
#' @param species if we compute the figure for a given species, must be indicated
#'
#' @return a figure corresponding to a graphical representation of Jacob's indices
#' @export

get_fig_ratio <- function(ratio_table,
                          save = F,
                          species = NULL){




#set color scales
#a faire : changer facet color background + reflechir aux couleurs

lbls <- c('YES', 'NO')
vec_color <- c( "#931e18", "#247d3f")

facet_col <- data.frame(
  season = c( "productive","lean"),
  var_color = c("#79ad41", "#ebc174" ) )

facet_col %<>%
   dplyr::mutate(season = forcats::fct(season,levels = c("productive","lean")))

# New facet labels
season.labs <- c("Productive season", "Lean season")
names(season.labs) <- c("productive", "lean")



# --- #



ratio_table %<>% # ratio_table = ratio_table_all_sp
  dplyr::mutate(fill_col = dplyr::if_else(Jacob_Index <= -0.5 |Jacob_Index >= 0.5,
                                          'YES',
                                          'NO'))
 # dplyr::mutate(season = forcats::fct_relevel(season,levels = c("productive","lean"))) %>%


gp <-

ggplot(ratio_table)+

  #get coloured background with colour depending on seasons
  geom_rect(data = facet_col,
            aes(xmin=-Inf, xmax=Inf,
                ymin=-1, ymax=1, fill= season), alpha= 0.2) +

  geom_point(aes(x = body_condition, y = Jacob_Index, col = fill_col), size = 6, shape = 18) +

  geom_hline(yintercept = 0.5, linetype="dashed", col = '#931e18')+
  geom_hline(yintercept = - 0.5, linetype="dashed", col = '#931e18')+
  geom_hline(yintercept = - 0, linewidth =1.05,  col = 'black')+
  geom_hline(yintercept = - 1, linewidth =0.5,  col = 'black')+
  geom_hline(yintercept =  1, linewidth =0.5,  col = 'black')+

  #facetting
  facet_grid(~season,
             labeller = labeller(season = season.labs)) + #change/assign new facet label names) +

  #modified plot theme
  theme_minimal()+
  theme(strip.text = element_text(face="bold", size = 16),
        #panel.grid.major.x = element_line(colour = 'black', linetype = "dotted"),
        legend.position = "bottom",
        legend.title = element_text(size = 15),
        legend.text = element_text(size = 15),
        axis.title = element_text(size = 15),
        axis.title.x = element_text(margin = margin(15,10,10,10)),
        axis.title.y = element_text(margin = margin(10,10,10,10)),
        axis.text = element_text(color ='black',
                                 size = 13.5 ),
        axis.text.x = element_text(size = 15),
        axis.ticks.x = element_line(colour = 'black', linewidth = 1))+


  scale_fill_manual(values = c('productive' = "#79ad41",  'lean' = "#ebc174" ),
                    guide = 'none')+ #remove associated legend

  scale_colour_manual(name = '' ,
                      labels = c('Within -0.5 et + 0.5', 'Above 0.5 or below -0.5'),
                      values = setNames(vec_color, lbls)) +

  labs(x = "Body condition",
       y = "Jacob's index values") +
  ylim(-1,1)


if(save == TRUE & is.null(species)){

  ggsave(gp, file =here::here("output", "figures", "fig_ratio_all_sp.jpg"), device = "jpg")

} else if (save == TRUE & !is.null(species)){

      ggsave(gp, file =here::here(paste("output/", "figures/", "fig_ratio_",species,".jpg", sep = '')), device = "jpg")}


return(gp)

}




# APPENDICES ####

#> Appendix 1 ~ Within sample variability Fat rates CV ####

#' Get boxplots of fat rates for each samples (i.e. sample_ID)
#'
#' @param tab fat data cleaned with 'clean_raw_fat_data' function, default is clean_fat_data
#' @param save whether we want the figure to be saved or not
#'
#' @return boxtplots of fat rates as a function of/for each sample_ID
#' @export

get_figA1a_fat_rate_cv <- function(tab = clean_fat_data,
                                 save =  TRUE){


  # Set colors scale
  lbls <- c('early', 'intermediate','late')
  vec_color <- c( "#1f78b4", "#f6c200", "#a50f15")# Yolan color code : blue= <6, Yellow = between 6 and 8 days , red = > 8 days


  fat_rate_CV_boxplot <-

    tab %>% # tab = clean_fat_data
    # computes fate rate for each repclicate (--> will give several values for each sample ID )
    dplyr::mutate(sample_replicate_fat_rate = sample_dry_mass/sample_wet_mass) %>%

    #ggplot boxplot
    ggplot(aes(x = samples_ID, y= sample_replicate_fat_rate, fill = collection_interval)) +
    geom_boxplot()+
    theme_bw()+
    theme(legend.position = "bottom",
          axis.text.x = (element_text(color ='black',
                                      angle = 45,
                                      hjust = 1,
                                      size = 6)),
          legend.text = element_text(size = 14),
          legend.title = element_text(size = 14),
          axis.title = element_text(size = 14),
          axis.title.x = element_text(margin = margin(10,10,10,10)),
          axis.title.y = element_text(margin = margin(10,10,10,10)),
          axis.text = element_text(color ='black',
                                   size = 14 ))+
    scale_fill_manual( name = "Collection date :",
                       labels = c('< 6 days', 'between 6 and 8 days', '>8 days'),
                       values = setNames(vec_color, lbls)) +
    labs(x = "Samples ID",
         y = "Replicate fat rate")



  #To save figure
  if(save == TRUE){
    ggsave(fat_rate_CV_boxplot, file =here::here("output", "figures", "appendices","A1a_fat_rate_CV_boxplot.jpg"), device = "jpg")
  }

  # return
  return(fat_rate_CV_boxplot)

}


# Histogramme ####

#histogram

#' Get an histogram of fat rate coefficients of variation
#'
#' @param tab table with fat rate coefficients of variation
#' @param save whether we want the figure to be saved or not, default = F
#'
#' @return a histogram of fat rate coeeficient of variation
#' @export

get_figA1b_CV_hist <- function(tab,
                                save = F){


  fat_rate_CV_hist <-
    tab %>% # tab <- CV_fat_rate_table

    ggplot(aes(CV_fat_rate))+
    geom_histogram(fill = '#ab3329', color = '#65150b')+
    geom_vline(xintercept = 0.25, color = '#ab3329', linetype = 'dashed' )+
    theme_bw()+
    theme(legend.position = "bottom",
          legend.text = element_text(size = 13),
          axis.text.x = (element_text(color ='black')),
          axis.text = element_text(color ='black',
                                   size = 13 ),
          axis.title = element_text(size = 14),
          axis.title.x = element_text(margin = margin(10,10,10,10)),
          axis.title.y = element_text(margin = margin(10,10,10,10)))+
    labs(x = "Sample fat rate coefficient of variation",
         y = "Number of carcasses")

  #To save figure
  if(save == TRUE){
    ggsave(fat_rate_CV_hist, file =here::here("output", "figures","appendices", "A1b_fat_rate_CV_hist.jpg"), device = "jpg")
  }


  return(fat_rate_CV_hist)

}



#' Get a simplified table of samples with high coefficient of variation
#'
#' @param tab table with the samples with high coefficient of variation
#' @param save wether we want to save the table or not, saved in figure/appendices/
#'
#' @return a table with as many row as the number of samples with high CV
#' @export

get_high_CV_table_simplified <- function(tab = high_CV_table,
                              save = F){

high_CV_table_simplified <-

  tab %>% # tab = high_CV_table
  unique() %>%
  dplyr::select(samples_ID,
                carcass_species,
                nb_sub_samples,mean_fat_rate,
                sd_fat_rate,
                CV_fat_rate) %>%
  dplyr::rename(replicate_number = nb_sub_samples) %>%
  dplyr::mutate(mean_fat_rate = round(mean_fat_rate, digits = 3),
                sd_fat_rate = round(sd_fat_rate, digits = 3),
                CV_fat_rate =  round(CV_fat_rate, digits = 3))


if(save == TRUE){

  readr::write_csv2(high_CV_table_simplified, file =here::here("output", "figures",'appendices', "high_CV_table_simplified.csv"))

}


return(high_CV_table_simplified)

}


#> Appendix 2 ~ MFR ~Bone freshness ####

#>> Fat rate ~Bone freshness ####

#' Get boxplots comparing replicate fat rates as function of bone 'freshness', facetted per carcass_ID
#'
#' @param tab table with both sample tested at normal time, and tested according different waiting times
#' @param save whether we want to save the figure or not, default = F
#'
#' @return a boxplot comparing replicate fat rates as function of bone 'freshness', facetted per carcass_ID
#' @export

get_figA2_fat_rate_bone_fresh <- function(tab,
                                   save = FALSE){



  # create facet labels (add species name to carcass_ID)
   tab_label <-

    tab %>% # tab = fat_rate_evol_data
    dplyr::select(carcass_ID, carcass_species) %>%
    tidyr::unite(label, carcass_ID, carcass_species, sep ="\n \n ", remove = FALSE) %>%
    dplyr::distinct()


  carcass.labs <- tab_label$label
  names(carcass.labs) <- tab_label$carcass_ID


  #setting color scale
  lbls <- c('D_<6', 'D8', 'D9', 'D12')
  vec_color <- c("#a40000","#16317d","#007e2f","#ffcd12")




  bp <-

  tab %>% # tab = fat_rate_evol_data
    ggplot2::ggplot(aes(x = D , y = sample_replicate_fat_rate  , colour = D, fill = D)) +
    geom_boxplot() +
    facet_grid(~carcass_ID,
               labeller = labeller(carcass_ID = carcass.labs)) + # change facet labels

    #modify plot theme
    theme_bw()+
    theme(strip.text = element_text(face="bold", size = 16),
          strip.background = element_rect( colour = 'grey'),
          legend.position = "bottom",
          legend.title = element_text(size = 15),
          legend.text = element_text(size = 15),
          axis.title = element_text(size = 15),
          axis.title.x = element_text(margin = margin(15,10,10,10)),
          axis.title.y = element_text(margin = margin(10,10,10,10)),
          axis.text = element_text(color ='black',
                                   size = 13.5 ),
          axis.text.x = element_text(size = 15))+

    # Set legends / colours
    scale_colour_manual( name = "Bone freshness :",
                       #labels = c('Emaciated', 'Thin', 'Good'),
                       values = setNames(vec_color, lbls)) +
    scale_fill_manual( name = "Bone freshness :",
                       #labels = c('Emaciated', 'Thin', 'Good'),
                       values = setNames(vec_color, lbls)) +
    labs(x = "Bone freshness",
         y = 'Replicate fat rate')


  if(save == TRUE){

    ggsave(bp, file =here::here("output", "figures","appendices", "A2_fat_rate_bone_fresh.jpg"), device = "jpg")

  }

  return(bp)

}


#################################### DRAFT ###################################################

# dplyr::group_by(body_condition, season1  ) %>%
#   dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good', 'fat'))) %>%
#   dplyr::summarise(nb_bc = dplyr::n()) %>%
#   dplyr::mutate(percentage = nb_bc/sum(nb_bc)*100) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(prey = "used")


# dplyr::group_by(season1) %>%
# dplyr::mutate(percentage = nb_bc/sum(nb_bc)*100) %>%
# dplyr::ungroup() %>%
# dplyr::mutate(prey = "available")



# >> 'used' body condition <=> body condition of prey killed by lions ####

# data_use_fig <-
#
# bc_ls_data %>% # tab1 = bc_ls_data
#   dplyr::group_by(season1, body_condition) %>%
#   dplyr::summarise(nb_bc = dplyr::n()) %>%
#   dplyr::mutate(proportion = nb_bc/sum(nb_bc)) %>%
#   dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good', 'fat'))) %>%
#   dplyr::mutate(prey = "used")
#
#
# # >> 'available' body condition <=> body condition of prey/herbivores observable in the landscape ####
#
# data_available_fig <-
#
#   tab  %>% # tab <- landsc_BC_data
#   dplyr::filter(season != "total") %>%
#   dplyr::rename(season1 = season) %>%
#   tidyr::pivot_longer(cols = c(emaciated, thin, good, fat), names_to= 'body_condition', values_to = "nb_bc") %>%
#    dplyr::mutate(body_condition = factor(body_condition, levels = c('emaciated', 'thin', 'good', 'fat'))) %>%
#   dplyr::mutate(prey = "available")

# Figure 5 - Body_condition ~season - per species ####

# get_fig_5 <- function(data_tab,
#                       save =  F,
#                       species = NULL){
#
#   fig_table <-
#
#     data_tab %>% # data_tab = data_per_sp
#     #if species
#     dplyr::filter(carcass_species == 'buffalo') %>%
#
#     # else
#     dplyr::select(season, body_condition, nb_bc, prey ) %>%
#     dplyr::mutate(prey = factor(prey, levels = c('used', 'available')))
#
#
#   # Figure
#
#   # color scale
#
#   lbls <- c('emaciated', 'thin','good', 'fat')
#   vec_color <- c( "#a50f15", "#ebc174", "#79ad41", "#1a472a")
#
#
#   fig5 <-
#
#     fig_table %>%
#     ggplot2::ggplot(aes(fill = body_condition, y = nb_bc, x = prey))+
#     geom_bar(position='fill', stat="identity")+
#     facet_wrap(~season)+
#     theme_bw()+
#     theme(legend.position = "bottom",
#           axis.text.x = (element_text(color ='black')))+
#     scale_fill_manual( name = "Body condition :",
#                        labels = c('Emaciated', 'Thin', 'Good', 'Fat'),
#                        values = setNames(vec_color, lbls)) +
#     labs(x = "",
#          y = "Proportions")
#
#
#   if(save == TRUE){
#
#     ggsave(fig5, file =here::here("output", "figures", "fig5_BC_used_av_season_per_sp.jpg"), device = "jpg")
#
#   }
#
#
#   return(fig5)
#
# }



