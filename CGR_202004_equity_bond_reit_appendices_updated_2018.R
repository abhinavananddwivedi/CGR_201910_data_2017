### Appendices and other tables for all asset classes for CGR 202003 with updated data till 2018 ###

library(tidyverse)
library(lmtest)
library(sandwich)
library(plm)

### Renaming relevant dataframes to ensure distinction 
### among asset classes (in case they share the same name)

# Equity

name_script_file_equity <- "CGR_202003_equity_results_updated_2018.R"
source(name_script_file_equity, echo = F) 


Div_ind_full_long_equity <- Div_ind_full_long
Div_ind_full_wide_equity <- Div_ind_full_wide
nest_panel_common_equity <- nest_panel_common
nest_panel_common_ols_equity <- nest_panel_common_ols
nest_panel_dev_indicators_equity <- nest_panel_dev_indicators
nest_panel_pre_post_equity <- nest_panel_pre_post
nest_year_regression_equity <- nest_year_regression
nest_year_pre_cohort_regress_equity <- nest_year_pre_cohort_regress
panel_common_equity <- panel_common_2
panel_common_idio_equity <- panel_common_idio
panel_dev_indicators_equity <- panel_dev_indicators



# Bond

name_script_file_bond <- "CGR_202003_bond_results_updated_2018.R"
source(name_script_file_bond, echo = F) 


Div_ind_full_long_bond <- Div_ind_full_long
Div_ind_full_wide_bond <- Div_ind_full_wide
nest_panel_common_bond <- nest_panel_common
nest_panel_common_ols_bond <- nest_panel_common_ols
nest_panel_dev_indicators_bond <- nest_panel_dev_indicators
nest_panel_pre_post_bond <- nest_panel_pre_post
nest_year_regression_bond <- nest_year_regression
nest_year_pre_cohort_regress_bond <- nest_year_pre_cohort_regress
panel_common_bond <- panel_common_2
panel_common_idio_bond <- panel_common_idio
panel_dev_indicators_bond <- panel_dev_indicators


# REIT

name_script_file_reit <- "CGR_202003_reit_results_updated_2018.R"
source(name_script_file_reit, echo = F) 


Div_ind_full_long_reit <- Div_ind_full_long
Div_ind_full_wide_reit <- Div_ind_full_wide
nest_panel_common_reit <- nest_panel_common
nest_panel_common_ols_reit <- nest_panel_common_ols
nest_panel_dev_indicators_reit <- nest_panel_dev_indicators
nest_panel_pre_post_reit <- nest_panel_pre_post
nest_year_regression_reit <- nest_year_regression
nest_year_pre_cohort_regress_reit <- nest_year_pre_cohort_regress
panel_common_reit <- panel_common_2
panel_common_idio_reit <- panel_common_idio
panel_dev_indicators_reit <- panel_dev_indicators


#### Results and analysis starts from here ####

func_mean_vec <- function(vec){return(mean(vec, na.rm = T))}

###############################################
############# Figure 4 ########################
###############################################

# In long format
Div_ind_full_long_equity <- Div_ind_full_long_equity %>%
  dplyr::rename('Div_Index_Equity' = Div_Index)
Div_ind_full_long_bond <- Div_ind_full_long_bond %>%
  dplyr::rename('Div_Index_Bond' = Div_Index)
Div_ind_full_long_reit <- Div_ind_full_long_reit %>%
  dplyr::rename('Div_Index_REIT' = Div_Index)


# In wide format
Div_ind_full_wide_equity <- Div_ind_full_long_equity %>%
  tidyr::spread(., key = 'Country', value = 'Div_Index_Equity')
Div_ind_full_wide_bond <- Div_ind_full_long_bond %>%
  tidyr::spread(., key = 'Country', value = 'Div_Index_Bond')
Div_ind_full_wide_reit <- Div_ind_full_long_reit %>%
  tidyr::spread(., key = 'Country', value = 'Div_Index_REIT')


Div_world_mean_equity <- apply(Div_ind_full_wide_equity[, -1], 1, func_mean_vec)
Div_world_mean_bond <- apply(Div_ind_full_wide_bond[, -1], 1, func_mean_vec)
Div_world_mean_reit <- apply(Div_ind_full_wide_reit[, -1], 1, func_mean_vec)

Div_world_mean_wide <- Div_ind_full_wide_equity %>%
  dplyr::select(Year) %>%
  tibble::add_column('World_mean_equity' = Div_world_mean_equity,
                     'World_mean_bond' = Div_world_mean_bond,
                     'World_mean_reit' = Div_world_mean_reit) 

Div_world_mean_all <- apply(Div_world_mean_wide[, -1], 1, func_mean_vec)

Div_world_mean_wide <- Div_world_mean_wide %>%
  tibble::add_column('World_mean_all' = Div_world_mean_all)

Div_world_mean_long <- Div_world_mean_wide %>%
  tidyr::gather(-Year, key = 'World_mean', value = 'Diversification_Index')

Div_world_mean_plot <- ggplot(Div_world_mean_long, aes(Year, Diversification_Index)) +
  geom_point() +
  geom_line(mapping = aes(linetype = World_mean)) +
  theme_bw()

Figure_2 <- ggplot(filter(Div_world_mean_long, World_mean == 'World_mean_all'), 
                   aes(Year, Diversification_Index)) +
  geom_point() +
  geom_line() +
  geom_smooth(method = 'lm') +
  theme_bw()

###############################################
########## TABLE 3 ############################
###############################################


Div_ind_full_long_all <- Div_ind_full_long_equity %>%
  dplyr::full_join(., Div_ind_full_long_bond, by = c('Year', 'Country')) %>%
  dplyr::full_join(., Div_ind_full_long_reit, by = c('Year', 'Country'))

### Nesting by country ###

nest_div_all <- Div_ind_full_long_all %>%
  dplyr::group_by(Country) %>%
  tidyr::nest()

func_rowmean <- function(df)
{
  df_2 <- df[, -1]
  rowmean_vec <- apply(df_2, 1, function(vec){return(mean(vec, na.rm = T))})
  return(rowmean_vec)
}

nest_div_all <- nest_div_all %>%
  dplyr::mutate('World_mean_all' = purrr::map(data, func_rowmean))

func_data_trend <- function(df, vec)
{
  year_vec <- df$Year
  
  tib <- tibble::tibble('Year' = year_vec, 'Div_Index' = vec)
  
  return(tib)
}

nest_div_all <- nest_div_all %>%
  dplyr::mutate('data_trend' = purrr::map2(data, World_mean_all, func_data_trend)) %>%
  mutate('trend_NW' = map(data_trend, func_div_trend_NW))

nest_div_all_trend <- nest_div_all %>%
  dplyr::select(Country, trend_NW) %>%
  dplyr::mutate('trend_print' = purrr::map(trend_NW, func_trend_print))

div_all_trend_print <- nest_div_all_trend$trend_print
names(div_all_trend_print) <- nest_div_all_trend$Country
