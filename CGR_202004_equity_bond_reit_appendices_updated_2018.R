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

# Figure_2 <- ggplot(filter(Div_world_mean_long, World_mean == 'World_mean_all'), 
#                    aes(Year, Diversification_Index)) +
#   geom_point() +
#   geom_line() +
#   geom_smooth(method = 'lm') +
#   theme_bw()
# 
x_reg <- Div_world_mean_wide$Year
y_reg <- Div_world_mean_wide$World_mean_all
Figure_2 <- plot(x = x_reg,
                 y = y_reg,
                 type = 'l',
                 xlab = 'Years', ylab = 'Average Diversification')
abline(lm(y_reg ~ x_reg), lty = 3)
grid()

###############################################
########## TABLE 3 ############################
###############################################


Div_ind_full_long_all <- Div_ind_full_long_equity %>%
  dplyr::full_join(., Div_ind_full_long_bond, by = c('Year', 'Country')) %>%
  dplyr::full_join(., Div_ind_full_long_reit, by = c('Year', 'Country'))

### Nesting by country ###

nest_div_all <- Div_ind_full_long_all %>%
  dplyr::group_by(Country) %>%
  tidyr::nest() %>%
  dplyr::arrange(Country)

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


################# TABLE 7 ALL ASSETS OLS #####################

div_all_ols_data <- nest_div_all %>%
  dplyr::select(Country, World_mean_all) %>%
  tidyr::unnest(World_mean_all)

panel_year_all <- rep(year_min:year_max, length(unique(div_all_ols_data$Country)))

panel_ols <- div_all_ols_data %>%
  tibble::add_column('Year' = panel_year_all) %>%
  dplyr::select(Country, Year, everything()) %>%
  dplyr::rename('Div' = World_mean_all)

RHS_common_ols <- panel_common_equity %>%
  dplyr::filter(Country == 'Australia') %>%
  dplyr::select(-c(Country, Div_Index)) 

panel_ols <- panel_ols %>%
  dplyr::left_join(., RHS_common_ols, by = 'Year')

summary_panel_ols <- plm::plm(data = panel_ols, model = 'pooling',
                              formula = form_common) %>%
  summary()

###############################################################
################### PRINT TABLE 3 #############################
###############################################################

# Pre and post 2000

nest_div_all_pre_post <- nest_div_all %>%
  dplyr::select(Country, data) %>%
  dplyr::mutate('data_pre' = purrr::map(data, func_pre00),
                'data_post' = purrr::map(data, func_post00),
                'world_mean_pre' = purrr::map(data_pre, func_rowmean),
                'world_mean_post' = purrr::map(data_post, func_rowmean),
                'data_trend_pre' = purrr::map2(data_pre, world_mean_pre, func_data_trend),
                'data_trend_post' = purrr::map2(data_post, world_mean_post, func_data_trend)) %>%
  dplyr::select(-data)


func_missing <- function(df)
{
  div_vec <- df$Div_Index
  sum_missing <- sum(is.na(div_vec) | is.nan(div_vec) | is.infinite(div_vec))
  
  return(sum_missing/length(div_vec))
}

nest_div_all_trend_pre_post <- nest_div_all_pre_post %>%
  dplyr::select(Country, data_trend_pre, data_trend_post) %>%
  dplyr::mutate('div_frac_NA_pre' = map_dbl(data_trend_pre, func_missing),
                'div_frac_NA_post' = map_dbl(data_trend_post, func_missing)) %>%
  dplyr::filter(div_frac_NA_pre < 0.9 & div_frac_NA_post < 0.9) %>%
  dplyr::mutate('summary_trend_pre' = purrr::map(data_trend_pre, func_div_trend_NW),
                'summary_trend_post' = purrr::map(data_trend_post, func_div_trend_NW))

print_trend_all_pre_post <- nest_div_all_trend_pre_post %>%
  dplyr::select(Country, summary_trend_pre, summary_trend_post) %>%
  dplyr::mutate('print_trend_pre' = purrr::map(summary_trend_pre, func_trend_print),
                'print_trend_post' = purrr::map(summary_trend_post, func_trend_print))


div_all_trend_print_pre <- print_trend_all_pre_post$print_trend_pre
names(div_all_trend_print_pre) <- print_trend_all_pre_post$Country

div_all_trend_print_post <- print_trend_all_pre_post$print_trend_post
names(div_all_trend_print_post) <- print_trend_all_pre_post$Country

############## PRINTING OUTPUTS ##########################

# Full
print_trend_div_all <- dplyr::bind_rows(div_all_trend_print) %>% t(.)

# Pre 2000
print_trend_div_all_pre <- dplyr::bind_rows(div_all_trend_print_pre) %>% t(.)

# Post 2000
print_trend_div_all_post <- dplyr::bind_rows(div_all_trend_print_post) %>% t(.)

##########################################################

########## Figure 4 computation ##########################


### Equity

temp_pre_equity <- nest_year_pre_cohort_regress_equity %>%
  dplyr::select(Year, LHS_pre_cohort)
temp_ord_equity <- nest_year_regression_equity %>%
  dplyr::select(Year, LHS_country_ordinary)

nest_year_return_equity <- temp_pre_equity %>%
  dplyr::full_join(., temp_ord_equity, by = 'Year')

### Compute column-wise volatility

func_vol_df <- function(df)
{
  if (is.null(df) == T)
  {
    return(0)
  } else
  {
    func_sd_vec <- function(vec)
    {
      sd_vec <- sd(vec, na.rm = T)
      return(sd_vec)
    }
    
    vol_df <- apply(df, 2, func_sd_vec)
    return(vol_df)
  }
}

nest_year_return_equity <- nest_year_return_equity %>%
  dplyr::mutate('vol_pre_cohort' = purrr::map(LHS_pre_cohort, func_vol_df),
                'vol_ord' = purrr::map(LHS_country_ordinary, func_vol_df),
                'vol_mean_pre' = purrr::map_dbl(vol_pre_cohort, func_mean_vec),
                'vol_mean_ord' = purrr::map_dbl(vol_ord, func_mean_vec))

func_mean_vec_2 <- function(vec_1, vec_2)
{
  df <- cbind(vec_1, vec_2)
  return(apply(df, 1, func_mean_vec))
}

vol_mean_equity <- nest_year_return_equity %>%
  dplyr::select(Year, vol_mean_pre, vol_mean_ord) %>%
  dplyr::mutate('vol_mean_full' = purrr::map2_dbl(vol_mean_pre, vol_mean_ord,
                                                  func_mean_vec_2))

### Bond

temp_pre_bond <- nest_year_pre_cohort_regress_bond %>%
  dplyr::select(Year, LHS_pre_cohort)
temp_ord_bond <- nest_year_regression_bond %>%
  dplyr::select(Year, LHS_country_ordinary)

nest_year_return_bond <- temp_pre_bond %>%
  dplyr::full_join(., temp_ord_bond, by = 'Year')

nest_year_return_bond <- nest_year_return_bond %>%
  dplyr::mutate('vol_pre_cohort' = purrr::map(LHS_pre_cohort, func_vol_df),
                'vol_ord' = purrr::map(LHS_country_ordinary, func_vol_df),
                'vol_mean_pre' = purrr::map_dbl(vol_pre_cohort, func_mean_vec),
                'vol_mean_ord' = purrr::map_dbl(vol_ord, func_mean_vec))

vol_mean_bond <- nest_year_return_bond %>%
  dplyr::select(Year, vol_mean_pre, vol_mean_ord) %>%
  dplyr::mutate('vol_mean_full' = purrr::map2_dbl(vol_mean_pre, vol_mean_ord,
                                                  func_mean_vec_2))

### REIT

temp_pre_reit <- nest_year_pre_cohort_regress_reit %>%
  dplyr::select(Year, LHS_pre_cohort)
temp_ord_reit <- nest_year_regression_reit %>%
  dplyr::select(Year, LHS_country_ordinary)

nest_year_return_reit <- temp_pre_reit %>%
  dplyr::full_join(., temp_ord_reit, by = 'Year')

nest_year_return_reit <- nest_year_return_reit %>%
  dplyr::mutate('vol_pre_cohort' = purrr::map(LHS_pre_cohort, func_vol_df),
                'vol_ord' = purrr::map(LHS_country_ordinary, func_vol_df),
                'vol_mean_pre' = purrr::map_dbl(vol_pre_cohort, func_mean_vec),
                'vol_mean_ord' = purrr::map_dbl(vol_ord, func_mean_vec))

vol_mean_reit <- nest_year_return_reit %>%
  dplyr::select(Year, vol_mean_pre, vol_mean_ord) %>%
  dplyr::mutate('vol_mean_full' = purrr::map2_dbl(vol_mean_pre, vol_mean_ord,
                                                  func_mean_vec_2))

###### Data for generating figure 4, 5 ######

# vol_mean_all <- tibble::tibble('Year' = vol_mean_equity$Year,
#                                'vol_mean_equity' = vol_mean_equity$vol_mean_full,
#                                'vol_mean_bond' = vol_mean_bond$vol_mean_full,
#                                'vol_mean_reit' = vol_mean_reit$vol_mean_full)

vol_mean_all <- tibble::tibble('Year' = vol_mean_equity$Year,
                               'vol_mean_equity' = 100*sqrt(252)*(vol_mean_equity$vol_mean_full),
                               'vol_mean_bond' = 100*sqrt(252)*(vol_mean_bond$vol_mean_full),
                               'vol_mean_reit' = 100*sqrt(252)*(vol_mean_reit$vol_mean_full))

### Figure 4 ###

Fig_4_data <- data.frame('Year' = Div_ind_full_wide$Year,
                         'Div_equity' = Div_world_mean_equity,
                         'Div_bond' = Div_world_mean_bond,
                         'Div_reit' = Div_world_mean_reit,
                         'Vol_equity' = vol_mean_all$vol_mean_equity,
                         'Vol_bond' = vol_mean_all$vol_mean_bond,
                         'Vol_reit' = vol_mean_all$vol_mean_reit)
#Equity
Fig_4_equity <- matplot(Fig_4_data$Year, Fig_4_data[, c(2,5)], 
                        xlab = 'Years', ylab = '', 
                        type = 'l')
grid()
#Bond
Fig_4_bond <- matplot(Fig_4_data$Year, Fig_4_data[, c(3,6)], 
                        xlab = 'Years', ylab = '', 
                        type = 'l')
grid()
#REIT
Fig_4_reit <- matplot(Fig_4_data$Year, Fig_4_data[, c(4,7)], 
                        xlab = 'Years', ylab = '', 
                        type = 'l')
grid()

### Figure 5 ###

Fig_5_data <- data.frame('Year' = Div_ind_full_wide$Year,
                         'Div_mean_all' = Div_world_mean_all,
                         'Vol_all' = apply(vol_mean_all[, -1], 1, mean))
Fig_5_plot <- matplot(Fig_5_data$Year, Fig_5_data[, -1], type = 'l', xlab = 'Years', ylab = 'Average Diversification')
grid()