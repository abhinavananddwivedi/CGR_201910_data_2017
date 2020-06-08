########################################################################################
################## Bull/bear diversification indices calculation #######################
########################################################################################

### REDOING DIVERSIFICATION INDEX CALCULATION ALL OVER AGAIN ###

library(tidyverse)

source('CGR_202001_equity_alt.R', echo = F)


nest_year_bear_bull <- nest_year_return_LHS_RHS %>%
  dplyr::select(Year, LHS_country_valid) %>%
  dplyr::mutate('LHS_clean' = purrr::map(LHS_country_valid, func_NA_med_df))


func_bear_df <- function(df)
{
  func_bear_vec_2 <- function(vec)
  {
    vec_length <- length(vec)
    vec_sort <- sort(vec)
    vec_bear <- vec_sort[1:floor(vec_length/2)]
    return(vec_bear)
  }
  
  bear_df <- apply(df, 2, func_bear_vec_2)
  return(bear_df)
}


func_bull_df <- function(df)
{
  func_bull_vec_2 <- function(vec)
  {
    vec_length <- length(vec)
    vec_sort <- sort(vec)
    vec_bull <- vec_sort[(floor(vec_length/2)+1):vec_length]
    return(vec_bull)
  }
  
  bull_df <- apply(df, 2, func_bull_vec_2)
  return(bull_df)
}

### Separating in bear and bull returns ###
nest_year_bear_bull <- nest_year_bear_bull %>%
  dplyr::mutate('LHS_bear' = purrr::map(LHS_clean, func_bear_df),
                'LHS_bull' = purrr::map(LHS_clean, func_bull_df))

##################################################################
############## ORDINARY COUNTRIES ################################
##################################################################

nest_year_lhs_bear <- nest_year_bear_bull %>%
  dplyr::select(Year, LHS_clean, LHS_bear) %>%
  dplyr::rename('lhs' = LHS_clean, 'lhs_b' = LHS_bear)

func_div_ordinary <- function(nest_data = nest_year_lhs_bear)
{
  nest_year_lhs_rhs <- nest_data %>%
    dplyr::mutate('rhs_country' = purrr::map(lhs_b, func_select_RHS_countries),
                  'lhs_country_ordinary' = purrr::map(lhs_b, func_select_ordinary)) %>%
    dplyr::select(-lhs)
  
  nest_year_lhs_rhs_2 <- nest_year_lhs_rhs %>%
    dplyr::mutate('Cov_matrix' = purrr::map(rhs_country, cov),
                  'Eig_val' = purrr::map(Cov_matrix, function(df){return(eigen(df)$values)}),
                  'Eig_vec' = purrr::map(Cov_matrix, function(df){return(eigen(df)$vectors)}),
                  'Share' = purrr::map(Eig_val, function(vec){return(cumsum(vec)/sum(vec))})) %>%
    dplyr::select(-c(lhs_b))
  
  nest_lag_eig <- dplyr::lag(nest_year_lhs_rhs_2$Eig_vec) 
  
  nest_year_lhs_rhs_2 <- nest_year_lhs_rhs_2 %>%
    tibble::add_column('Lag_eig_vec' = nest_lag_eig)
  
  num_pc_b <- 3
  
  nest_year_lhs_rhs_2 <- nest_year_lhs_rhs_2 %>%
    dplyr::filter(!is.na(Lag_eig_vec)) %>%
    dplyr::filter(Year > 1986) %>%
    dplyr::mutate('PC_out_sample' = purrr::map2(rhs_country, Lag_eig_vec,
                                                function(df1, df2){return(df1%*%df2)}),
                  'PC_out_sample_90' = purrr::map(PC_out_sample,
                                                  function(df){return(df[, 1:num_pc_b])}))
  
  nest_year_b_regression <- nest_year_lhs_rhs_2 %>%
    dplyr::select(Year, lhs_country_ordinary, PC_out_sample_90)
  
  nest_year_b_regression <- nest_year_b_regression %>%
    dplyr::mutate('div_ind_b' = purrr::map2(lhs_country_ordinary, 
                                            PC_out_sample_90,
                                            func_lm_div))
  
  nest_year_b_regression <- nest_year_b_regression %>%
    dplyr::mutate('div_ind_b_2' = purrr::map2(lhs_country_ordinary, div_ind_b, 
                                              func_attach_name))
  
  div_ordinary_long <- nest_year_b_regression %>%
    dplyr::mutate('Country' = purrr::map(div_ind_b_2, func_pick_name)) %>%
    dplyr::select(Country, Year, div_ind_b_2) %>%
    tidyr::unnest(cols = c('Country', 'div_ind_b_2'))
  
  div_ordinary_wide <- div_ordinary_long %>%
    tidyr::spread(key = Country, value = div_ind_b_2) 
  
  return(div_ordinary_wide)
}

# Ordinary bear
Div_ind_ordinary_bear <- func_div_ordinary(nest_data = nest_year_lhs_bear)

nest_year_lhs_bull <- nest_year_bear_bull %>%
  dplyr::select(Year, LHS_clean, LHS_bull) %>%
  dplyr::rename('lhs' = LHS_clean, 'lhs_b' = LHS_bull)

# Ordinary bull
Div_ind_ordinary_bull <- func_div_ordinary(nest_data = nest_year_lhs_bull)

###################################################################
############## PRECOHORT COUNTRIES ################################
###################################################################

func_div_pre_cohort <- function(nest_data = nest_year_lhs_bear)
{
  nest_year_pre_cohort_b <- nest_data %>%
    dplyr::mutate('lhs_pre_cohort' = purrr::map(lhs_b, 
                                                func_select_pre_cohort),
                  'rhs_countries' = purrr::map(lhs_b, func_select_RHS_countries),
                  'rhs_clean' = purrr::map(rhs_countries, func_NA_med_df)) 
  
  nest_year_pre_cohort_b_2 <- nest_year_pre_cohort_b %>%
    dplyr::select(Year, lhs_pre_cohort, rhs_clean) %>%
    dplyr::mutate('rhs_country_j' = purrr::map(rhs_clean, func_rm_col_j)) 
  
  nest_year_pre_cohort_b_2 <- nest_year_pre_cohort_b_2 %>%
    dplyr::mutate('Cov_list_j' = purrr::map(rhs_country_j, 
                                            function(list){return(purrr::map(list, cov))})) %>%
    dplyr::mutate('Eig_val_list_j' = purrr::map(Cov_list_j, func_eig_val_list)) %>%
    dplyr::mutate('Eig_vec_list_j' = purrr::map(Cov_list_j, func_eig_vec_list)) 
  
  nest_lag_eig_list <- dplyr::lag(nest_year_pre_cohort_b_2$Eig_vec_list_j) 
  
  nest_year_pre_cohort_b_2 <- nest_year_pre_cohort_b_2 %>%
    tibble::add_column('Lag_eig_vec_list_j' = nest_lag_eig_list)
  
  nest_year_pre_cohort_b_final <- nest_year_pre_cohort_b_2 %>%
    dplyr::select(Year, lhs_pre_cohort, rhs_country_j, 
                  Eig_vec_list_j, Lag_eig_vec_list_j)
  
  
  nest_year_pre_cohort_b_final_2 <- nest_year_pre_cohort_b_final %>%
    dplyr::filter(!map_lgl(Lag_eig_vec_list_j, function(vec){any(is.na(vec))})) %>%
    dplyr::mutate('PC_list_j' = purrr::map2(rhs_country_j, Lag_eig_vec_list_j,
                                            func_list_multiply)) %>%
    dplyr::select(Year, lhs_pre_cohort, PC_list_j)
  
  num_pc_equity <- 3
  
  
  nest_year_pre_cohort_b_regress <- nest_year_pre_cohort_b_final_2 %>%
    dplyr::mutate('PC_list_j_final' = purrr::map(PC_list_j, func_select_PC_list))
  
  nest_year_pre_cohort_b_regress <- nest_year_pre_cohort_b_regress %>%
    dplyr::filter(Year > 1986) %>%
    dplyr::mutate('div_pre_cohort' = purrr::map2(lhs_pre_cohort, 
                                                 PC_list_j_final,
                                                 func_pre_cohort_regress))
  
  nest_year_pre_cohort_b_regress <- nest_year_pre_cohort_b_regress %>%
    dplyr::mutate('div_pre_cohort_name' = purrr::map(div_pre_cohort, 
                                                     func_attach_name_pre_cohort))
  
  div_pre_cohort_long <- nest_year_pre_cohort_b_regress %>%
    dplyr::mutate('Country' = purrr::map(div_pre_cohort_name, func_pick_name)) %>%
    dplyr::select(Country, Year, div_pre_cohort_name) %>%
    tidyr::unnest(cols = c('Country', div_pre_cohort_name))
  
  div_pre_cohort_wide <- div_pre_cohort_long %>%
    tidyr::spread(key = Country, value = div_pre_cohort_name) 
  
  return(div_pre_cohort_wide)
}


# Pre cohort bear
Div_ind_pre_cohort_bear <- func_div_pre_cohort(nest_data = nest_year_lhs_bear)

# Pre cohort bull
Div_ind_pre_cohort_bull <- func_div_pre_cohort(nest_data = nest_year_lhs_bull)

### Joining bear---ordinary and pre cohort ###
Div_ind_bear <- Div_ind_pre_cohort_bear %>%
  dplyr::full_join(., Div_ind_ordinary_bear, by = 'Year')

### Joining bull---ordinary and pre cohort ###
Div_ind_bull <- Div_ind_pre_cohort_bull %>%
  dplyr::full_join(., Div_ind_ordinary_bull, by = 'Year')


Div_mean_bear <- apply(Div_ind_bear[, -1], 1, function(x){return(mean(x, na.rm = T))})
Div_mean_bull <- apply(Div_ind_bull[, -1], 1, function(x){return(mean(x, na.rm = T))})


Div_ind_bear <- Div_ind_bear %>%
  tibble::add_column('Div_world_mean' = Div_mean_bear) %>%
  dplyr::select(Year, Div_world_mean, everything())

Div_ind_bull <- Div_ind_bull %>%
  tibble::add_column('Div_world_mean' = Div_mean_bull) %>%
  dplyr::select(Year, Div_world_mean, everything())

diff_bear_bull <- Div_ind_bull$Div_world_mean - Div_ind_bear$Div_world_mean %>%
  tibble::as_tibble(.)

########################################
### Difference between bull and bear ###
########################################

Div_diff_bull_bear <- diff_bear_bull %>%
  tibble::add_column('Year' = Div_ind_bear$Year) %>%
  tibble::add_column('Div_bear_mean' = Div_ind_bear$Div_world_mean,
             'Div_bull_mean' = Div_ind_bull$Div_world_mean) %>%
  dplyr::rename('Diff' = value) %>%
  select(Year, Div_bull_mean, Div_bear_mean, everything())


### Annual returns from daily returns ###

func_row_mean_df <- function(df)
{
  mean_vec <- apply(df, 1, function(vec){return(mean(vec, na.rm = T))})
  mean_daily <- mean(mean_vec, na.rm = T)
  return(mean_daily)
}

nest_year_return <- nest_year_return %>%
  dplyr::mutate('daily_return' = purrr::map_dbl(LHS_country_valid, func_row_mean_df),
                'annual_return' = purrr::map_dbl(daily_return, 
                                                 function(vec){return((1+daily_return)^260 - 1)}))

nest_annual_return <- nest_year_return %>%
  dplyr::select(Year, annual_return) %>%
  dplyr::filter(Year >= 1987)

### Attaching annual returns to bear bull difference ###

Fig_4_appendix <- Div_diff_bull_bear %>%
  dplyr::select(Year, Diff) %>%
  tibble::add_column('annual_ret' = nest_annual_return$annual_return)

