library(tidyverse)

### The previous dataset
df_equity <- readr::read_csv('FTS_Data_Equity.csv',
                             na = c("", "NA", ".", " ", "NaN", 'Inf', '-Inf'),
                             skip = 3,
                             col_names = T,
                             col_types = cols(.default = col_double(),
                                              Date = col_date(format = "%m/%d/%Y")))

df_equity <- df_equity %>%
  dplyr::mutate('Year' = lubridate::year(Date)) %>%
  dplyr::mutate('Canada_lag' = dplyr::lag(Canada)) %>% #include one-day lags
  dplyr::mutate('US_lag' = dplyr::lag(US)) %>% #include one-day lags
  dplyr::select(-Date_Number) %>%
  dplyr::select(Date, Year, everything())

name_country_full <- df_equity %>%
  dplyr::select(-c(Date, Year, Canada_lag, US_lag)) %>%
  colnames(.)


func_deduplic <- function(vec)
{
  # This function accepts a vector
  # and replaces duplicated entries
  # with missing values
  vec[duplicated(vec)] <- NA
  return(vec)
}

# Ignoring duplicated entries
df_equity_deduplic <- df_equity %>%
  dplyr::select(-c(Year, Date)) %>%
  apply(., 2, func_deduplic) %>%
  tibble::as_tibble(.) %>%
  tibble::add_column('Date' = df_equity$Date, 'Year' = df_equity$Year) %>%
  dplyr::select(Date, Year, everything(.))

# Taking log returns = log(I_t) - log(I_{t-1})
df_equity_return <- df_equity_deduplic %>%
  dplyr::select(-c(Date, Year)) %>%
  apply(., 2, function(vec){return(c(NA, diff(log(vec))))}) %>%
  tibble::as_tibble(.) %>%
  tibble::add_column('Date' = df_equity$Date, 'Year' = df_equity$Year) %>%
  dplyr::select(Date, Year, everything(.))

# Using nested dataframes
nest_year_return <- df_equity_return %>%
  dplyr::select(-Date) %>%
  dplyr::group_by(Year) %>%
  tidyr::nest(.)

# How many valid returns to use per year?
num_equity_usable <- 50

func_valid_ret <- function(df, n = num_equity_usable)
{
  # This function accepts a return data matrix and 
  # accepts only those columns which have enough
  # usable entries ( >= num_equity_usable)
  temp_NA <- apply(df, 2, function(vec){sum(is.na(vec))})
  
  df_2 <- df[, temp_NA < (nrow(df) - n)]
  
  return(df_2)
}

nest_year_return <- nest_year_return %>%
  dplyr::mutate('LHS_country_valid' = purrr::map(data, func_valid_ret))

year_cohort <- 1986

func_rm_full_NA <- function(df)
{
  # This function accepts a data frame
  # and removes columns which are
  # completely full of missing values
  return(df[, colSums(is.na(df)) < nrow(df)])
}

# Names of pre-86 cohort countries
name_country_pre_cohort <- df_equity_return %>%
  dplyr::filter(Year < year_cohort) %>%
  dplyr::select(-c(Date, Year, Canada_lag, US_lag)) %>%
  func_rm_full_NA(.) %>%
  colnames(.)

# Names of ordinary countries
name_country_ordinary <- dplyr::setdiff(name_country_full, 
                                        name_country_pre_cohort)

# Select from the set of columns the pre-86 countries 
# along with the one-day lags of US and Canada
func_select_pre_cohort <- function(df)
{
  # This function accepts a return data matrix
  # and returns the pre86 cohort with the one-day
  # lags of US and Canada---to be used for the 
  # computation of principal components
  return(df[, c(name_country_pre_cohort, 'Canada_lag', 'US_lag')])
}


func_rm_inf_NaN_df <- function(df)
{
  func_rm_inf_NaN_vec <- function(vec)
  {
    vec[is.nan(vec) | is.infinite(vec)] <- NA
    return(vec)
  }
  
  df1 <- apply(df, 2, func_rm_inf_NaN_vec)
  return(df1)
}

func_NA_med_df <- function(df)
{
  func_NA_med_vec <- function(vec)
  {
    vec[is.na(vec)] <- median(vec, na.rm = T)
    return(vec)
  }
  
  df1 <- apply(df, 2, func_NA_med_vec)
  return(df1)
}

nest_year_post_cohort <- nest_year_return %>%
  dplyr::filter(Year >= year_cohort) %>%
  dplyr::mutate('LHS_clean' = purrr::map(LHS_country_valid, func_rm_inf_NaN_df)) %>%
  dplyr::mutate('LHS_pre_cohort' = purrr::map(LHS_clean, func_select_pre_cohort)) %>%
  dplyr::mutate('RHS_data' = purrr::map(LHS_pre_cohort, func_NA_med_df)) %>%
  dplyr::select(-c(data, LHS_country_valid))

func_rm_col_i_df <- function(df)
{
  RHS_list <- list(NULL)
  
  for (i in 1:(ncol(df) - 2)) #include pre-cohort countries, exclude lags
  {
    RHS_list[[i]] <- df[, -i]
  }
  
  return(RHS_list)
}

nest_year_RHS <- nest_year_post_cohort %>%
  dplyr::select(Year, RHS_data) %>%
  dplyr::mutate('RHS_i' = purrr::map(RHS_data, func_rm_col_i_df)) 

func_eig_vec_list <- function(list)
{
  func_eig_vec_df <- function(df)
  {
    return(eigen(df)$vectors)
  }
  
  eig_vec_list <- purrr::map(list, func_eig_vec_df)
  
  return(eig_vec_list)
}

nest_year_RHS <- nest_year_RHS %>%
  dplyr::mutate('Cov_matrix_i' = purrr::map(RHS_i, function(list){return(purrr::map(list, cov))})) %>%
  dplyr::mutate('Eig_vec_i' = purrr::map(Cov_matrix_i, func_eig_vec_list)) %>%
  dplyr::mutate('Lag_eig_vec_i' = dplyr::lag(Eig_vec_i))

func_multiply_list <- function(list1, list2)
{
  func_multiply_df <- function(df1, df2)
  {
    return(df1%*%df2)
  }
  
  list3 <- purrr::map2(list1, list2, func_multiply_df)
  
  return(list3)
}

num_pc_equity <- 16

nest_year_RHS <- nest_year_RHS %>%
  dplyr::filter(purrr::map(Lag_eig_vec_i, length) > 0) %>%
  dplyr::mutate('PC' = purrr::map2(RHS_i, Lag_eig_vec_i, func_multiply_list)) 

func_select_PC_90_list <- function(list)
{
  func_select_PC_90_df <- function(df)
  {
    return(df[, 1:num_pc_equity])
  }
  
  list2 <- purrr::map(list, func_select_PC_90_df)
  
  return(list2)
}

nest_year_RHS <- nest_year_RHS %>%
  dplyr::mutate('PC_90' = purrr::map(PC, func_select_PC_90_list))

nest_year_pre_cohort_regression <- nest_year_RHS %>%
  dplyr::select(Year, PC_90) %>%
  dplyr::left_join(., nest_year_post_cohort, by = 'Year') %>%
  dplyr::select(Year, LHS_pre_cohort, PC_90)

func_LHS_deselect_NA_lags <- function(df)
{
  df2 <- df[, 1:(ncol(df) - 2)]
  return(df2)
}

nest_year_pre_cohort_regression <- nest_year_pre_cohort_regression %>%
  dplyr::mutate('LHS_pre_cohort_no_lags' = purrr::map(LHS_pre_cohort, func_LHS_deselect_NA_lags))

func_regress_pre_cohort <- function(df, list)
{
  div_ind <- list(NULL)
  
  for (j in 1:ncol(df))
  {
    rhs <- list[[j]]
    lhs <- df[, j]
    
    lm_summary <- summary(lm(formula = lhs ~ rhs))
    adj_rsq <- max(lm_summary$adj.r.squared, 0)
    div_ind[[j]] <- 100*(1 - adj_rsq)
  }
  
  return(unlist(div_ind))
}

nest_year_pre_cohort_regression <- nest_year_pre_cohort_regression %>%
  dplyr::mutate('Div_ind_pre' = purrr::map2(LHS_pre_cohort_no_lags, PC_90,
                                            func_regress_pre_cohort))

func_attach_name_pre_cohort <- function(vec)
{
  names(vec) <- name_country_pre_cohort
  return(vec)
}

func_pick_name <- function(vec_name) {names(vec_name)} #extract names

nest_year_pre_cohort_regression <- nest_year_pre_cohort_regression %>%
  dplyr::mutate('Div_ind_pre_cohort_name' = purrr::map(Div_ind_pre,
                                                       func_attach_name_pre_cohort))

# Unnest results in long format
list_unnest_div_pre_cohort <- nest_year_pre_cohort_regression %>%
  dplyr::mutate('Country' = purrr::map(Div_ind_pre_cohort_name, func_pick_name)) %>%
  dplyr::select(Year, Div_ind_pre_cohort_name, Country) %>%
  tidyr::unnest(.)

# Spread in wide format
Div_ind_pre_cohort <- list_unnest_div_pre_cohort %>%
  tidyr::spread(key = Country, value = Div_ind_pre_cohort_name) 