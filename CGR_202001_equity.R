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
  vec[duplicated(vec)] <- NA
  return(vec)
}

df_equity_deduplic <- df_equity %>%
  dplyr::select(-c(Year, Date)) %>%
  apply(., 2, func_deduplic) %>%
  tibble::as_tibble(.) %>%
  tibble::add_column('Date' = df_equity$Date, 'Year' = df_equity$Year) %>%
  dplyr::select(Date, Year, everything(.))

df_equity_return <- df_equity_deduplic %>%
  dplyr::select(-c(Date, Year)) %>%
  apply(., 2, function(vec){return(c(NA, diff(log(vec))))}) %>%
  tibble::as_tibble(.) %>%
  tibble::add_column('Date' = df_equity$Date, 'Year' = df_equity$Year) %>%
  dplyr::select(Date, Year, everything(.))

nest_year_return <- df_equity_return %>%
  dplyr::select(-Date) %>%
  dplyr::group_by(Year) %>%
  tidyr::nest(.)

num_equity_usable <- 50

func_valid_ret <- function(df, n = num_equity_usable)
{
  temp_NA <- apply(df, 2, function(vec){sum(is.na(vec))})
  
  df_2 <- df[, temp_NA < (nrow(df) - n)]
  
  return(df_2)
}

nest_year_return <- nest_year_return %>%
  dplyr::mutate('LHS_country_valid' = purrr::map(data, func_valid_ret))

year_cohort <- 1986

func_rm_full_NA <- function(df)
{
  return(df[, colSums(is.na(df)) < nrow(df)])
}

name_country_pre_cohort <- df_equity_return %>%
  dplyr::filter(Year < year_cohort) %>%
  dplyr::select(-c(Date, Year, Canada_lag, US_lag)) %>%
  func_rm_full_NA(.) %>%
  colnames(.)
 
name_country_ordinary <- dplyr::setdiff(name_country_full, 
                                        name_country_pre_cohort)

func_select_pre_cohort <- function(df)
{
  return(df[, c(name_country_pre_cohort, 'Canada_lag', 'US_lag')])
}

func_select_ordinary <- function(df)
{
  
  return(df[, dplyr::intersect(colnames(df), name_country_ordinary)])
}

nest_year_return_LHS_RHS <- nest_year_return %>%
  dplyr::filter(Year >= year_cohort) %>%
  dplyr::mutate('RHS_country' = purrr::map(data, 
                                           func_select_pre_cohort)) %>%
  dplyr::mutate('LHS_country_ordinary' = purrr::map(LHS_country_valid, 
                                                    func_select_ordinary)) %>%
  dplyr::select(-data)


func_NA_med_df <- function(df)
{
  func_NA_med_vec <- function(vec)
  {
    vec[is.na(vec)] <- median(vec, na.rm = T)
    return(vec)
  }
  
  df_2 <- apply(df, 2, func_NA_med_vec)
  return(df_2)
}

nest_year_return_LHS_RHS <- nest_year_return_LHS_RHS %>%
  dplyr::mutate('RHS_country_clean' = purrr::map(RHS_country, func_NA_med_df), 
                'Cov_matrix' = purrr::map(RHS_country_clean, cov),
                'Eig_val' = purrr::map(Cov_matrix, function(df){return(eigen(df)$values)}),
                'Eig_vec' = purrr::map(Cov_matrix, function(df){return(eigen(df)$vectors)}),
                'Share' = purrr::map(Eig_val, function(vec){return(cumsum(vec)/sum(vec))}),
                'Lag_eig_vec' = dplyr::lag(Eig_vec)) %>%
  dplyr::select(-RHS_country)

func_pc_90 <- function(vec)
{
  return(min(which(vec >= 0.90)))
}

num_pc_90 <- sapply(nest_year_return_LHS_RHS$Share, func_pc_90)
num_pc_equity <- median(num_pc_90)

nest_year_return_LHS_RHS <- nest_year_return_LHS_RHS %>%
  dplyr::filter(!map_lgl(Lag_eig_vec, is.null)) %>%
  dplyr::mutate('PC_out_sample' = purrr::map2(RHS_country_clean, Lag_eig_vec,
                                              function(df1, df2){return(df1%*%df2)}),
                'PC_out_sample_90' = purrr::map(PC_out_sample,
                                                function(df){return(df[,1:num_pc_equity])}))


nest_year_regression <- nest_year_return_LHS_RHS %>%
  dplyr::select(Year, LHS_country_ordinary, PC_out_sample_90)

func_inf_nan_NA_df <- function(df)
{
  func_inf_nan_NA_vec <- function(vec)
  {
    vec[is.infinite(vec) | is.nan(vec)] <- NA
    return(vec)
  }
  return(apply(df, 2, func_inf_nan_NA_vec))
}

nest_year_regression <- nest_year_regression %>%
  dplyr::mutate('LHS_clean' = purrr::map(LHS_country_ordinary, func_inf_nan_NA_df))

func_lm_div <- function(df1, df2)
{
  lhs <- as.matrix(df1)
  rhs <- as.matrix(df2)
  
  div <- list(NULL)
  
  for (j in 1:ncol(lhs))
  {
    lm_summary <- summary(lm(formula = lhs[, j] ~ rhs))
    adj_rsq <- max(lm_summary$adj.r.squared, 0)
    div[[j]] <- 100*(1 - adj_rsq)
  }
  
  return(unlist(div))
}

nest_year_regression <- nest_year_regression %>%
  dplyr::mutate('Div_ind' = purrr::map2(LHS_clean, PC_out_sample_90,
                                        func_lm_div))

func_attach_name <- function(df_1, vec)
{
  names(vec) <- colnames(df_1)
  return(vec)
}

nest_year_regression <- nest_year_regression %>%
  dplyr::mutate('Div_ind_edit' = purrr::map2(LHS_clean, Div_ind,
                                             func_attach_name))

func_pick_name <- function(vec_name) {names(vec_name)}

list_unnest_div_ordinary <- nest_year_regression %>%
  dplyr::mutate('Country' = purrr::map(Div_ind_edit, func_pick_name)) %>%
  dplyr::select(Year, Div_ind_edit, Country) %>%
  tidyr::unnest(.)

Div_ind_ordinary <- list_unnest_div_ordinary %>%
  tidyr::spread(key = Country, value = Div_ind_edit) 

apply(Div_ind_ordinary[, - 1], 2, function(x){mean(x, na.rm = T)}) %>%
  plot(., type = 'l')

readr::write_csv(Div_ind_ordinary, 'Diversification_regular_countries.csv')