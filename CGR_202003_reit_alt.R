library(tidyverse)

########################################
### The previous dataset---till 2012 ###
########################################

# df_reit <- readr::read_csv('FTS_Data_REIT.csv',
#                            na = c("", "NA", ".", " ", "NaN", 'Inf', '-Inf'),
#                            skip = 3,
#                            col_names = T,
#                            col_types = cols(.default = col_double(),
#                                             Date = col_date(format = "%m/%d/%Y")))
# 
# df_reit <- df_reit %>%
#   dplyr::mutate('Year' = lubridate::year(Date)) %>%
# #  dplyr::mutate('Canada_lag' = dplyr::lag(Canada)) %>% #include one-day lags
#   dplyr::mutate('US_lag' = dplyr::lag(US)) %>% #include one-day lags
#   dplyr::select(-Date_Number) %>%
#   dplyr::select(Date, Year, everything())

###########################################
### The new dataset---updated till 2018 ###
###########################################

df_reit <- readr::read_csv('CGR_data_2018_reit.csv',
                             na = c("", "NA", ".", " ", "NaN", 'Inf', '-Inf'),
                             col_names = T,
                             col_types = cols(.default = col_double(),
                                              Date = col_date(format = "%d/%m/%Y")))

df_reit <- df_reit %>%
  dplyr::mutate('Year' = lubridate::year(Date)) %>%
  dplyr::filter(Year < 2019) %>%
#  dplyr::mutate('Canada_lag' = dplyr::lag(Canada)) %>% #include one-day lags
  dplyr::mutate('US_lag' = dplyr::lag(US)) %>% #include one-day lags
  dplyr::select(Date, Year, everything())

### Analysis begins here ###

# name_country_full <- df_reit %>%
#   dplyr::select(-c(Date, Year, Canada_lag, US_lag)) %>%
#   colnames(.)

name_country_full <- df_reit %>%
  dplyr::select(-c(Date, Year, US_lag)) %>%
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
df_reit_deduplic <- df_reit %>%
  dplyr::select(-c(Year, Date)) %>%
  apply(., 2, func_deduplic) %>%
  tibble::as_tibble(.) %>%
  tibble::add_column('Date' = df_reit$Date, 'Year' = df_reit$Year) %>%
  dplyr::select(Date, Year, everything(.))

# Taking log returns = log(I_t) - log(I_{t-1})
df_reit_return <- df_reit_deduplic %>%
  dplyr::select(-c(Date, Year)) %>%
  apply(., 2, function(vec){return(c(NA, diff(log(vec))))}) %>%
  tibble::as_tibble(.) %>%
  tibble::add_column('Date' = df_reit$Date, 'Year' = df_reit$Year) %>%
  dplyr::select(Date, Year, everything(.))

# Using nested dataframes
nest_year_return <- df_reit_return %>%
  dplyr::select(-Date) %>%
  dplyr::group_by(Year) %>%
  tidyr::nest(.)

# How many valid returns to use per year?
num_reit_usable <- 50
#num_reit_usable <- 100

func_valid_ret <- function(df, n = num_reit_usable)
{
  # This function accepts a return data matrix and 
  # accepts only those columns which have enough
  # usable entries ( >= num_reit_usable)
  temp_NA <- apply(df, 2, function(vec){sum(is.na(vec))})
  
  df_2 <- df[, temp_NA < (nrow(df) - n)]
  
  return(df_2)
}

nest_year_return <- nest_year_return %>%
  dplyr::mutate('LHS_country_valid' = purrr::map(data, func_valid_ret))

##############################
##### COUNTRY COHORTS ########
##############################

# Pre 2000 cohort
temp_99 <- nest_year_return %>%
  dplyr::filter(Year == 1999) %>%
  dplyr::select(-data)

country_99_temp <- temp_99$LHS_country_valid[[1]] %>%
  colnames()

country_99 <- country_99_temp[1:(length(country_99_temp)-1)]

# post 2000
temp_2018 <- nest_year_return %>%
  dplyr::filter(Year == 2018) %>%
  dplyr::select(-data)

country_9918 <- temp_2018$LHS_country_valid[[1]] %>%
  colnames()

country_2018_temp <- setdiff(country_9918, country_99)
country_2018 <- country_2018_temp[1:(length(country_2018_temp)-1)]

#############################  


year_cohort <- 1986

func_rm_full_NA <- function(df)
{
  # This function accepts a data frame
  # and removes columns which are
  # completely full of missing values
  return(df[, colSums(is.na(df)) < nrow(df)])
}

# Names of pre-86 cohort countries
# name_country_pre_cohort <- df_reit_return %>%
#   dplyr::filter(Year < year_cohort) %>%
#   dplyr::select(-c(Date, Year, Canada_lag, US_lag)) %>%
#   func_rm_full_NA(.) %>%
#   colnames(.)

name_country_pre_cohort <- df_reit_return %>%
  dplyr::filter(Year < year_cohort) %>%
  dplyr::select(-c(Date, Year, US_lag)) %>%
  func_rm_full_NA(.) %>%
  colnames(.)

# Names of ordinary countries
name_country_ordinary <- dplyr::setdiff(name_country_full, 
                                        name_country_pre_cohort)

# Select from the set of columns the pre-86 countries 
# along with the one-day lags of US and Canada
func_select_RHS_countries <- function(df)
{
  # This function accepts a return data matrix
  # and returns the pre86 cohort with the one-day
  # lags of US and Canada---to be used for the 
  # computation of principal components
  
  # colnames_select <- c(dplyr::intersect(colnames(df), name_country_pre_cohort),
  #                      'Canada_lag', 'US_lag')
  
  colnames_select <- c(dplyr::intersect(colnames(df), name_country_pre_cohort),
                       'US_lag')
  
  return(df[, colnames_select])
}

func_select_ordinary <- function(df)
{
  # This accepts a data return matrix and selects 
  # the ordinary non-pre86 cohort country columns
  return(df[, dplyr::intersect(colnames(df), name_country_ordinary)])
}

func_select_pre_cohort <- function(df)
{
  # This accepts a data return matrix and selects 
  # the ordinary non-pre86 cohort country columns
  return(df[, dplyr::intersect(colnames(df), name_country_pre_cohort)])
}

nest_year_return_LHS_RHS <- nest_year_return %>%
  dplyr::filter(Year >= year_cohort) %>%
#  dplyr::mutate('data_clean' = purrr::map(data, func_rm_full_NA)) %>%
  dplyr::mutate('RHS_country' = purrr::map(data, 
                                           func_select_RHS_countries)) %>%
  dplyr::mutate('LHS_country_ordinary' = purrr::map(LHS_country_valid, 
                                                    func_select_ordinary)) %>%
  dplyr::select(-data)


func_NA_med_df <- function(df)
{
  # This function accepts a dataframe and
  # replaces missing values with column medians
  func_NA_med_vec <- function(vec)
  {
    vec[is.na(vec)] <- median(vec, na.rm = T)
    return(vec)
  }
  
  df_2 <- apply(df, 2, func_NA_med_vec)
  return(df_2)
}

func_rm_full_NA_rowcol <- function(df)
{
  # This function accepts a dataframe and kill those rows
  # and columns that are full with NA
  return(df[rowSums(is.na(df)) < ncol(df), colSums(is.na(df)) < nrow(df)])
}


nest_year_return_LHS_RHS <- nest_year_return_LHS_RHS %>%
  dplyr::mutate('RHS_country_clean' = purrr::map(RHS_country, func_NA_med_df), 
                'Cov_matrix' = purrr::map(RHS_country_clean, cov),
#                'Cov_clean' = purrr::map(Cov_matrix, func_rm_full_NA_rowcol),
                'Eig_val' = purrr::map(Cov_matrix, function(df){return(eigen(df)$values)}),
                'Eig_vec' = purrr::map(Cov_matrix, function(df){return(eigen(df)$vectors)}),
                'Share' = purrr::map(Eig_val, function(vec){return(cumsum(vec)/sum(vec))})) %>%
  dplyr::select(-RHS_country)

nest_lag_eig <- dplyr::lag(nest_year_return_LHS_RHS$Eig_vec) #due to some weird nested lag behavior

# Adding lags of eigenvectors
nest_year_return_LHS_RHS <- nest_year_return_LHS_RHS %>%
  tibble::add_column('Lag_eig_vec' = nest_lag_eig)

func_pc_90 <- function(vec)
{
  # This function accepts a cumulative share of variance
  # vector and returns the number of PCs needed for 90% coverage 
  return(min(which(vec >= 0.90)))
}

# How many PCs needed for explaining 90% of variation?
num_pc_90 <- sapply(nest_year_return_LHS_RHS$Share, func_pc_90)
# num_pc_reit <- ceiling(median(num_pc_90)) # 12 are enough
num_pc_reit <- min(num_pc_90) 

#############################################
### PC computation for ordinary countries ###
#############################################

# Compute PCs
nest_year_return_LHS_RHS <- nest_year_return_LHS_RHS %>%
  dplyr::filter(map(Lag_eig_vec, length) > 1) %>%
  dplyr::mutate('PC_out_sample' = purrr::map2(RHS_country_clean, Lag_eig_vec,
                                              function(df1, df2){return(df1%*%df2)}), #note df1%*%df2 [!]
                'PC_out_sample_90' = purrr::map(PC_out_sample,
                                                function(df){return(df[, 1:num_pc_reit])}))


nest_year_regression <- nest_year_return_LHS_RHS %>%
  dplyr::select(Year, LHS_country_ordinary, PC_out_sample_90)

func_inf_nan_NA_df <- function(df)
{
  # This accepts a dataframe and treats any infinite
  # or NaN values as missing
  func_inf_nan_NA_vec <- function(vec)
  {
    vec[is.infinite(vec) | is.nan(vec)] <- NA
    return(vec)
  }
  
  return(apply(df, 2, func_inf_nan_NA_vec))
}

# Treat as missing any infinite or NaN values that might be remaining
nest_year_regression <- nest_year_regression %>%
  dplyr::mutate('LHS_clean' = purrr::map(LHS_country_ordinary, func_inf_nan_NA_df))

func_lm_div <- function(df1, df2)
{
  # This function accepts the LHS and RHS of regressions
  # and returns the diversification index = 100*(1-adj_R_sqr)
  lhs <- as.matrix(df1)
  rhs <- as.matrix(df2)
  
  div <- list(NULL)
  
  for (j in 1:ncol(lhs)) #for each LHS country
  {
    lm_summary <- summary(lm(formula = lhs[, j] ~ rhs))
    adj_rsq <- max(lm_summary$adj.r.squared, 0)
    div[[j]] <- 100*(1 - adj_rsq)
  }
  
  return(unlist(div))
}

# Compute diversification indices
nest_year_regression <- nest_year_regression %>%
  dplyr::filter(map_dbl(LHS_clean, length) > 0) %>%
  dplyr::mutate('Div_ind' = purrr::map2(LHS_clean, PC_out_sample_90,
                                        func_lm_div))

func_attach_name <- function(df_1, vec)
{
  # Accepts regression LHS matrix and unnamed 
  # diversification vector indices and attaches 
  # the column names of LHS to the vector of indices
  names(vec) <- colnames(df_1)
  return(vec)
}

# Attach names of countries to diversification indices
nest_year_regression <- nest_year_regression %>%
  dplyr::mutate('Div_ind_edit' = purrr::map2(LHS_clean, Div_ind,
                                             func_attach_name))

func_pick_name <- function(vec_name) {names(vec_name)} #extract names

# Unnest results in long format
list_unnest_div_ordinary <- nest_year_regression %>%
  dplyr::mutate('Country' = purrr::map(Div_ind_edit, func_pick_name)) %>%
  dplyr::select(Year, Div_ind_edit, Country) %>%
  tidyr::unnest(.)

# Spread in wide format
Div_ind_ordinary <- list_unnest_div_ordinary %>%
  tidyr::spread(key = Country, value = Div_ind_edit) 

###############################################
### PC computation for pre cohort countries ###
###############################################

nest_year_pre_cohort <- nest_year_return %>%
  dplyr::select(Year, LHS_country_valid) %>%
  dplyr::filter(Year >= year_cohort) %>%
  dplyr::mutate('LHS_clean' = purrr::map(LHS_country_valid, 
                                         func_inf_nan_NA_df)) %>% #remove infinite or NaNs
  dplyr::mutate('LHS_pre_cohort' = purrr::map(LHS_clean, 
                                              func_select_pre_cohort)) %>%
  dplyr::mutate('RHS_countries' = purrr::map(LHS_clean, func_select_RHS_countries)) %>%
  dplyr::mutate('RHS_clean' = purrr::map(RHS_countries, func_NA_med_df))

nest_year_pre_cohort_2 <- nest_year_pre_cohort %>%
  dplyr::select(Year, LHS_pre_cohort, RHS_clean)

func_rm_col_j <- function(df)
{
  # This function accepts a data matrix and generates 
  # a sequence of matrices each one missing one 
  # column at a time (excluding lags of Canada and US)
  temp_list <- list(NULL)
  
  # df2 <- df[, -c(ncol(df)-1, ncol(df))] #exclude North Am lags
  df2 <- df[, -ncol(df)] #exclude US lags
  
  for (j in 1:ncol(df2))
  {
    temp_list[[j]] <- df2[, -j] #remove column j
  }
  
  return(temp_list)
}

# Generate sequence of matrices with each column removed once
nest_year_pre_cohort_2 <- nest_year_pre_cohort_2 %>%
  dplyr::mutate('RHS_country_j' = purrr::map(RHS_clean, func_rm_col_j)) 

func_eig_val_list <- function(list)
{
  # This function accepts a list of matrices and
  # applies to each matrix, a function that computes
  # its eigenvalues, then returns the list of 
  # eigenvalues---one for each matrix in the list
  func_eig_val_df <- function(df)
  {
    return(eigen(df)$values)
  }
  
  eig_val_list <- purrr::map(list, func_eig_val_df)
  
  return(eig_val_list)
}

func_eig_vec_list <- function(list)
{
  # This function accepts a list of matrices and
  # applies to each matrix, a function that computes
  # its eigenvectors, then returns the list of 
  # eigenvectors---one for each matrix in the list
  func_eig_vec_df <- function(df)
  {
    return(eigen(df)$vectors)
  }
  
  eig_vec_list <- purrr::map(list, func_eig_vec_df)
  
  return(eig_vec_list)
}


nest_year_pre_cohort_2 <- nest_year_pre_cohort_2 %>%
  dplyr::mutate('Cov_list_j' = purrr::map(RHS_country_j, 
                                          function(list){return(purrr::map(list, cov))})) %>%
  dplyr::mutate('Eig_val_list_j' = purrr::map(Cov_list_j, func_eig_val_list)) %>%
  dplyr::mutate('Eig_vec_list_j' = purrr::map(Cov_list_j, func_eig_vec_list)) 

nest_lag_eig_list <- dplyr::lag(nest_year_pre_cohort_2$Eig_vec_list_j) #due to some weird nested lag behavior

nest_year_pre_cohort_2 <- nest_year_pre_cohort_2 %>%
  tibble::add_column('Lag_eig_vec_list_j' = nest_lag_eig_list)

nest_year_pre_cohort_final <- nest_year_pre_cohort_2 %>%
  dplyr::select(Year, LHS_pre_cohort, RHS_country_j, 
                Eig_vec_list_j, Lag_eig_vec_list_j)

func_list_multiply <- function(list1, list2)
{
  # This function accepts two lists of matrices
  # then multiplies matrix i in list 1 to matrix i
  # in list 2 and returns the output as another list
  func_df_mult <- function(df1, df2)
  {
    return(df1%*%df2) #note matrix multiplication %*%
  }
  
  list3 <- purrr::map2(list1, list2, func_df_mult)
  
  return(list3)
  
}


nest_year_pre_cohort_final <- nest_year_pre_cohort_final %>%
  dplyr::mutate('length_list_RHS' = purrr::map_dbl(RHS_country_j, length),
                'length_list_lag_eig' = purrr::map_dbl(Lag_eig_vec_list_j, length))

# Which RHS and lag eigenvector list lengths are the same? Multiply them
nest_year_pre_cohort_final_2 <- nest_year_pre_cohort_final %>%
  dplyr::filter(length_list_RHS == length_list_lag_eig) %>%
  dplyr::mutate('PC_list_j' = purrr::map2(RHS_country_j, Lag_eig_vec_list_j,
                                          func_list_multiply)) %>%
  dplyr::select(Year, LHS_pre_cohort, PC_list_j)


# Which RHS and lag eigenvector list lengths are NOT the same? Multiply RHS with eigenvectors
nest_year_pre_cohort_final_3 <- nest_year_pre_cohort_final %>%
  dplyr::filter(length_list_RHS != length_list_lag_eig) %>%
  dplyr::mutate('PC_list_j' = purrr::map2(RHS_country_j, Eig_vec_list_j, 
                                          func_list_multiply)) %>%
  dplyr::select(Year, LHS_pre_cohort, PC_list_j)

nest_year_pre_cohort_regress <- rbind(nest_year_pre_cohort_final_3,
                                      nest_year_pre_cohort_final_2) %>%
  dplyr::arrange(Year)

# func_select_PC_list <- function(list)
# {
#   # This function accepts a list of PCs matrices and 
#   # returns the list with each PC matrix with 
#   # number of columns from 1 to num_pc_reit
#   
#   func_select_PC_df <- function(df)
#   {
#     # ncol_min <- min(ncol(df), num_pc_reit)
#     # return(df[, 1:ncol_min])
#     return(df[, 1:num_pc_reit])
#   }
#   
#   list1 <- purrr::map(list, func_select_PC_df)
#   
#   return(list1)
# }
# 
# nest_year_pre_cohort_regress <- nest_year_pre_cohort_regress %>%
#   dplyr::mutate('PC_list_j_final' = purrr::map(PC_list_j, func_select_PC_list))


func_pre_cohort_regress <- function(df1, list)
{
  # This function accepts the matrix of LHS countries
  # and the list of RHS matrices, then returns the 
  # diversification index for each country
  div_list <- list(NULL)
  
  for (i in 1:ncol(df1))
  {
    df2 <- list[[i]]
    
    temp_lhs <- df1[, i]
    temp_rhs <- as.matrix(df2)
    
    lm_summary <- summary(lm(formula = temp_lhs ~ temp_rhs))
    adj_rsq <- max(lm_summary$adj.r.squared, 0)
    div_list[[i]] <- 100*(1 - adj_rsq)
  }
  
  return(unlist(div_list))
}


# nest_year_pre_cohort_regress <- nest_year_pre_cohort_regress %>%
#   dplyr::filter(purrr::map(PC_list_j_final, length) > 0) %>%
#   dplyr::mutate('Div_ind_pre_cohort' = purrr::map2(LHS_pre_cohort, PC_list_j_final,
#                                                    func_pre_cohort_regress))

nest_year_pre_cohort_regress <- nest_year_pre_cohort_regress %>%
  dplyr::filter(purrr::map(PC_list_j, length) > 0) %>%
  dplyr::mutate('Div_ind_pre_cohort' = purrr::map2(LHS_pre_cohort, PC_list_j,
                                                   func_pre_cohort_regress))



func_attach_name_pre_cohort <- function(vec, LHS_data)
{
  names(vec) <- colnames(LHS_data)
  return(vec)
}

nest_year_pre_cohort_regress <- nest_year_pre_cohort_regress %>%
  dplyr::mutate('Div_ind_pre_cohort_name' = purrr::map2(Div_ind_pre_cohort, LHS_pre_cohort,
                                                        func_attach_name_pre_cohort))

######################################################################################

# Unnest results in long format
list_unnest_div_pre_cohort <- nest_year_pre_cohort_regress %>%
  dplyr::mutate('Country' = purrr::map(Div_ind_pre_cohort_name, func_pick_name)) %>%
  dplyr::select(Year, Div_ind_pre_cohort_name, Country) %>%
  tidyr::unnest(.)

# Spread in wide format
Div_ind_pre_cohort <- list_unnest_div_pre_cohort %>%
  tidyr::spread(key = Country, value = Div_ind_pre_cohort_name) 

# Full dataframe for all countries
Div_ind_full_long <- dplyr::full_join(Div_ind_ordinary, Div_ind_pre_cohort,
                                      by = c('Year')) %>%
  tidyr::gather(., Australia:US, key = 'Country', value = 'Div_Index') %>%
  dplyr::arrange(Country)

Div_ind_full_wide <- Div_ind_full_long %>%
  tidyr::spread(key = 'Country', value = 'Div_Index') 

Div_world_mean <- apply(Div_ind_full_wide[, -1], 1, function(x){return(mean(x, na.rm = T))})


Div_ind_full_wide <- Div_ind_full_wide %>%
  tibble::add_column('Div_world_mean' = Div_world_mean) %>%
  dplyr::select(Year, Div_world_mean, everything())

Div_ind_plot <- ggplot(data = Div_ind_full_wide, 
                       mapping = aes(Year, Div_world_mean)) +
  geom_line() +
  geom_smooth(method = lm) +
  theme_bw()

Div_ind_full_long_2 <- Div_ind_full_wide %>%
  tidyr::gather(Div_world_mean:US, key = 'Country', value = 'Div_Index')

