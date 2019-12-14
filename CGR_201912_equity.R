###############################################
### CGR post 201910, data updated till 2018 ###
###############################################

### Libraries #################################

library(tidyverse)
library(plm)
library(lmtest)
library(Matrix)

### Reading the data file ###

df_equity <- readr::read_csv('CGR_equity_2019.csv', 
                             na = c("", "NA", ".", " ", "NaN", 'Inf', '-Inf'),
                             col_names = T,
                             col_types = cols(.default = col_double(), 
                                              Date = col_date(format = "%d/%m/%Y")))


################################
### Main analysis starts now ###
################################

### Removing empty columns ###
func_full_NA_col_killer <- function(data_frame)
{
  # This function kills a data frame's full NA columns
  temp_no_NA_col <- data_frame[, colSums(is.na(data_frame)) < nrow(data_frame)]
  
  return(temp_no_NA_col)
}

df_equity <- df_equity %>%
  dplyr::mutate('Year' = lubridate::year(Date)) %>%
  dplyr::mutate('Month' = lubridate::month(Date)) %>%
  dplyr::mutate('Day' = lubridate::day(Date)) %>%
  dplyr::select(Date, Year, Month, Day, everything())

name_country <- colnames(df_equity)[-c(1,2,3,4)] #country names
num_country <- length(name_country)

name_country_pre86 <- df_equity %>% 
  dplyr::filter(Year < 1986) %>%
  func_full_NA_col_killer(.) %>%
  dplyr::select(-c(Date, Year, Month, Day)) %>%
  colnames(.)

name_country_regular <- setdiff(name_country, name_country_pre86)

year_min <- min(df_equity$Year) #earliest year
year_max <- max(df_equity$Year) #latest year
num_years <- year_max - year_min + 1
year_grid <- year_min:year_max
date_grid <- df_equity$Date[-1]

# Changing index levels to (log) returns

func_log_ret <- function(price_vec)
{
  # This function accepts a vector of prices and returns a vector of
  # log returns via the formula: r_t = log(p_t) - log(p_{t-1}).
  log_pr <- log(price_vec)
  return(diff(log_pr))
}

df_log_pr <- apply(df_equity[,-c(1,2,3,4)], 2, func_log_ret) %>%
  tibble::as_tibble(.) 

# Some Inf and NaNs may have crept due to taking logs
func_Inf_NaN_treat_NA_vec <- function(vec)
{
  #This function interprets NaN or Inf entries as NA
  vec[is.infinite(vec)] <- NA
  vec[is.nan(vec)] <- NA
  return(vec)
}

df_log_pr <- apply(df_log_pr, 2, func_Inf_NaN_treat_NA_vec) %>%
  tibble::as_tibble()

# With log returns
df_equity_clean <- cbind('Date' = date_grid, 
                         'Year' = df_equity$Year[-1], 
                         'Month' = df_equity$Month[-1],
                         'Day' = df_equity$Day[-1],
                         df_log_pr) %>% 
  tibble::as_tibble(.) 

# Nesting

nest_df_equity <- df_equity_clean %>%
  dplyr::group_by(Year) %>% 
  dplyr::filter(Year != 2019) %>% #ignore partial year observations
  dplyr::select(-Date) %>%
  tidyr::nest(.)

# Only countries with more than 50 usable returns are 
# allowed in the regression

func_filter_reg_country <- function(df)
{
  #This function accepts a dataframe, kills its
  #full NA columns, then accepts only those 
  #columns that have more than 50 (nonstale) observed
  #returns each year
  df_1 <- func_full_NA_col_killer(df)
  temp_NA <- apply(df_1, 2, function(x){sum(is.na(x))})
  #temp_stale <- apply(df_1, 2, func_stale_num)
  #temp_2 <- df_1[, as.numeric(temp_NA + temp_stale) < 200] #usable values > 50
  temp_2 <- df_1[, as.numeric(temp_NA) < 200] #usable values > 50
  
  return(temp_2)
}

func_select_pre86 <- function(df)
{
  #Select pre-86 country columns from the full data matrix
  temp_temp <- dplyr::select(df, name_country_pre86)
  return(temp_temp)
}

nest_df_equity_LHS <- nest_df_equity %>%
  dplyr::mutate("LHS_country_data" = purrr::map(data, func_filter_reg_country)) %>%
  dplyr::mutate('LHS_country_pre86' = purrr::map(data, func_select_pre86))

nest_df_equity_RHS <- df_equity_clean %>% 
  dplyr::filter(Year != 2019) %>%
  dplyr::select(c(Date, Year, Month, Day, name_country_pre86)) %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)

# Add list of lagged RHS data matrix 
lag_data_RHS <- c(list(NULL), nest_df_equity_RHS$data[1:45])

nest_df_equity_RHS <- nest_df_equity_RHS %>%
  tibble::add_column('Lag_data' = lag_data_RHS)

#Augmenting RHS matrix by lags of Canada and US
func_join_RHS_lag <- function(df_1, df_2)
{
  #This function accepts the original and lagged RHS 
  #data matrices, then selects North American countries
  #from the lagged data matrix, then augments the 
  #original RHS data matrix by the lags of Canada and US
  country_North_Am <- c('Canada', 'United States')
  
  lag_North_Am <- df_2 %>% 
    dplyr::select(Month, Day, country_North_Am) 
  # %>%
  #   dplyr::rename('Canada_lag' = Canada, 'US_lag' = `United States`)
  
  RHS_full <- dplyr::left_join(df_1, lag_North_Am, by = c('Month', 'Day'))
  
  return(RHS_full)
}

#RHS data matrix is augmented by lags of North American countries
RHS_data_matrix_list <- purrr::map2(nest_df_equity_RHS$data[-1],
                                    nest_df_equity_RHS$Lag_data[-1],
                                    func_join_RHS_lag)

#Remove month and day now
func_remove_month_day <- function(df)
{
  #This function accepts a dataframe and removes
  #the first two columns which here contain 
  #the month and day for the year's data matrix
  return(df[,-c(1,2)])
}

RHS_data_matrix_list <- purrr::map(RHS_data_matrix_list,
                                   func_remove_month_day)
                                    
nest_df_equity_RHS <- nest_df_equity_RHS %>%
  dplyr::mutate('RHS_data_matrix' = c(list(NULL), RHS_data_matrix_list))

nest_df_equity_RHS <- nest_df_equity_RHS %>%
  dplyr::filter(Year != 1973) %>%
  dplyr::mutate("RHS_country_data" = purrr::map(RHS_data_matrix, 
                                                func_filter_reg_country)) 

## Filling medians for residual missing values ######
## This is necessary otherwise NA will proliferate ##
## during covariance matrix calcualtions ############
func_NA_filler_vec <- function(vec)
{
  # This function fills a vector's missing values with its median
  vec[is.na(vec)] <- median(vec, na.rm = T)
  return(vec)
}

func_NA_filler_df <- function(df)
{
  #This function fills a matrix's missing values with column median
  temp <- apply(df, 2, func_NA_filler_vec) %>%
    tibble::as_tibble(.)
  return(temp)
}

nest_df_equity_RHS <- nest_df_equity_RHS %>%
  dplyr::mutate('RHS_country_clean' = purrr::map(RHS_country_data, 
                                                 func_NA_filler_df)) 

nest_df_equity_merge <- nest_df_equity_LHS %>%
  dplyr::filter(Year != 1973) %>%
  dplyr::select(-data) %>%
  tibble::add_column('RHS_data' = nest_df_equity_RHS$RHS_country_clean) %>%
  dplyr::mutate('Cov_matrix' = purrr::map(RHS_data, function(df){cov(df)}))

func_eig_val <- function(df)
{
  temp_eig <- eigen(df)
  return(temp_eig$values)
}

func_eig_vec <- function(df)
{
  temp_eig <- eigen(df)
  return(temp_eig$vectors)
}

# Variance explanation contribution of eigenvectors 
# is \lambda_i/(sum over \lambda_i)
func_eigen_share <- function(eig_val_vec)
{
  #This function accepts eigen values
  #and returns share of variance explained
  #by top to bottom eigenvectors (values)
  share <- cumsum(eig_val_vec)/sum(eig_val_vec)
  return(share)
}

nest_df_equity_merge <- nest_df_equity_merge %>%
  dplyr::mutate('Eigen_values' = purrr::map(Cov_matrix, func_eig_val)) %>%
  dplyr::mutate('Eigen_vectors' = purrr::map(Cov_matrix, func_eig_vec)) %>%
  dplyr::mutate('Share' = purrr::map(Eigen_values, func_eigen_share)) %>%
  dplyr::filter(Year > 1985)
