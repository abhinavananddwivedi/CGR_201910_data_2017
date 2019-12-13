###############################################
### CGR post 201910, data updated till 2018 ###
###############################################

### Libraries #################################

library(tidyverse)
library(plm)
library(lmtest)
library(lubridate)

### Reading the data file ###

df_equity <- readr::read_csv('CGR_equity_2019.csv', 
                             na = c("", "NA", ".", " ", "NaN"),
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


# df_equity_clean <- df_equity %>% func_full_NA_col_killer(.) #remove empty columns
# df_equity_clean[df_equity_clean < 0] <- NA #ignore negative values

# df_equity_clean <- df_equity_clean %>%
#   dplyr::mutate('Year' = lubridate::year(Date)) %>%
#   dplyr::select(Date, Year, everything())

df_equity_clean <- df_equity %>%
  dplyr::mutate('Year' = lubridate::year(Date)) %>%
  dplyr::select(Date, Year, everything())

name_country <- colnames(df_equity_clean)[-c(1,2)] #country names
num_country <- length(name_country)

name_country_pre86 <- df_equity_clean %>% 
  dplyr::filter(Year < 1986) %>%
  func_full_NA_col_killer(.) %>%
  dplyr::select(-c(Date, Year)) %>%
  colnames(.)

name_country_regular <- setdiff(name_country, name_country_pre86)

year_min <- min(df_equity_clean$Year) #earliest year
year_max <- max(df_equity_clean$Year) #latest year
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

df_log_pr <- apply(df_equity_clean[,-c(1,2)], 2, func_log_ret) %>%
  tibble::as_tibble(.) 

# With log returns
df_equity_clean_final <- cbind('Date' = date_grid, 
                         'Year' = df_equity_clean$Year[-1], 
                         df_log_pr) %>%
  tibble::as_tibble(.) 

# Nesting

nest_df_equity <- df_equity_clean_final %>%
  dplyr::group_by(Year) %>% 
  dplyr::filter(Year != 2019) %>% #ignore partial year observations
  dplyr::select(-Date) %>%
  tidyr::nest(.)

# Only countries with more than 50 usable returns are 
# allowed in the regression

func_stale_num <- function(vec)
{
  #This returns the number of stale obervations 
  #in each column vector after ignoring missing values
  unique_obs <- length(unique(vec[!is.na(vec)]))
  stale_num <- length(vec) - unique_obs
  return(stale_num)
}

func_filter_reg_country <- function(df)
{
  #This function accepts a dataframe, kills its
  #full NA columns, then accepts only those 
  #columns that have more than 50 (nonstale) observed
  #returns each year
  df_1 <- func_full_NA_col_killer(df)
  temp_NA <- apply(df_1, 2, function(x){sum(is.na(x))})
  temp_stale <- apply(df_1, 2, func_stale_num)
  temp_2 <- df_1[, as.numeric(temp_NA + temp_stale) < 200] #usable values > 50
  
  return(temp_2)
}

func_select_pre86 <- function(df)
{
  #Select pre-86 country columns from the full data matrix
  temp_temp <- dplyr::select(df, name_country_pre86)
  #temp_temp <- df[, c(Date, name_country_pre86)]
  return(temp_temp)
}

nest_df_equity_LHS <- nest_df_equity %>%
  dplyr::mutate("LHS_country_data" = purrr::map(data, func_filter_reg_country)) %>%
  dplyr::mutate('LHS_country_pre86' = purrr::map(data, func_select_pre86))

nest_df_equity_RHS <- df_equity_clean_final %>% 
  dplyr::filter(Year != 2019) %>%
  dplyr::select(c(Date, Year, name_country_pre86)) %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)


nest_df_equity_RHS <- nest_df_equity_RHS %>%
  dplyr::mutate("RHS_country_data" = purrr::map(data, func_filter_reg_country)) 

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
  dplyr::select(-data) %>%
  tibble::add_column('RHS_data' = nest_df_equity_RHS$RHS_country_clean)

func_select_LHS_RHS <- function(df_1, df_2)
{
  col_df1 <- colnames(df_1)
  temp_1 <- df_1[, col_df1 %in% name_country_regular]
  
  # if (ncol(temp_1) > 0)
  # {
  #   
  #   temp_RHS <- 
  # }
  
  return(temp_2)
}


# Variance explanation contribution of eigenvectors 
# is \lambda_i/(sum over \lambda_i)
func_eigen_share <- function(df)
{
  #This function accepts a (covariance) matrix
  #and returns share of variance explained
  #by top to bottom eigenvectors (values)
  temp <- eigen(df)
  share <- cumsum(temp$values)/sum(temp$values)
  return(share)
}

nest_df_equity_RHS <- nest_df_equity_RHS %>%
  dplyr::mutate('Share' = purrr::map(Cov_matrix, func_eigen_share))


