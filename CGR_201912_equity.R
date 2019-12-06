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
                             na = c("", "NA", ".", " "),
                             col_names = T,
                             col_types = cols(.default = col_double(), 
                                              Date = col_date(format = "%d/%m/%Y")))


### Removing empty columns ###

func_full_NA_col_killer <- function(data_frame)
{
  # This function kills a data frame's full NA columns
  temp_no_NA_col <- data_frame[, colSums(is.na(data_frame)) < nrow(data_frame)]
  
  return(temp_no_NA_col)
}

################################
### Main analysis starts now ###
################################

df_equity_clean <- df_equity %>% func_full_NA_col_killer(.) #remove empty columns
df_equity_clean[df_equity_clean < 0] <- NA #ignore negative values

temp_yr <- lubridate::year(df_equity_clean$Date)
df_equity_clean <- df_equity_clean %>%
  tibble::add_column(Year = temp_yr) %>%
  dplyr::select(Date, Year, everything()) #adding year column

name_country <- colnames(df_equity_clean)[-1] #country names
num_country <- ncol(df_equity_clean) - 1 #num of columns (excl Date)

name_country_pre86 <- df_equity_clean %>% 
  dplyr::filter(Year < 1986) %>%
  func_full_NA_col_killer(.) %>%
  dplyr::select(-c(Date, Year)) %>%
  colnames(.)

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
df_equity_clean <- cbind('Date' = date_grid, 
                         'Year' = df_equity_clean$Year[-1], 
                         df_log_pr) %>%
  tibble::as_tibble(.) 

# Nesting

nest_df_equity <- df_equity_clean %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)

# Only countries with more than 50 usable returns are 
# allowed in the regression

func_LHS_reg_country <- function(df)
{
  #This function accepts a dataframe, kills its
  #full NA columns, then accepts only those 
  #columns that have more than 50 non-missing 
  #returns each year
  df_1 <- func_full_NA_col_killer(df)
  temp <- apply(df_1, 2, function(x){sum(is.na(x))})
  temp_2 <- df_1[, temp < 315] #missing values < 50
  
  return(temp_2)
}

nest_df_equity_LHS <- nest_df_equity %>%
  dplyr::mutate("LHS_country" = purrr::map(data, func_LHS_reg_country))

nest_df_equity_RHS <- df_equity_clean %>% 
  dplyr::select(c(Date, Year, name_country_pre86)) %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)

nest_df_equity_RHS <- nest_df_equity_RHS %>%
  dplyr::mutate("RHS_data" = purrr::map(data, func_full_NA_col_killer))


## Filling medians for missing values ##

func_NA_filler <- function(vec)
{
  # This function fills a vector's missing values with its median 
  vec[is.na(vec)] <- median(vec, na.rm = T)
  return(vec)
}


# Changing data from wide format to long format
# df_equity_long <- df_equity_clean %>%
#   tidyr::gather(Australia:Venezuela,
#                 key = "Country",
#                 value = "Ind_Ret") %>%
#   dplyr::arrange(Country)

  