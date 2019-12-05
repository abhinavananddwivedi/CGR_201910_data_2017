###############################################
### CGR post 201910, data updated till 2018 ###
###############################################

### Libraries #################################

library(tidyverse)
library(plm)
library(lmtest)
library(lubridate)

### Reading the data file ###

df_equity <- readr::read_csv('Data_equity_2018_clean.csv', 
                             na = c("", "NA", ".", " "),
                             col_names = T,
                             col_types = cols(.default = col_double(), 
                                              Date = col_date(format = "%d/%m/%Y"))) %>%
  dplyr::select(-starts_with('X'))


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

name_country <- colnames(df_equity_clean)[-1] #country names
num_country <- ncol(df_equity_clean) - 1 #num of countries (excl Date)
name_country_pre86 <- name_country[-c(4, 7, 10:14, 17, 19:27, 
                                 29:30, 32:36, 39, 41:44, 
                                 46:47)] #countries with data from before 1986

year_min <- min(lubridate::year(df_equity_clean$Date)) #earliest year
year_max <- max(lubridate::year(df_equity_clean$Date)) #latest year
num_years <- year_max - year_min + 1
year_grid <- year_min:year_max
date_grid <- df_equity$Date[-1]

# Changing index levels to (log) returns

func_log_ret <- function(price_vec)
{
  # The following function accepts a vector of prices and returns a vector of
  # log returns via the formula: r_t = log(p_t) - log(p_{t-1}).
  log_pr <- log(price_vec)
  return(diff(log_pr))
}

df_log_pr <- apply(df_equity_clean[,-1], 2, func_log_ret) %>%
  tibble::as_tibble(.) 

# With log returns
df_equity_clean <- cbind('Date' = date_grid, df_log_pr) %>%
  tibble::as_tibble(.) 

temp_yr <- lubridate::year(df_equity_clean$Date)
df_equity_clean <- df_equity_clean %>%
  tibble::add_column(Year = temp_yr) %>%
  dplyr::select(Date, Year, everything())

# Nesting

nest_df_equity <- df_equity_clean %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)

nest_df_pre86 <- df_equity_clean %>% 
  dplyr::select(c(Date, Year, name_country_pre86)) %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)

# Changing data from wide format to long format
# df_equity_long <- df_equity_clean %>%
#   tidyr::gather(Australia:Venezuela,
#                 key = "Country",
#                 value = "Ind_Ret") %>%
#   dplyr::arrange(Country)

  