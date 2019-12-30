#################################################
### CGR post 201910, data updated till 2018 #####
#################################################

### EQUITY DIVERSIFICATION INDICES ###

#################################################
### Redoing all from scratch ####################
#################################################

#################################################
### Extensive usage of functional programming ###
#################################################

### Libraries ###################################

library(tidyverse)
library(Matrix)

### Reading the current data file ###
df_equity <- readr::read_csv('CGR_equity_2019.csv',
                             na = c("", "NA", ".", " ", "NaN", 'Inf', '-Inf'),
                             col_names = T,
                             col_types = cols(.default = col_double(),
                                              Date = col_date(format = "%d/%m/%Y"))) %>%
  dplyr::filter(lubridate::year(Date) != 2019) #ignore partial year's observations

df_equity <- df_equity %>%
  dplyr::mutate('Year' = lubridate::year(Date)) %>%
  dplyr::mutate('Month' = lubridate::month(Date)) %>%
  dplyr::mutate('Day' = lubridate::day(Date)) %>%
  dplyr::select(Date, Year, Month, Day, everything())

# # The previous dataset
# df_equity <- readr::read_csv('FTS_Data_Equity.csv',
#                              na = c("", "NA", ".", " ", "NaN", 'Inf', '-Inf'),
#                              skip = 3,
#                              col_names = T,
#                              col_types = cols(.default = col_double(),
#                                               Date = col_date(format = "%m/%d/%Y")))
# 
# df_equity <- df_equity %>%
#   dplyr::mutate('Year' = lubridate::year(Date)) %>%
#   dplyr::mutate('Month' = lubridate::month(Date)) %>%
#   dplyr::mutate('Day' = lubridate::day(Date)) %>%
#   dplyr::select(-Date_Number) %>%
#   dplyr::select(Date, Year, Month, Day, everything())

#num_usable <- 50 #How many usable returns a country ought to have per year?
num_usable <- 100
#num_usable <- 75

name_country <- colnames(df_equity)[-c(1,2,3,4)] #country names
num_country <- length(name_country)

### Removing empty columns ###
func_full_NA_col_killer <- function(data_frame)
{
  # This function kills a data frame's full NA columns
  temp_no_NA_col <- data_frame[, colSums(is.na(data_frame)) < nrow(data_frame)]
  
  return(temp_no_NA_col)
}

# The cohort of countries with data available pre-86
name_country_pre86 <- df_equity %>% 
  dplyr::filter(Year < 1986) %>%
  func_full_NA_col_killer(.) %>%
  dplyr::select(-c(Date, Year, Month, Day)) %>%
  colnames(.)

name_country_regular <- setdiff(name_country, name_country_pre86)

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
df_equity_clean <- cbind('Date' = df_equity$Date[-1], 
                         'Year' = df_equity$Year[-1], 
                         'Month' = df_equity$Month[-1],
                         'Day' = df_equity$Day[-1],
                         df_log_pr) %>% 
  tibble::as_tibble(.) 

# Nesting
nest_df_equity <- df_equity_clean %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)

# Only countries with more than 100 usable returns are 
# allowed in the regression
func_filter_reg_country <- function(df, n = num_usable)
{
  #This function accepts a dataframe, kills its
  #full NA columns, then accepts only those 
  #columns that have more than 100 nonstale observed
  #returns in one year
  
  df_1 <- df[, -c(1:3)] #ignore the first 3 columns
  
  #Kill fully missing columns
  df_1 <- func_full_NA_col_killer(df_1)
  
  #How many remaining missing values?
  temp_NA <- apply(df_1, 2, function(x){sum(is.na(x))})
  
  #How many stale entries?
  temp_stale <- nrow(df_1) - apply(df_1, 2, 
                                   function(x){length(unique(x))})
  
  temp_not_usable <- temp_NA + temp_stale
  
  temp_usable <- df_1[, temp_not_usable < nrow(df_1) - n]
  
  temp_return <- cbind(df[,c(1:3)], temp_usable) %>%
    tibble::as_tibble(.)
  
  return(temp_return)
}

nest_df_equity_LHS <- nest_df_equity %>%
  dplyr::mutate("LHS_country_data" = purrr::map(data, func_filter_reg_country)) 

temp_temp <- map(nest_df_equity_LHS$LHS_country_data, func_full_NA_col_killer)

# Final round of full NA column-killing
nest_df_equity_LHS <- nest_df_equity_LHS %>%
  tibble::add_column('LHS_country' = temp_temp) %>%
  dplyr::select(-LHS_country_data) %>%
  dplyr::rename('LHS_country_data' = LHS_country)

##################################################
### Constructing the RHS data matrix #############
##################################################

nest_df_equity_RHS <- df_equity_clean %>% 
  dplyr::select(c(Date, Year, Month, Day, name_country_pre86)) %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)

year_min <- min(nest_df_equity$Year)
year_max <- max(nest_df_equity$Year)
year_min_cohort <- 1986
num_years_full <- year_max - year_min + 1
num_years_cohort <- year_max - year_min_cohort + 1

# Add list of lagged RHS data matrix 
lag_data_RHS <- c(list(NULL), nest_df_equity_RHS$data[1:(num_years_full - 1)])

nest_df_equity_RHS <- nest_df_equity_RHS %>%
  tibble::add_column('Lag_data' = lag_data_RHS)

# Augmenting RHS matrix by lags of Canada and US
country_North_Am <- c('Canada', 'United States', 'US')

func_join_RHS_lag <- function(df_1, df_2)
{
  #This function accepts the original and lagged RHS 
  #data matrices, then selects North American countries
  #from the lagged data matrix, then augments the 
  #original RHS data matrix by the lags of Canada and US
  
  lag_North_Am <- df_2 %>% 
    dplyr::select(Month, Day, 
                  which(colnames(.) %in% country_North_Am))
  
  RHS_full <- dplyr::left_join(df_1, 
                               lag_North_Am, 
                               by = c('Month', 'Day'))
  
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

nest_df_equity_RHS_clean <- nest_df_equity_RHS %>%
  dplyr::filter(Year >= year_min_cohort) %>%
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

nest_df_equity_RHS_clean <- nest_df_equity_RHS_clean %>%
  dplyr::mutate('RHS_country_clean' = purrr::map(RHS_country_data, 
                                                 func_NA_filler_df)) 

nest_df_equity_clean <- nest_df_equity_LHS %>%
  dplyr::filter(Year >= year_min_cohort) %>%
  dplyr::select(-data) %>%
  tibble::add_column('RHS_data' = nest_df_equity_RHS_clean$RHS_country_clean) %>%
  dplyr::mutate('Cov_matrix' = purrr::map(RHS_data, function(df){cov(df)}))

func_eig_val <- function(df)
{
  #This function accepts a (symmetric) 
  #matrix and returns the eigenvalues
  temp_eig <- eigen(df)
  return(temp_eig$values)
}

func_eig_vec <- function(df)
{
  #This function accepts a (symmetric) matrix
  #and returns the matrix of eigenvector columns
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

nest_df_equity_clean <- nest_df_equity_clean %>%
  dplyr::mutate('Eigen_values' = purrr::map(Cov_matrix, func_eig_val)) %>%
  dplyr::mutate('Eigen_vectors' = purrr::map(Cov_matrix, func_eig_vec)) %>%
  dplyr::mutate('Share' = purrr::map(Eigen_values, func_eigen_share)) %>%
  dplyr::select(-c(Cov_matrix, Eigen_values))

# Add list of lagged eigenvector matrix 
nest_df_equity_clean <- nest_df_equity_clean %>% 
  dplyr::mutate('Lag_eigvec' = dplyr::lag(Eigen_vectors))

# How many eigenvectors explain 90% of return variance?
func_num_pc_90 <- function(vec_eig_share)
{
  #This function accepts a vector of cumulative
  #share of variance due to eigenvectors and 
  #returns the number of eigenvectors needed 
  #to explain 90% of the variance in the data
  pc_90 <- which(vec_eig_share >= 0.9)
  
  return(min(pc_90))
}

#How many eigenvectors needed for 90% variance attribution?
num_pc_90 <-  sapply(nest_df_equity_clean$Share, func_num_pc_90) #15 seem enough
num_pc_equity <- 15

func_pc_out_90 <- function(df)
{
  #This function accepts a dataframe and returns
  #columns 1 to num_pc_equity, which is 15 as above
  temp <- df[,1:num_pc_equity]
  colnames(temp) <- paste0("PC_", 1:num_pc_equity)
  
  return(temp)
}

# Out-of-sample principal components
nest_df_equity_clean <- nest_df_equity_clean %>%
  dplyr::mutate('PC_out_sample' = purrr::map2(RHS_data, 
                                              Lag_eigvec, 
                                              function(df1, df2){return(df1*df2)}))

# Take only num_pc_equity = 15 principal components
nest_df_equity_clean <- nest_df_equity_clean %>%
  dplyr::mutate('PC_out_sample_90' = purrr::map(PC_out_sample, func_pc_out_90)) %>%
  dplyr::select(Year, LHS_country_data, PC_out_sample_90)

##########################################################
###### Principal components: for regular countries #######
##########################################################

func_select_country_regular <- function(df)
{
  #This function accepts the data matrix and 
  #returns the submatrix with regular countries
  df_names <- colnames(df)
  
  temp <- df[, dplyr::intersect(df_names, name_country_regular)]
  
  return(temp)
}

nest_df_equity_LHS_regular <- nest_df_equity_clean %>%
  dplyr::mutate('LHS_country_regular' = purrr::map(LHS_country_data, 
                                                   func_select_country_regular)) 

num_country_LHS <- sapply(nest_df_equity_LHS_regular$LHS_country_regular, ncol)

nest_df_equity_LHS_regular <- nest_df_equity_LHS_regular %>%
  tibble::add_column('num_country_LHS' = num_country_LHS)

### Regressing regular countries' data columns on out of sample PCs ###

func_lm_adj_rsqr <- function(df1, df2)
{
  #This function accepts the data matrix of LHS
  #regular countries (df1) and the out of sample PC
  #RHS matrix (df2) and returns the explanatory
  #power (adj rsqr) of regressions
  
  temp_lhs <- as.matrix(df1)
  temp_rhs <- as.matrix(df2)
  
  temp_lm <- lm(formula = temp_lhs ~ temp_rhs)
  temp_lm_summary <- summary(temp_lm)
  
  #Extract the explanatory power from OLS summary
  temp_adj <- sapply(temp_lm_summary, function(x){x$adj.r.squared})
  temp_adj[temp_adj < 0] <- 0
  temp_div <- 100*(1 - temp_adj)
  
  return(temp_div)
  
}


nest_df_equity_LHS_regular <- nest_df_equity_LHS_regular %>%
  dplyr::filter(num_country_LHS > 1) %>%
  mutate('Div_index' = purrr::map2(LHS_country_regular,
                                   PC_out_sample_90,
                                   func_lm_adj_rsqr))


func_edit_name <- function(vec_name)
{
  #Remove the prefix 'Response ' from names
  names(vec_name) <- stringr::str_remove(names(vec_name), 'Response ')
  return(vec_name) 
}

nest_df_equity_regular_final <- nest_df_equity_LHS_regular %>%
  dplyr::mutate('Div_ind_edit' = c(purrr::map(Div_index, func_edit_name))) %>%
  dplyr::select(-c(Div_index, num_country_LHS))
 
func_pick_name <- function(vec_name) {names(vec_name)}

list_unnest_df_equity_regular_final <- nest_df_equity_regular_final %>%
  dplyr::mutate('Country' = purrr::map(Div_ind_edit, func_pick_name)) %>%
  dplyr::select(Year, Div_ind_edit, Country) %>%
  tidyr::unnest(.)

Div_ind_regular_final <- list_unnest_df_equity_regular_final %>%
  tidyr::spread(key = Country, value = Div_ind_edit) 


#######################################################################
##### Computing RHS matrix for pre-86 countries #######################
#######################################################################

temp_rhs_pre86 <- nest_df_equity_RHS_clean$RHS_country_clean

func_rm_col_i <- function(df)
{
  #This function accepts a data frame and 
  #returns a list of dataframes, each one
  #a copy of the input except that each 
  #column is omitted one at a time
  
  temp_list <- list(NULL)
  
  for (i in 1:ncol(df))
  {
    temp_list[[i]] <- df[, -i]
  }
  
  return(temp_list)
}

list_rhs_pre86 <- purrr::map(temp_rhs_pre86, func_rm_col_i)

nest_df_equity_RHS_pre86 <- nest_df_equity_RHS_clean %>%
  tibble::add_column('RHS_list_pre86' = list_rhs_pre86) %>%
  dplyr::select(Year, RHS_country_clean, RHS_list_pre86)

list_cov <- purrr::map(list_rhs_pre86, 
                       function(temp_list){lapply(temp_list, cov)})
list_eigen_val <- purrr::map(list_cov, 
                             function(temp_list){lapply(temp_list, func_eig_val)})
list_eigen_vec <- purrr::map(list_cov, 
                             function(temp_list){lapply(temp_list, func_eig_vec)})
list_eigen_share <- purrr::map(list_eigen_val, 
                             function(temp_list){lapply(temp_list, func_eigen_share)})

nest_df_equity_RHS_pre86 <- nest_df_equity_RHS_pre86 %>%
  tibble::add_column('Share' = list_eigen_share, 
                     'Eig_vec' = list_eigen_vec)

# Shift down eigenvector list for out of sample PCs
nest_df_equity_RHS_pre86 <- nest_df_equity_RHS_pre86 %>% 
  dplyr::mutate('Eig_vec_lag' = dplyr::lag(Eig_vec)) 

#Unnesting now to compute yearly pre-86 out of sample PCs
list_unnest_rhs_pre86 <- nest_df_equity_RHS_pre86 %>%
  dplyr::select(Year, RHS_list_pre86, Eig_vec_lag) %>%
  dplyr::filter(!map_lgl(Eig_vec_lag, is.null)) %>% #ignote null 
  tidyr::unnest(.)

#Multiply RHS matrix in year T by eigenvectors from year T-1
list_unnest_rhs_pre86 <- list_unnest_rhs_pre86 %>%
  dplyr::mutate('PC_out_sample' = purrr::map2(RHS_list_pre86,
                                              Eig_vec_lag,
                                              function(df1, df2){df1*df2}))

## Selecting the pre86 countries in LHS ##

func_select_country_pre86 <- function(df)
{
  #This function accepts the data matrix and 
  #returns the submatrix with pre-86 countries
  df_names <- colnames(df)
  
  temp <- df %>% 
    dplyr::select(., which(df_names %in% name_country_pre86))
  
  return(temp)
}

nest_df_equity_LHS_pre86 <- nest_df_equity_LHS %>%
  dplyr::filter(Year != 1973) %>%
  dplyr::mutate('LHS_country_data_pre86' = purrr::map(LHS_country_data,
                                                      func_select_country_pre86))

nest_df_equity_merge_pre86 <- dplyr::left_join(list_unnest_rhs_pre86,
                                nest_df_equity_LHS_pre86,
                                by = 'Year') %>%
  dplyr::select(Year, LHS_country_data_pre86,
                RHS_list_pre86, PC_out_sample)

nest_df_equity_merge_pre86 <- nest_df_equity_merge_pre86 %>%
  dplyr::mutate('PC_out_sample_90' = purrr::map(PC_out_sample,
                                                function(df){df[,1:15]})) %>%
  dplyr::select(-PC_out_sample)


### Regressing pre86 LHS countries on out of sample PCs ###

nest_df_equity_merge_pre86 <- nest_df_equity_merge_pre86 %>%
  dplyr::mutate('Div_index_pre86' = purrr::map2(LHS_country_data_pre86,
                                         PC_out_sample_90,
                                         func_lm_adj_rsqr))

# Now selecting the relevant results from pre86 countries 
func_edit_list_names <- function(vec_named)
{
  names(vec_named) <- substr(names(vec_named), 10, length(names(vec_named)))
  return(vec_named)
}

nest_df_equity_merge_pre86 <- nest_df_equity_merge_pre86 %>%
  dplyr::mutate('Div_edit_pre86' = purrr::map(Div_index_pre86,
                                              func_edit_list_names)) %>%
  dplyr::select(-Div_index_pre86)


Div_list_pre86 <- list(NULL)

for (i in 1:length(year_seq))
{
  temp_filter <- nest_df_equity_merge_pre86 %>%
    dplyr::filter(Year == year_seq[i]) %>%
    dplyr::select(Div_edit_pre86) 
  
  temp_temp <- sapply(temp_filter$Div_edit_pre86, rbind) %>%
    diag(.)
  
  names(temp_temp) <- name_country_pre86
  
  Div_list_pre86[[i]] <- temp_temp
  
}

nest_df_equity_pre86_final <- nest_df_equity_RHS_pre86 %>%
  tibble::add_column('Div_ind_pre86_final' = Div_list_pre86) %>%
  dplyr::select(Year, Div_ind_pre86_final)

Div_ind_pre86_final <- sapply(nest_df_equity_pre86_final$Div_ind_pre86_final, 
                              rbind) %>% 
  t(.) %>%
  tibble::as_tibble() 

colnames(Div_ind_pre86_final) <- name_country_pre86

Div_ind_pre86_final <- Div_ind_pre86_final %>%
  tibble::add_column('Year' = year_seq) %>%
  dplyr::select(Year, everything())

#################################################################
### Joining the regular and pre86 countries' diversification ####
#################################################################

div_ind_final <- dplyr::full_join(Div_ind_regular_final,
                                  Div_ind_pre86_final,
                                  by = 'Year') %>%
  dplyr::arrange(Year) 


readr::write_csv(div_ind_final, 'Div_ind_equity.csv')
