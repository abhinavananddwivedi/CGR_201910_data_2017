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

library(tidyverse)

### Reading the current data file ###
# df_equity <- readr::read_csv('CGR_equity_2019.csv',
#                              na = c("", "NA", ".", " ", "NaN", 'Inf', '-Inf'),
#                              col_names = T,
#                              col_types = cols(.default = col_double(),
#                                               Date = col_date(format = "%d/%m/%Y"))) %>%
#   dplyr::filter(lubridate::year(Date) != 2019) #ignore partial year's observations
# 
# df_equity <- df_equity %>%
#   dplyr::mutate('Year' = lubridate::year(Date)) %>%
#   dplyr::mutate('Month' = lubridate::month(Date)) %>%
#   dplyr::mutate('Day' = lubridate::day(Date)) %>%
#   dplyr::select(Date, Year, Month, Day, everything())

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

func_duplic_NA <- function(vec)
{
  #This function accepts a vector, finds
  #duplicated entries, then replaces them 
  #with NAs
  vec[duplicated(vec)] <- NA
  return(vec)
}

# Find duplicated index values, treat them as missing
df_equity_no_duplic <- df_equity %>%
  dplyr::select(-c(Date, Year)) %>%
  apply(. , 2, func_duplic_NA) %>%
  tibble::as_tibble(.)
  
df_equity <- df_equity_no_duplic %>%
  tibble::add_column('Date' = df_equity$Date, 'Year' = df_equity$Year) %>%
  dplyr::select(Date, Year, everything(.))

num_usable <- 50 #How many usable returns a country ought to have per year?
#num_usable <- 100

name_country <- df_equity %>%
  dplyr::select(-c(Date, Year, Canada_lag, US_lag)) %>%
  colnames(.)

num_country <- length(name_country)

### Removing empty columns ###
func_full_NA_col_killer <- function(data_frame)
{
  # This function kills a data frame's full NA columns
  temp_no_NA_col <- data_frame[, colSums(is.na(data_frame)) < nrow(data_frame)]
  
  return(temp_no_NA_col)
}

# Pre-86 cohort
name_country_pre86 <- df_equity %>%
  dplyr::filter(Year < 1986) %>%
  dplyr::select(-c(Date, Year, Canada_lag, US_lag)) %>%
  func_full_NA_col_killer(.) %>%
  colnames(.)

name_country_regular <- dplyr::setdiff(name_country, name_country_pre86)

# Changing index levels to (log) returns
func_log_ret <- function(price_vec)
{
  #This function accepts a vector of prices 
  #and returns a vector of log returns via 
  #the formula: r_t = log(p_t) - log(p_{t-1})
  
  log_pr <- log(price_vec)
  return(diff(log_pr))
}

df_log_pr <- df_equity %>%
  dplyr::select(-c(Date, Year)) %>%
  apply(., 2, func_log_ret) %>%
  tibble::as_tibble(.)

# Some Inf and NaNs may have crept in due to taking logs
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
                         df_log_pr) %>% 
  tibble::as_tibble(.) 

# Nesting
nest_df_equity <- df_equity_clean %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)

# Only countries with more than n = num_usable returns are 
# allowed in the regression
func_filter_reg_country <- function(df, n = num_usable)
{
  #This function accepts a dataframe, kills its
  #full NA columns, then accepts only those 
  #columns that have more than n = num_usable 
  #nonstale observed returns in one year
  
  df_1 <- df
  
  #Kill fully missing columns
  df_1 <- func_full_NA_col_killer(df_1)
  
  #How many remaining missing values?
  temp_NA <- apply(df_1, 2, function(x){sum(is.na(x))})
  
  temp_usable <- df_1[, temp_NA < (nrow(df_1) - n)]

  return(temp_usable)
}

nest_df_equity_LHS <- nest_df_equity %>%
  dplyr::mutate("LHS_country_clean" = purrr::map(data, func_filter_reg_country)) 

##################################################
### Constructing the RHS data matrix #############
##################################################

nest_df_equity_RHS <- df_equity_clean %>% 
  dplyr::select(c(Date, Year, name_country_pre86, 
                  Canada_lag, US_lag)) %>%
  dplyr::group_by(Year) %>% 
  dplyr::select(-Date) %>%
  tidyr::nest(.)

year_min <- min(nest_df_equity$Year)
year_max <- max(nest_df_equity$Year)
year_min_cohort <- 1986
num_years_full <- year_max - year_min + 1
num_years_cohort <- year_max - year_min_cohort + 1

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

# Joining the LHS and RHS countries and computing covariance matrices
nest_df_equity_clean <- nest_df_equity_LHS %>%
  dplyr::filter(Year >= year_min_cohort) %>%
  dplyr::select(-c(data)) %>%
  dplyr::left_join(., nest_df_equity_RHS, by = 'Year') %>%
  dplyr::rename('RHS_country_data' = data) %>%
  dplyr::mutate('RHS_country_clean' = purrr::map(RHS_country_data, func_NA_filler_df)) %>%
  dplyr::mutate('Cov_matrix' = purrr::map(RHS_country_clean, cov))

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

# Compute eigenvalues, eigenvectors, cumulative shares etc.
nest_df_equity_clean <- nest_df_equity_clean %>%
  dplyr::select(-RHS_country_data) %>%
  dplyr::mutate('Eigen_values' = purrr::map(Cov_matrix, func_eig_val)) %>%
  dplyr::mutate('Eigen_vectors' = purrr::map(Cov_matrix, func_eig_vec)) %>%
  dplyr::mutate('Share' = purrr::map(Eigen_values, func_eigen_share))

# Lagging eigenvectors for computing out-of-sample principal components
nest_df_equity_clean <- nest_df_equity_clean %>% 
  dplyr::select(-c(Cov_matrix, Eigen_values)) %>%
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

# How many eigenvectors needed for 90% variance attribution?
num_pc_90 <-  sapply(nest_df_equity_clean$Share, func_num_pc_90) 
num_pc_equity <- median(num_pc_90)

func_pc_out_90 <- function(df)
{
  #This function accepts a dataframe and returns
  #columns 1 to col_num = num_pc_equity
  
  temp <- df[, 1:num_pc_equity]
  colnames(temp) <- paste0("PC_", 1:num_pc_equity)
  
  return(temp)
}

# Computing out-of-sample principal components
nest_df_equity_clean <- nest_df_equity_clean %>%
  dplyr::mutate('PC_out_sample' = purrr::map2(RHS_country_clean, 
                                              Lag_eigvec, 
                                              function(df1, df2){return(df1*df2)}))

# Take only n = num_pc_equity principal components
nest_df_equity_clean <- nest_df_equity_clean %>%
  dplyr::mutate('PC_out_sample_90' = purrr::map(PC_out_sample, func_pc_out_90)) 

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
  dplyr::select(Year, LHS_country_clean, PC_out_sample_90) %>%
  dplyr::mutate('LHS_country_regular' = purrr::map(LHS_country_clean, 
                                                   func_select_country_regular)) 

### Regressing regular countries' data columns on out of sample PCs ###

# Diagnostics for crashing regressions
# i <- 25
# df_1 <- nest_df_equity_LHS_regular$LHS_country_regular[[i]]
# df_2 <- nest_df_equity_LHS_regular$PC_out_sample_90[[i]]
# apply(df_1, 2, function(x){sum(is.na(x))/length(x)})
# temp_lhs <- as.matrix(df_1)
# temp_rhs <- as.matrix(df_2)
# temp_div <- list(NULL)
# for (j in 1:ncol(temp_lhs))
# {
#   temp_lm <- summary(lm(formula = temp_lhs[,j] ~ temp_rhs))
#   temp_adj <- temp_lm$adj.r.squared
#   temp_adj[temp_adj < 0 ] <- 0
#   temp_div[[j]] <- 100*(1 - temp_adj)
# }
# unlist(temp_div)

#threshold = 0.80

func_lm_adj_rsqr <- function(df_1, df_2)
{
  #This function accepts the data matrix of LHS
  #countries (df1) and the out of sample PC
  #RHS matrix (df2) and returns div = 100 - adj rsqr
  
  temp_lhs <- as.matrix(df_1)
  temp_rhs <- as.matrix(df_2)
  
  # How many missing values?
  temp_temp <- apply(temp_lhs, 2, function(x){sum(is.na(x))/length(x)})
  
  #temp_lhs_clean <- temp_lhs[, temp_temp < prop]
  
  temp_div <- list(NULL)
  
  # Regress each column (country) on RHS
  for (j in 1:ncol(temp_lhs))
  {
    # temp_lm_summary <- summary(lm(formula = temp_lhs_clean[, j] ~ temp_rhs))
    temp_lm_summary <- summary(lm(formula = temp_lhs[, j] ~ temp_rhs))
    temp_adj <- temp_lm_summary$adj.r.squared
    temp_adj[temp_adj < 0] <- 0
    temp_div[[j]] <- 100*(1 - temp_adj)
  }
  
  return(unlist(temp_div))
}

nest_df_equity_LHS_regular <- nest_df_equity_LHS_regular %>%
  dplyr::filter(purrr::map(LHS_country_regular, ncol) > 1) %>% #ignore if single country
  mutate('Div_index' = purrr::map2(LHS_country_regular,
                                   PC_out_sample_90,
                                   func_lm_adj_rsqr))

# func_attach_name <- function(df_1, vec, prop = threshold)
# {
#   temp_2 <- apply(df_1, 2, function(x){sum(is.na(x))/length(x)})
#   temp_3 <- df_1[, temp_2 < prop]
#   names(vec) <- colnames(temp_3)
#   return(vec)
# }

func_attach_name <- function(df_1, vec)
{
  names(vec) <- colnames(df_1)
  return(vec)
}

nest_df_equity_regular_final <- nest_df_equity_LHS_regular %>%
  dplyr::mutate('Div_ind_edit' = purrr::map2(LHS_country_regular, Div_index,
                                             func_attach_name)) %>%
  dplyr::select(c(Year, Div_ind_edit))
 
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
  dplyr::filter(!map_lgl(Eig_vec_lag, is.null)) %>% #ignote null entries
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
    dplyr::select(., dplyr::intersect(df_names, name_country_pre86))
  
  return(temp)
}

nest_df_equity_LHS_pre86 <- nest_df_equity_LHS %>%
  dplyr::mutate('LHS_country_data_pre86' = purrr::map(LHS_country_data,
                                                      func_select_country_pre86))

nest_df_equity_pre86 <- dplyr::left_join(list_unnest_rhs_pre86,
                                nest_df_equity_LHS_pre86,
                                by = 'Year') %>%
  dplyr::select(Year, LHS_country_data_pre86,
                RHS_list_pre86, PC_out_sample)

nest_df_equity_pre86 <- nest_df_equity_pre86 %>%
  dplyr::mutate('PC_out_sample_90' = purrr::map(PC_out_sample,
                                                function(df){df[,1:num_pc_equity]})) 


### Regressing pre86 LHS countries on out of sample PCs ###

nest_df_equity_pre86 <- nest_df_equity_pre86 %>%
  dplyr::mutate('Div_index_pre86' = purrr::map2(LHS_country_data_pre86,
                                         PC_out_sample_90,
                                         func_lm_adj_rsqr))

# Now select the relevant results from pre86 countries 
func_edit_list_names <- function(vec_named)
{
  #This function collects a named vector and edits
  #the names by removing the letters 'Response '  
  names(vec_named) <- substr(names(vec_named), 10, length(names(vec_named)))
  return(vec_named)
}

nest_df_equity_pre86 <- nest_df_equity_pre86 %>%
  dplyr::select(-c(RHS_list_pre86, PC_out_sample)) %>%
  dplyr::mutate('Div_edit_pre86' = purrr::map(Div_index_pre86,
                                              func_edit_list_names)) 


Div_list_pre86 <- list(NULL)

year_seq <- unique(nest_df_equity_pre86$Year)

for (i in 1:length(year_seq))
{
  temp_filter <- nest_df_equity_pre86 %>%
    dplyr::filter(Year == year_seq[i]) %>%
    dplyr::select(Div_edit_pre86) 
  
  temp_temp <- sapply(temp_filter$Div_edit_pre86, rbind) %>%
    diag(.)
  
  names(temp_temp) <- name_country_pre86
  
  Div_list_pre86[[i]] <- temp_temp
  
}

nest_df_equity_pre86_final <- nest_df_equity_RHS_pre86 %>%
  dplyr::filter(Year %in% year_seq) %>%
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


#readr::write_csv(div_ind_final, 'Div_ind_equity.csv')
