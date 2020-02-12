### Results and Analysis for CGR 202002 ###

library(tidyverse)
library(lmtest)
library(sandwich)

name_script_file <- "CGR_202001_equity_alt.R"
source(name_script_file, echo = F) #Compute diversification using original data

summ_stat_div <- apply(Div_ind_full_wide[, - 1], 2, summary) #Compute summary stats

# Figure 1
plot_div <- Div_ind_plot 

# Table 2
file_RHS_common <- readr::read_csv('Panel_CGR_Common_RHS.csv')

RHS_common <- file_RHS_common %>%
  dplyr::select(-c(`GLOBAL SUPPLY CHAIN`, `GDP PER CAPITA`)) %>%
  dplyr::rename('ERM' = `1992 - ERM`, 'EZ' = `2009-10 - EUROZONE`)

# Panel with common RHS factors
panel_common <- RHS_common %>%
  dplyr::full_join(., Div_ind_full_long, by = 'Year') %>%
  dplyr::select(c(Year, Country, Div_Index, everything())) %>%
  dplyr::arrange(Country)

# Nesting data by country
nest_panel_common <- panel_common %>%
  dplyr::group_by(Country) %>%
  tidyr::nest()

# Formula for common factor RHS regressions
form_common <- Div ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + EZ

func_div_ols <- function(df, formula = form_common)
{
  # This function accepts a dataframe of LHS and RHS variables
  # and the regression formula and returns the regression
  # summary
  lhs <- df$Div_Index
  rhs_tib <- df %>%
    dplyr::select(-c(Year, Div_Index))
  
  data_matrix <- data.frame(Div = lhs, rhs_tib)
  
  lm_summary <- summary(lm(data = data_matrix, formula))
  return(lm_summary)
}

# Formula for finding diversification trends
form_trend <- Div ~ Year

func_div_trend_NW <- function(df, formula = form_trend)
{
  # This function computes the linear trend and reports
  # heteroskedasticity and autocorrelation consistent errors
  # according to Newey West
  lhs <- dplyr::select(df, Div_Index)
  rhs <- dplyr::select(df, Year)

  data.matrix <- data.frame(Div = lhs$Div_Index, 
                            Year = rhs$Year)

  lm_div <- lm(data = data.matrix, formula)
  lm_summ <- summary(lm_div)
  vcov_err <- sandwich::NeweyWest(lm_div, lag = 1, 
                                  prewhite = F, 
                                  adjust = T)
  lm_summ$coefficients <- unclass(lmtest::coeftest(lm_div, 
                                                   vcov. = vcov_err))

  return(lm_summ)
}

func_extract_trend_summary <- function(trend_summary)
{
  # This function accepts a summary of trend regression
  # and returns the relevant row results from the 
  # trend regression
  lm_coeff <- trend_summary$coefficients
  name_select <- c('Estimate', 't value', 'Pr(>|t|)')
  
  lm_trend <- lm_coeff[2, dplyr::intersect(name_select, colnames(lm_coeff))]
  
  return(lm_trend)
}

func_extract_ols_summary <- function(lm_summary)
{
  # This function accepts a summary of linear regression
  # and returns the relevant row results from the 
  # regression coefficient matrix
  lm_coeff <- lm_summary$coefficients
  name_select <- c('Estimate', 't value', 'Pr(>|t|)')
  
  lm_ols <- lm_coeff[, dplyr::intersect(name_select, colnames(lm_coeff))]
  
  return(lm_ols)
}

### OLS and Trends ###

func_div_count_NA <- function(df)
{
  return(sum(is.na(df$Div_Index)/nrow(df)))
}

nest_panel_common <- nest_panel_common %>%
  dplyr::mutate('Div_NA_fraction' = purrr::map_dbl(data, func_div_count_NA)) %>%
  dplyr::mutate('summary_trend_NW' = purrr::map(data, func_div_trend_NW)) %>%
  dplyr::filter(Div_NA_fraction < 0.75) %>%
  dplyr::mutate('summary_OLS' = purrr::map(data, func_div_ols),
                'output_OLS' = purrr::map(summary_OLS, func_extract_ols_summary),
                'output_trend_NW' = purrr::map(summary_trend_NW, func_extract_trend_summary))

# temp_trend_NW <- purrr::map(nest_panel_common$summary_trend_NW,
#                             func_extract_trend_summary)
# names(temp_trend_NW) <- name_country_full
# trend_matrix_full_NW <- t(as.data.frame(temp_trend_NW)) # Write out as .csv file

#########################################
### By countries: developed, emerging ###
#########################################

name_country_developed <-  c('Argentina', 'Australia', 'Austria', 
                             'Bahrain', 'Belgium', 'Canada', 'Chile', 
                             'Croatia', 'Cyprus', 'Czech Rep.', 'Denmark', 
                             'Estonia',  'Finland', 'France', 'Germany', 
                             'Greece', 'Hong Kong', 'Hungary', 'Iceland', 
                             'Ireland', 'Israel', 'Italy', 'Japan', 'Kuwait', 
                             'Latvia', 'Lithuania', 'Luxembourg', 'Malta', 
                             'Netherlands', 'New Zealand', 'Norway', 'Poland', 
                             'Portugal', 'Qatar', 'Saudi Arabia', 'Singapore', 
                             'Slovakia', 'Slovenia', 'Spain', 'Sweden', 
                             'Switzerland', 'UAE', 'UK', 'US')

name_country_emerging <- dplyr::setdiff(name_country_full, name_country_developed)

# Developed countries: OLS and trends
nest_panel_common_dev <- nest_panel_common %>%
  dplyr::filter(Country %in% name_country_developed) %>%
  dplyr::select(Country, data, Div_NA_fraction) %>%
  dplyr::mutate('summary_trend_NW' = purrr::map(data, func_div_trend_NW)) %>%
  dplyr::filter(Div_NA_fraction < 0.75) %>%
  dplyr::mutate('summary_OLS' = purrr::map(data, func_div_ols),
                'output_OLS' = purrr::map(summary_OLS, func_extract_ols_summary),
                'output_trend_NW' = purrr::map(summary_trend_NW, func_extract_trend_summary))

# Emerging countries: OLS and trends
nest_panel_common_emerging <- nest_panel_common %>%
  dplyr::filter(Country %in% name_country_emerging) %>%
  dplyr::select(Country, data, Div_NA_fraction) %>%
  dplyr::mutate('summary_trend_NW' = purrr::map(data, func_div_trend_NW)) %>%
  dplyr::filter(Div_NA_fraction < 0.75) %>%
  dplyr::mutate('summary_OLS' = purrr::map(data, func_div_ols),
                'output_OLS' = purrr::map(summary_OLS, func_extract_ols_summary),
                'output_trend_NW' = purrr::map(summary_trend_NW, func_extract_trend_summary))


#######################################
### By year: pre-2000 and post 2000 ###
#######################################

func_pre00 <- function(df)
{
  return(dplyr::filter(df, Year <= 2000))
}

func_post00 <- function(df)
{
  return(dplyr::filter(df, Year > 2000))
}

nest_panel_pre_post <- nest_panel_common %>%
  dplyr::select(Country, data, Div_NA_fraction) %>%
  dplyr::mutate('Pre_2000' = purrr::map(data, func_pre00),
                'Post_2000' = purrr::map(data, func_post00),
                'Post00_trend' = purrr::map(Post_2000, func_div_trend_NW),
                'output_post00_trend' = purrr::map(Post00_trend, func_extract_trend_summary)) 

