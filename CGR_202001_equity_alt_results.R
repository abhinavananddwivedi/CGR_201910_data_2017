### Results and Analysis for CGR 202002 ###

library(tidyverse)
library(lmtest)
library(sandwich)
library(plm)

name_script_file <- "CGR_202001_equity_alt.R"
source(name_script_file, echo = F) #Compute diversification using original daily index data

summ_stat_div <- apply(Div_ind_full_wide[, - 1], 2, summary) #Compute summary stats

# Figure 1
plot_div <- Div_ind_plot 

file_RHS_common <- readr::read_csv('Panel_CGR_Common_RHS.csv')

RHS_common <- file_RHS_common %>%
  dplyr::select(-c(`GLOBAL SUPPLY CHAIN`, `GDP PER CAPITA`)) %>%
  dplyr::rename('ERM' = `1992 - ERM`, 'EZ' = `2009-10 - EUROZONE`)

# Panel with common RHS factors
panel_common <- RHS_common %>%
  dplyr::full_join(., Div_ind_full_long_2, by = 'Year') %>%
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

################################
### Computing OLS and Trends ###
################################

func_div_frac_NA <- function(df)
{
  # This function accepts the regression dataframe 
  # and returns the fraction of missing entries 
  # in the diversification index
  return(sum(is.na(df$Div_Index)/nrow(df)))
}

# Compute trend and OLS for full sample
nest_panel_common <- nest_panel_common %>%
  dplyr::mutate('Div_NA_fraction' = purrr::map_dbl(data, func_div_frac_NA)) %>%
  dplyr::mutate('summary_trend_NW' = purrr::map(data, func_div_trend_NW))

temp_2 <- nest_panel_common %>%
  dplyr::select(-summary_trend_NW) %>%
  dplyr::filter(Div_NA_fraction < 0.75) %>% #else regressions crash
  dplyr::mutate('summary_OLS' = purrr::map(data, func_div_ols)) %>%
  dplyr::select(-data)

nest_panel_common <- nest_panel_common %>%
  dplyr::full_join(., temp_2, by = c('Country', 'Div_NA_fraction'))

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
  dplyr::select(Country, data) %>%
  dplyr::mutate('Pre_2000' = purrr::map(data, func_pre00),
                'Post_2000' = purrr::map(data, func_post00),
                'Div_NA_frac_pre' = purrr::map_dbl(Pre_2000, func_div_frac_NA),
                'Post00_trend' = purrr::map(Post_2000, func_div_trend_NW)) %>%
  dplyr::select(-data)

temp_pre <- nest_panel_pre_post %>%
  dplyr::select(c(Country, Pre_2000, Div_NA_frac_pre)) %>%
  dplyr::filter(Div_NA_frac_pre < 0.9) %>% # else regressions crash
  dplyr::mutate('Pre00_trend' = purrr::map(Pre_2000, func_div_trend_NW)) %>%
  dplyr::select(-Pre_2000)

nest_panel_pre_post <- nest_panel_pre_post %>%
  dplyr::full_join(., temp_pre, by = c('Country', 'Div_NA_frac_pre'))
  

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
  dplyr::mutate('summary_trend_NW' = purrr::map(data, func_div_trend_NW)) 

temp_dev_OLS <- nest_panel_common_dev %>%
  dplyr::select(-summary_trend_NW) %>%
  dplyr::filter(Div_NA_fraction < 0.75) %>% # else regressions crash
  dplyr::mutate('summary_OLS' = purrr::map(data, func_div_ols)) %>%
  dplyr::select(-data)

nest_panel_common_dev <- nest_panel_common_dev %>%
  dplyr::full_join(., temp_dev_OLS, by = c('Country', 'Div_NA_fraction')) 

# Emerging countries: OLS and trends
nest_panel_common_emerging <- nest_panel_common %>%
  dplyr::filter(Country %in% name_country_emerging) %>%
  dplyr::select(Country, data, Div_NA_fraction) %>%
  dplyr::mutate('summary_trend_NW' = purrr::map(data, func_div_trend_NW)) 

temp_emerg_OLS <- nest_panel_common_emerging %>%
  dplyr::select(-summary_trend_NW) %>%
  dplyr::filter(Div_NA_fraction < 0.75) %>% # else regressions crash
  dplyr::mutate('summary_OLS' = purrr::map(data, func_div_ols)) %>%
  dplyr::select(-data)

nest_panel_common_emerging <- nest_panel_common_emerging %>%
  dplyr::full_join(., temp_emerg_OLS, by = c('Country', 'Div_NA_fraction'))


### Pre- and post-2000 trends in developed and emerging ###

## Developed ##

nest_panel_common_dev <- nest_panel_common_dev %>%
  dplyr::mutate('Pre_2000' = purrr::map(data, func_pre00),
                'Post_2000' = purrr::map(data, func_post00),
                'Div_NA_frac_pre' = purrr::map_dbl(Pre_2000, func_div_frac_NA),
                'Post00_trend' = purrr::map(Post_2000, func_div_trend_NW))

temp_pre_dev <- nest_panel_common_dev %>%
  dplyr::select(c(Country, Pre_2000, Div_NA_frac_pre)) %>%
  dplyr::filter(Div_NA_frac_pre < 0.9) %>% # else regressions crash
  dplyr::mutate('Pre00_trend' = purrr::map(Pre_2000, func_div_trend_NW)) %>%
  dplyr::select(-Pre_2000)

nest_panel_common_dev <- nest_panel_common_dev %>%
  dplyr::full_join(., temp_pre_dev, by = c('Country', 'Div_NA_frac_pre'))

## Emerging ##

nest_panel_common_emerging <- nest_panel_common_emerging %>%
  dplyr::mutate('Pre_2000' = purrr::map(data, func_pre00),
                'Post_2000' = purrr::map(data, func_post00),
                'Div_NA_frac_pre' = purrr::map_dbl(Pre_2000, func_div_frac_NA),
                'Post00_trend' = purrr::map(Post_2000, func_div_trend_NW))

temp_pre_emerg <- nest_panel_common_emerging %>%
  dplyr::select(c(Country, Pre_2000, Div_NA_frac_pre)) %>%
  dplyr::filter(Div_NA_frac_pre < 0.9) %>% # else regressions crash
  dplyr::mutate('Pre00_trend' = purrr::map(Pre_2000, func_div_trend_NW)) %>%
  dplyr::select(-Pre_2000)

nest_panel_common_emerging <- nest_panel_common_emerging %>%
  dplyr::full_join(., temp_pre_emerg, by = c('Country', 'Div_NA_frac_pre'))


func_extract_trend_summary <- function(trend_summary)
{
  # This function accepts a summary of trend regression
  # and returns the relevant row and column results from the 
  # trend regression
  lm_coeff <- trend_summary$coefficients
  name_select <- c('Estimate', 't value', 'Pr(>|t|)')
  
  # Ignore the first row for intercept
  lm_trend <- lm_coeff[2, dplyr::intersect(name_select, colnames(lm_coeff))]
  
  return(lm_trend)
}

func_extract_ols_summary <- function(lm_summary)
{
  # This function accepts a summary of linear regression
  # and returns the relevant column results from the 
  # regression coefficient matrix
  lm_coeff <- lm_summary$coefficients
  name_select <- c('Estimate', 't value', 'Pr(>|t|)')
  
  lm_ols <- lm_coeff[, dplyr::intersect(name_select, colnames(lm_coeff))]
  
  return(lm_ols)
}



####################################
### Panel estimation begins here ###
####################################

func_panel_est <- function(formula, panel_data, mdl = "within")
{
  # This function accepts a formula, panel data matrix and model spec
  # and returns the summary of an unbalanced, fixed-effects panel regression
  # with clustered robust standard errors with clustering at both 
  # Country and Year levels
  
  panel_data <- panel_data %>%
    dplyr::rename('Div' = Div_Index)
  
  # Panel estimation with fixed effects
  plm_fixed <- plm::plm(formula, data = panel_data, 
                        model = mdl, type = "HC0", 
                        effect = "individual")
  
  # Robust, clustered standard errors
  vcov_err <- plm::vcovDC(plm_fixed) #Double clustering
  
  plm_fixed_robust <- lmtest::coeftest(plm_fixed, vcov. = vcov_err)
  
  plm_out <- summary(plm_fixed)
  
  # Include robust clustered errors
  plm_out$coefficients <- unclass(plm_fixed_robust) 
  
  return(plm_out)
}

# Convert to 'item-year' columns format for panel data in plm
panel_common_2 <- panel_common %>%
  dplyr::select(c(Country, Year, everything())) 

panel_est_full <- func_panel_est(form_common, panel_common_2)







