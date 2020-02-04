### Results and Analysis for CGR 202002 ###

library(tidyverse)
library(lmtest)
library(sandwich)

name_script_file <- "CGR_202001_equity_alt.R"

source(name_script_file, echo = F) # Compute diversification using original data

summ_stat_div <- apply(Div_ind_full_wide[, - 1], 2, summary)

# Figure 1
plot_div <- Div_ind_plot 

# Table 2
file_RHS_common <- readr::read_csv('Panel_CGR_Common_RHS.csv')

RHS_common <- file_RHS_common %>%
  dplyr::select(-c(`GLOBAL SUPPLY CHAIN`, `GDP PER CAPITA`)) %>%
  dplyr::rename('ERM' = `1992 - ERM`, 'EZ' = `2009-10 - EUROZONE`)

panel_common <- RHS_common %>%
  dplyr::full_join(., Div_ind_full_long, by = 'Year') %>%
  dplyr::select(c(Year, Country, Div_Index, everything())) %>%
  dplyr::arrange(Country)
nest_panel_common <- panel_common %>%
  dplyr::group_by(Country) %>%
  tidyr::nest()

form_common <- Div ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + EZ

func_div_ols <- function(df, formula = form_common)
{
  lhs <- df$Div_Index
  rhs_tib <- df %>%
    dplyr::select(-c(Year, Div_Index))
  
  data_matrix <- data.frame(Div = lhs, rhs_tib)
  
  lm_summary <- summary(lm(data = data_matrix, formula))
  return(lm_summary)
}

form_trend <- Div ~ Year

func_div_trend <- function(df, formula = form_trend)
{
  lhs <- dplyr::select(df, Div_Index)
  rhs <- dplyr::select(df, Year)
  
  data_matrix <- data.frame(Div = lhs$Div_Index, Year = rhs$Year)
  
  return(summary(lm(data = data_matrix, formula)))
}

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
  vcov_err <- sandwich::NeweyWest(lm_div, lag = 1, prewhite = F, adjust = T)
  lm_summ$coefficients <- unclass(lmtest::coeftest(lm_div, 
                                                   vcov. = vcov_err))

  return(lm_summ)
}

func_extract_trend_summary <- function(lm_summary)
{
  lm_coeff <- lm_summary$coefficients
  name_select <- c('Estimate', 't value', 'Pr(>|t|)')
  
  #lm_trend <- lm_coeff['Year', dplyr::intersect(name_select, colnames(lm_coeff))]
  lm_trend <- lm_coeff[2, dplyr::intersect(name_select, colnames(lm_coeff))]
  
  return(lm_trend)
}

func_extract_ols_summary <- function(lm_summary)
{
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
  dplyr::mutate('summary_trend' = purrr::map(data, func_div_trend),
                'summary_trend_NW' = purrr::map(data, func_div_trend_NW)) 

nest_panel_common_OLS <- nest_panel_common %>%
  dplyr::filter(Div_NA_fraction < 0.75) %>%
  dplyr::mutate('summary_ols' = purrr::map(data, func_div_ols))

temp_ols <- sapply(nest_panel_common_OLS$summary_ols,
                   func_extract_ols_summary)
names(temp_ols) <- nest_panel_common_OLS$Country # Write out as .csv file

temp_trend_NW <- purrr::map(nest_panel_common$summary_trend_NW,
                            func_extract_trend_summary)
names(temp_trend_NW) <- name_country_full
trend_matrix_full_NW <- t(as.data.frame(temp_trend_NW)) # Write out as .csv file

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
  dplyr::mutate('summary_trend' = purrr::map(data, func_div_trend),
                'summary_trend_NW' = purrr::map(data, func_div_trend_NW))


nest_panel_common_dev_OLS <- nest_panel_common_dev %>%
  dplyr::filter(Div_NA_fraction < 0.75) %>%
  dplyr::mutate('summary_ols' = purrr::map(data, func_div_ols))

temp_ols_dev_OLS <- sapply(nest_panel_common_dev_OLS$summary_ols,
                   func_extract_ols_summary)
names(temp_ols_dev_OLS) <- nest_panel_common_dev_OLS$Country # Write out as .csv file

temp_trend_dev_NW <- purrr::map(nest_panel_common_dev$summary_trend_NW,
                            func_extract_trend_summary)
names(temp_trend_dev_NW) <- nest_panel_common_dev$Country 
trend_matrix_dev_NW <- t(as.data.frame(temp_trend_dev_NW)) # Write out as .csv file


# Emerging countries: OLS and trends
nest_panel_common_emerging <- nest_panel_common %>%
  dplyr::filter(Country %in% name_country_emerging) %>%
  dplyr::select(Country, data) %>%
  dplyr::mutate('summary_trend' = purrr::map(data, func_div_trend),
                'summary_trend_NW' = purrr::map(data, func_div_trend_NW))

# temp_ols_emerg <- sapply(nest_panel_common_emerging$summary_ols,
#                        func_extract_ols_summary)
# names(temp_ols_emerg) <- name_country_emerging # Write out as .csv file

temp_trend_emerg_NW <- purrr::map(nest_panel_common_emerging$summary_trend_NW,
                                func_extract_trend_summary)
names(temp_trend_emerg_NW) <- name_country_emerging
trend_matrix_emerg_NW <- t(as.data.frame(temp_trend_emerg_NW)) # Write out as .csv file


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
                'Post_2000' = purrr::map(data, func_post00))

nest_panel_pre_post_results <- nest_panel_pre_post %>%
  dplyr::select(-data) %>%
  dplyr::mutate('summary_ols_post00' = purrr::map(Post_2000, func_div_ols),
                'summary_trend_post00' = purrr::map(Post_2000, func_div_trend),
                'summary_trend_NW_post00' = purrr::map(Post_2000, func_div_trend_NW))

### Post 2000 ###

temp_ols_post00 <- sapply(nest_panel_pre_post_results$summary_ols_post00,
                   func_extract_ols_summary)
names(temp_ols_post00) <- name_country_full # Write out as .csv file

temp_trend_post00_NW <- purrr::map(nest_panel_pre_post_results$summary_trend_NW_post00,
                                func_extract_trend_summary)
names(temp_trend_post00_NW) <- name_country_full
trend_matrix_post00_NW <- t(as.data.frame(temp_trend_post00_NW)) # Write out as .csv file
