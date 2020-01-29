### Results and Analysis for CGR ###

library(tidyverse)
library(lmtest)
library(sandwich)

location_script_file <- "CGR_202001_equity_alt.R"

source(location_script_file, echo = F) # Read and parse original data files

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

# panel_common_pre00 <- panel_common %>%
#   dplyr::filter(Year <= 2000)
# nest_panel_common_pre00 <- panel_common_pre00 %>%
#   dplyr::group_by(Country) %>%
#   tidyr::nest()
# 
# panel_common_post00 <- panel_common %>%
#   dplyr::filter(Year > 2000)
# nest_panel_common_post00 <- panel_common_post00 %>%
#   dplyr::group_by(Country) %>%
#   tidyr::nest()

form_common <- Div ~ TED + VIX + SENT + FEDFUNDS + INTERNET + ERM + EZ

func_div_ols <- function(df, formula = form_common)
{
  lhs <- df$Div_Index
  rhs_tib <- df %>%
    dplyr::select(-c(Year, Div_Index))
  
  data_matrix <- data.frame(Div = lhs, rhs_tib)
  
  lm_summary <- summary(lm(data = data_matrix, formula = form_common))
  return(lm_summary)
}

form_trend <- Div ~ Year

func_div_trend <- function(df, formula = form_trend)
{
  lhs <- dplyr::select(df, Div_Index)
  rhs <- dplyr::select(df, Year)
  
  data_matrix <- data.frame(Div = lhs$Div_Index, Year = rhs$Year)
  
  return(summary(lm(data = data_matrix, formula = form_trend)))
}

func_extract_lm_summary <- function(lm_summary)
{
  lm_coeff <- lm_summary$coefficients
  name_select <- c('Estimate', 't value', 'Pr(>|t|)')
  
  lm_trend <- lm_coeff['Year', dplyr::intersect(name_select, colnames(lm_coeff))]
  
  return(lm_trend)
}

### OLS ###

nest_panel_common <- nest_panel_common %>%
  dplyr::mutate('summary_ols' = purrr::map(data, func_div_ols),
                'summary_trend' = purrr::map(data, func_div_trend))
# nest_panel_common_pre00 <- nest_panel_common_pre00 %>%
#   dplyr::mutate('summary_ols' = purrr::map(data, func_div_ols),
#                 'summary_trend' = purrr::map(data, func_div_trend))
# nest_panel_common_post00 <- nest_panel_common_post00 %>%
#   dplyr::mutate('summary_ols' = purrr::map(data, func_div_ols),
#                 'summary_trend' = purrr::map(data, func_div_trend))


### Trends ###

temp_trend <- sapply(nest_panel_common$summary_trend, 
                      func_extract_lm_summary)
colnames(temp_trend) <- name_country_full
trend_matrix_full <- t(temp_trend)

# temp_trend_pre00 <- sapply(nest_panel_common_pre00$summary_trend,
#                            func_extract_lm_summary)
# colnames(temp_trend_pre00) <- name_country_full
# trend_matrix_pre00 <- t(temp_trend_pre00)
# 
# temp_trend_post00 <- sapply(nest_panel_common_post00$summary_trend,
#                            func_extract_lm_summary)
# colnames(temp_trend_post00) <- name_country_full
# trend_matrix_post00 <- t(temp_trend_post00)

# 
