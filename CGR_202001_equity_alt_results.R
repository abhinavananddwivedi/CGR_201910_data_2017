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

  data_matrix <- data.frame(Div = lhs$Div_Index, 
                            Year = rhs$Year)

  lm_div <- lm(data = data_matrix, formula)
  lm_summ <- summary(lm_div)
  vcov_err <- sandwich::NeweyWest(lm_div, 
                                  lag = 1, 
                                  prewhite = F, 
                                  adjust = T)
  lm_summ$coefficients <- unclass(lmtest::coeftest(lm_div, 
                                                   vcov. = vcov_err))

  return(lm_summ)
}

###############################################################
### Computing linear trend for each country (TABLE 2) #########
###############################################################

func_div_frac_NA <- function(df)
{
  # This function accepts the regression dataframe 
  # and returns the fraction of missing entries 
  # in the diversification index
  return(sum(is.na(df$Div_Index)/nrow(df)))
}

# Compute trend for full sample
nest_panel_common <- nest_panel_common %>%
  dplyr::mutate('Div_NA_fraction' = purrr::map_dbl(data, func_div_frac_NA)) %>%
  dplyr::mutate('summary_trend_NW' = purrr::map(data, func_div_trend_NW))

# temp_2 <- nest_panel_common %>%
#   dplyr::select(-summary_trend_NW) %>%
#   dplyr::filter(Div_NA_fraction < 0.75) %>% #else regressions crash
#   dplyr::mutate('summary_OLS' = purrr::map(data, func_div_ols)) %>%
#   dplyr::select(-data)
# 
# nest_panel_common <- nest_panel_common %>%
#   dplyr::full_join(., temp_2, by = c('Country', 'Div_NA_fraction'))

### By year: pre-2000 and post 2000 ###

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
  dplyr::filter(Div_NA_frac_pre != 1) %>% # else regressions crash
  dplyr::mutate('Pre00_trend' = purrr::map(Pre_2000, func_div_trend_NW)) %>%
  dplyr::select(-Pre_2000)

nest_panel_pre_post <- nest_panel_pre_post %>%
  dplyr::full_join(., temp_pre, by = c('Country', 'Div_NA_frac_pre'))

########################################################################
################ FOR PRINTING OUTPUTS (TABLE 2) ########################
########################################################################

func_trend_print <- function(lm_trend)
{
  lm_coef <- lm_trend$coefficients
  lm_coef_year <- lm_coef['Year', ]
  
  return(lm_coef_year)
}

### Full country set, full years ###
panel_trend_print <- nest_panel_common %>%
  select(Country, summary_trend_NW) %>%
  mutate('Coef_year' = purrr::map(summary_trend_NW, func_trend_print))

print_trend <- panel_trend_print$Coef_year
names(print_trend) <- name_country_full

print_trend_2 <- dplyr::bind_rows(print_trend) %>% t(.)

### Full country set, pre 2000 ###
panel_trend_print_pre <- nest_panel_pre_post %>%
  filter(Div_NA_frac_pre < 0.9) %>% # no regression results otherwise
  select(Country, Pre00_trend) %>%
  mutate('Coef_year' = purrr::map(Pre00_trend, func_trend_print))

print_trend_pre <- panel_trend_print_pre$Coef_year
names(print_trend_pre) <- panel_trend_print_pre$Country

print_trend_pre_2 <- dplyr::bind_rows(print_trend_pre) %>% t(.)

### Full country set, post 2000 ###
panel_trend_print_post <- nest_panel_pre_post %>%
  select(Country, Post00_trend) %>%
  mutate('Coef_year' = purrr::map(Post00_trend, func_trend_print))

print_trend_post <- panel_trend_print_post$Coef_year
names(print_trend_post) <- name_country_full

print_trend_post_2 <- dplyr::bind_rows(print_trend_post) %>% t(.)

##############################################################

##############################################################
## Trend for the subsample of developed countries (TABLE 4) ##
##############################################################

### By countries: developed and non-developed (emerging) ###

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

panel_common_dev <- panel_common %>% dplyr::filter(., Country %in% name_country_developed)
trend_dev <- func_div_trend_NW(panel_common_dev, formula = form_trend)

panel_common_dev_pre <- panel_common_dev %>% dplyr::filter(., Year < 2000)
trend_dev_pre <- func_div_trend_NW(panel_common_dev_pre, formula = form_trend)

panel_common_dev_post <- panel_common_dev %>% dplyr::filter(., Year >= 2000)
trend_dev_post <- func_div_trend_NW(panel_common_dev_post, formula = form_trend)

#############################################################################
## Trend for the subsample of non-developed (emerging) countries (TABLE 4) ##
#############################################################################

name_country_emerging <- dplyr::setdiff(name_country_full, name_country_developed)

panel_common_emerg <- panel_common %>% dplyr::filter(., Country %in% name_country_emerging)
trend_emerg <- func_div_trend_NW(panel_common_emerg, formula = form_trend)

panel_common_emerg_pre <- panel_common_emerg %>% dplyr::filter(., Year < 2000)
trend_emerg_pre <- func_div_trend_NW(panel_common_emerg_pre, formula = form_trend)

panel_common_emerg_post <- panel_common_emerg %>% dplyr::filter(., Year >= 2000)
trend_emerg_post <- func_div_trend_NW(panel_common_emerg_post, formula = form_trend)

########################################################################
################ FOR PRINTING OUTPUTS (TABLE 4) ########################
########################################################################

trend_dev_emerg_pre_post <- rbind('Developed' = func_trend_print(trend_dev),
                                  'Emerging'  = func_trend_print(trend_emerg),
                                  'Developed Pre 2000' = func_trend_print(trend_dev_pre),
                                  'Emerging Pre 2000' = func_trend_print(trend_emerg_pre),
                                  'Developed Post 2000' = func_trend_print(trend_dev_post),
                                  'Emerging Post 2000' = func_trend_print(trend_emerg_post))

#########################################################################


################################################
### Computing OLS for each country (TABLE 7) ###
################################################

### Full sample of countries

nest_panel_common <- nest_panel_common %>%
  dplyr::mutate('summary_OLS' = purrr::map(data, func_div_ols))

# name_country_panel_frontier <- c('Argentina', 'Bahrain', 'Bangladesh', 'Botswana', 
#                            'Bulgaria', "Cote d'Ivoire", 'Croatia', 'Cyprus', 
#                            'Ecuador', 'Estonia', 'Ghana', 'Jamaica', 'Jordan', 
#                            'Kazakhstan', 'Kenya', 'Kuwait', 'Latvia', 'Lebanon', 
#                            'Lithuania', 'Mauritius', 'Namibia', 'Nigeria', 
#                            'Oman', 'Pakistan', 'Panama', 'Qatar', 'Romania', 
#                            'Slovakia', 'Slovenia', 'Sri Lanka', 'Trinidad', 
#                            'Tunisia', 'Ukraine', 'UAE', 'Vietnam', 'Zambia')

####################################################################
### OLS by countries: Developed, emerging and frontier (TABLE 7) ###
####################################################################

name_country_frontier <- c('Bangladesh', 'Botswana', 'Bulgaria', "Cote d'Ivoire", 
                           'Ecuador', 'Ghana', 'Jamaica', 'Jordan',  'Kazakhstan',
                           'Kenya', 'Lebanon', 'Mauritius', 'Namibia', 'Nigeria', 
                           'Oman', 'Pakistan', 'Panama', 'Romania', 'Sri Lanka', 
                           'Trinidad', 'Tunisia', 'Ukraine', 'Vietnam', 'Zambia')

name_country_emerging_alt <- dplyr::setdiff(name_country_full, 
                                            dplyr::union(name_country_developed, 
                                                         name_country_frontier))
# Developed countries 
OLS_dev <- func_div_ols(panel_common_dev) 

# Emerging countries (alternative list)
panel_common_emerg_alt <- dplyr::filter(panel_common, Country %in% name_country_emerging_alt)
OLS_emerg <- func_div_ols(panel_common_emerg_alt) #emerging countries 

# Frontier countries (after removing overlaps with developed countries)
panel_common_frontier <- dplyr::filter(panel_common, Country %in% name_country_frontier)
OLS_frontier <- func_div_ols(panel_common_frontier)

########################################################################
################ FOR PRINTING OUTPUTS (TABLE 7) ########################
########################################################################

OLS_dev_emerg_frontier <- rbind(OLS_dev$coefficients, 
                                OLS_emerg$coefficients, 
                                OLS_frontier$coefficients)

OLS_full_print <- map(nest_panel_common$summary_OLS, 
                      function(ols_summary){return(ols_summary$coefficients)})
names(OLS_full_print) <- name_country_full

########################################################################


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
  plm_fixed <- plm::plm(formula, 
                        data = panel_data, 
                        model = mdl, 
                        type = "HC0", 
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

### Computing panel estimates ###

# Full country sample, full year sample
panel_est_full <- func_panel_est(form_common, panel_common_2)

# Full country sample, Pre-2000
panel_est_pre00 <- func_panel_est(form_common, dplyr::filter(panel_common_2, 
                                                             Year < 2000))

# Full country sample, Post-2000
panel_est_post00 <- func_panel_est(form_common, dplyr::filter(panel_common_2, 
                                                            Year >= 2000))

# Develped country sample, full year sample
panel_est_dev <- func_panel_est(form_common, dplyr::filter(panel_common_2, 
                                                            Country %in% name_country_developed))

# Emerging country sample, full year sample 
panel_est_emerg <- func_panel_est(form_common, dplyr::filter(panel_common_2, 
                                                            Country %in% name_country_emerging))

# Developed country sample, Pre 2000
panel_est_dev_pre <- func_panel_est(form_common, dplyr::filter(panel_common_2, 
                                                               Country %in% name_country_developed &
                                                                 Year < 2000))

# Emerging country sample, Pre 2000
panel_est_emerg_pre <- func_panel_est(form_common, dplyr::filter(panel_common_2,
                                                            Country %in% name_country_emerging &
                                                              Year < 2000))

# Developed country sample, Post 2000
panel_est_dev_post <- func_panel_est(form_common, dplyr::filter(panel_common_2,
                                                                Country %in% name_country_developed &
                                                                  Year >= 2000))

# Emerging country sample, Post 2000
panel_est_emerg_post <- func_panel_est(form_common, dplyr::filter(panel_common_2,
                                                                  Country %in% name_country_emerging &
                                                                    Year >= 2000))

##############################################################
### Panel estimation with common and idiosyncratic factors ###
##############################################################

# Aggregate economic risk
panel_agg_econ <- readr::read_csv('Add_Panel_Econ_Risk.csv') %>%
  dplyr::rename('Agg_econ_risk' = `Aggregate Economic Risk`) %>%
  dplyr::select(Country, Year, Agg_econ_risk)

# Aggregate financial risk
panel_agg_fin <- readr::read_csv('Add_Panel_Fin_Risk.csv') %>%
  dplyr::rename('Agg_fin_risk' = `Aggregate Financial Risk`) %>%
  dplyr::select(Country, Year, Agg_fin_risk)

# Liquidity risk
panel_liq <- readr::read_csv('Add_Panel_Liq.csv') %>%
  dplyr::rename('Country' = country, 'Year' = year)

## Political risk file read
panel_political <- readr::read_csv('Political_Risk_201704.csv')

func_sum_row <- function(data_frame)
{
  # This function accepts a panel data frame and
  # returns its row sums after ignoring the first
  # two columns
   
  temp_data <- data_frame %>%
    dplyr::select(-c(Country, Year)) 
  
  # Compute row sums
  temp_sum <- apply(temp_data, 1, function(vec){return(sum(vec, na.rm = T))})
  
  temp_temp <- data_frame %>%
    tibble::add_column(., 'Agg_pol_risk' = temp_sum) %>%
    dplyr::select(Country, Year, Agg_pol_risk)
  
  return(temp_temp)
}

# Aggregating individual political risk variables
panel_agg_pol_risk <- func_sum_row(panel_political)

# Developmental indicators
panel_dev_indicators <- readr::read_csv('Development_Indicators_201704.csv') %>%
  dplyr::rename('Country' = `Country Name`,
                'Country_internet' = `Internet users (per 100 people)`)
# Nesting by country
nest_panel_dev_indicators <- panel_dev_indicators %>%
  dplyr::group_by(Country) %>%
  tidyr::nest()

year_max = max(panel_common$Year)
year_min = min(panel_common$Year)

func_dev_pc1 <- function(country_df, T = 0.2)
{
  # This function accepts a country dataframe and a tolerance
  # fraction for missing values. It filters years in 1986 to 2012 
  # and computes PC1 from columns having more than 50% non-missing 
  # entries
  
  df <- country_df %>%
    dplyr::filter(Year >= year_min & Year <= year_max) 
  
  col_year <- df$Year
  
  # Missing values in each column
  col_NA <- apply(df, 2, function(vec){sum(is.na(vec))})
  # Select colunms with more than fraction T non-missing values
  df_2 <- df[, col_NA <= T*nrow(df)]
  # Fill residual missing values with medians
  df_3 <- func_NA_med_df(df_2[, -1]) #remove years
  # Compute principal components
  dev_pc <- prcomp(df_3)$x
  dev_pc_1 <- data.frame('Year' = col_year, 'PC1' = dev_pc[, 1])
  # Return 1st PC
  return(dev_pc_1)

}

# Compute dev pc 1 for each country in our sample
nest_panel_dev_indicators <- nest_panel_dev_indicators %>%
  dplyr::filter(Country %in% name_country_full) %>%
  dplyr::mutate('Dev_PC1' = purrr::map(data, func_dev_pc1))
# Collect in panel format
panel_dev_pc1 <- nest_panel_dev_indicators %>%
  dplyr::select(-data) %>%
  tidyr::unnest()

# Country internet (as developmental factor)
panel_country_internet <- panel_dev_indicators %>%
  dplyr::select(Country, Year, Country_internet)

### COMPUTE ALSO THE FIRST DEVELOPMENTAL PRINCIPAL COMPONENT ###

# Joining the panels of idiosyncratic variables #
panel_idio <- panel_agg_econ %>%
  dplyr::full_join(., panel_agg_fin, by = c('Country', 'Year')) %>%
  dplyr::full_join(., panel_agg_pol_risk, by = c('Country', 'Year')) %>%
  dplyr::full_join(., panel_liq, by = c('Country', 'Year')) %>%
  dplyr::full_join(., panel_country_internet, by = c('Country', 'Year')) %>%
  dplyr::full_join(., panel_dev_pc1, by = c('Country', 'Year'))

# Joining the common and idiosyncratic panels together
panel_common_idio <- panel_common_2 %>%
  dplyr::left_join(., panel_idio, by = c('Country', 'Year'))

###############
### TABLE 8 ###
###############

### Replacing global internet with country internet 
form_common_country_internet <- Div ~ TED + VIX + SENT + FEDFUNDS + ERM + EZ + Country_internet

panel_est_common_country_internet <- func_panel_est(formula = form_common_country_internet,
                                                    panel_data = panel_common_idio)

### Replacing global internet with developmental PC1
form_common_dev_pc1 <- Div ~ TED + VIX + SENT + FEDFUNDS + ERM + EZ + PC1

panel_est_common_dev_pc1 <- func_panel_est(formula = form_common_dev_pc1, 
                                           panel_data = panel_common_idio)

###############
### TABLE 9 ###
###############

### With risks only: economic, political, financial, liquidity 
form_common_idio <- Div ~ TED + VIX + SENT + FEDFUNDS + 
  ERM + EZ + Agg_econ_risk + Agg_fin_risk + Agg_pol_risk + Liq_risk

panel_est_common_idio <- func_panel_est(formula = form_common_idio, panel_data = panel_common_idio)

### With risks: economic, political, financial, liquidity + Country internet
form_common_idio_country_internet <- Div ~ TED + VIX + SENT + FEDFUNDS + 
  ERM + EZ + Agg_econ_risk + Agg_fin_risk + Agg_pol_risk + Liq_risk + Country_internet

panel_est_common_idio_country_internet <- func_panel_est(formula = form_common_idio_country_internet, 
                                                         panel_data = panel_common_idio)

### With risks: economic, political, financial, liquidity + Developmental PC1
form_common_idio_dev_pc1 <- Div ~ TED + VIX + SENT + FEDFUNDS + 
  ERM + EZ + Agg_econ_risk + Agg_fin_risk + Agg_pol_risk + Liq_risk + PC1

panel_est_common_idio_dev_pc1 <- func_panel_est(formula = form_common_idio_dev_pc1, 
                                                panel_data = panel_common_idio)

################
### TABLE 10 ###
################

# Developed countries only
panel_est_common_idio_country_internet_dev <- func_panel_est(formula = form_common_idio_country_internet, 
                                                             panel_data = dplyr::filter(panel_common_idio,
                                                                                        Country %in% name_country_developed))

# Pre-2000
panel_est_common_idio_country_internet_pre <- func_panel_est(formula = form_common_idio_country_internet, 
                                                             panel_data = dplyr::filter(panel_common_idio,
                                                                                        Year < 2000))

# Post-2000
panel_est_common_idio_country_internet_post <- func_panel_est(formula = form_common_idio_country_internet, 
                                                             panel_data = dplyr::filter(panel_common_idio,
                                                                                        Year >= 2000))
