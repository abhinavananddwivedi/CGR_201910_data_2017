### Results and Analysis for CGR 202003 with updated data till 2018 ###

library(tidyverse)
library(lmtest)
library(sandwich)
library(plm)

name_script_file <- "CGR_202003_bond_alt.R"
source(name_script_file, echo = F) #Compute diversification using original daily index data

summ_stat_div <- apply(Div_ind_full_wide[, - 1], 2, summary) #Compute summary stats

# Figure 1
x_reg <- Div_ind_full_wide$Year
y_reg <- Div_ind_full_wide$Div_world_mean

Fig_1_trend_bond <- plot(x_reg, y_reg, type = 'l', xlab = 'Years', ylab = 'Diversification')
                    grid()
                    abline(lm(y_reg ~ x_reg), lty = 3)

# file_RHS_common <- readr::read_csv('Panel_CGR_Common_RHS.csv')

# RHS_common <- file_RHS_common %>%
#   dplyr::select(-c(`GLOBAL SUPPLY CHAIN`, `GDP PER CAPITA`)) %>%
#   dplyr::rename('ERM' = `1992 - ERM`, 'EZ' = `2009-10 - EUROZONE`)


file_RHS_common <- readr::read_csv('Panel_bond_updated_2018.csv')

RHS_common <- file_RHS_common %>%
  dplyr::select(Year, Country, TED, VIX, SENT, FEDFUNDS, INTERNET, ERM, Euro)


# Panel with common RHS factors
panel_common <- RHS_common %>%
  dplyr::full_join(., Div_ind_full_long, by = c('Year', 'Country')) %>%
  dplyr::select(c(Year, Country, Div_Index, everything())) %>%
  dplyr::rename('EZ' = Euro) %>%
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
  dplyr::filter(Div_NA_fraction < 1) %>%
  dplyr::mutate('summary_trend_NW' = purrr::map(data, func_div_trend_NW))


### By year: pre-2000 and post 2000 ###

func_pre00 <- function(df)
{
  return(dplyr::filter(df, Year <= 2000))
}

func_post00 <- function(df)
{
  return(dplyr::filter(df, Year > 2000))
}
func_period_1 <- function(df)
{
  return(dplyr::filter(df, Year < 2007))
}
func_period_2 <- function(df)
{
  return(dplyr::filter(df, Year >= 2007 & Year <= 2012))
}
func_period_3 <- function(df)
{
  return(dplyr::filter(df, Year > 2012))
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

########################################
#### PRINT TABLE 2: Full ###############
########################################

world_trend_full <- Div_ind_full_wide %>%
  dplyr::select(Year, Div_world_mean)

world_trend_full <- data.frame('World' = summary(lm(data = world_trend_full, 
                                                    formula = Div_world_mean ~ Year))$coefficients['Year',])

print_table_2_full <- sapply(c(world_trend_full, print_trend), rbind) %>% t()

print_table_2_full <- data.frame(print_table_2_full) %>%
  tibble::rownames_to_column(., var = 'Country') %>%
  tibble::as_tibble() 

names(print_table_2_full) <- c('Country', 'Estimate', 'Std error', 'T stats', 'p value')
####################
####################
####################


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
######## Based on 3 subperiods: P1, P2, P3 ###################
##############################################################

nest_panel_periods <- nest_panel_common %>%
  dplyr::select(Country, data) %>%
  dplyr::mutate('P1' = purrr::map(data, func_period_1),
                'P2' = purrr::map(data, func_period_2),
                'P3' = purrr::map(data, func_period_3),
                'NA_frac_P1' = purrr::map_dbl(P1, func_div_frac_NA),
                'NA_frac_P2' = purrr::map_dbl(P2, func_div_frac_NA),
                'NA_frac_P3' = purrr::map_dbl(P3, func_div_frac_NA)) %>%
  dplyr::select(-data)

### Period 1 trends ###
nest_panel_P1 <- nest_panel_periods %>%
  dplyr::select(Country, P1, NA_frac_P1) %>%
  dplyr::filter(NA_frac_P1 < 1) %>%
  dplyr::mutate('trend_P1' = purrr::map(P1, func_div_trend_NW))
### Period 2 trends ###
nest_panel_P2 <- nest_panel_periods %>%
  dplyr::select(Country, P2, NA_frac_P2) %>%
  dplyr::filter(NA_frac_P2 < 1) %>%
  dplyr::mutate('trend_P2' = purrr::map(P2, func_div_trend_NW))
### Period 3 trends ###
nest_panel_P3 <- nest_panel_periods %>%
  dplyr::select(Country, P3, NA_frac_P3) %>%
  dplyr::filter(NA_frac_P3 < 1) %>%
  dplyr::mutate('trend_P3' = purrr::map(P3, func_div_trend_NW))


###############################################################
############ Print table 2 ####################################
###############################################################

trend_data <- tibble::tibble('Year' = Div_ind_full_wide$Year, 
                             'World_mean' = Div_ind_full_wide$Div_world_mean)


#### PERIOD 1 ####
panel_trend_P1 <- nest_panel_P1 %>%
  dplyr::select(Country, trend_P1) %>%
  dplyr::mutate('Coef_year' = purrr::map(trend_P1, func_trend_print))

print_trend_P1 <- panel_trend_P1$Coef_year
names(print_trend_P1) <- nest_panel_P1$Country

world_trend_P1 <- Div_ind_full_wide %>%
  dplyr::filter(Year < 2007) %>%
  dplyr::select(Year, Div_world_mean)

world_trend_P1 <- data.frame('World' = summary(lm(data = dplyr::filter(trend_data, Year < 2007), 
                                                  formula = World_mean ~ Year))$coefficients['Year',])

########################################
#### PRINT TABLE 2: Equity Period 1 ####
########################################
print_table_2_P1 <- sapply(c(world_trend_P1, print_trend_P1), rbind) %>% t()

print_table_2_P1 <- data.frame(print_table_2_P1) %>%
  tibble::rownames_to_column(., var = 'Country') %>%
  tibble::as_tibble() 

names(print_table_2_P1) <- c('Country', 'Estimate', 'Std error', 'T stats', 'p value')
####################
####################
####################

#### PERIOD 2 ####
panel_trend_P2 <- nest_panel_P2 %>%
  dplyr::filter(NA_frac_P2 < 0.8) %>%
  dplyr::select(Country, trend_P2) %>%
  dplyr::mutate('Coef_year' = purrr::map(trend_P2, func_trend_print))

print_trend_P2 <- panel_trend_P2$Coef_year
names(print_trend_P2) <- panel_trend_P2$Country

world_trend_P2 <- Div_ind_full_wide %>%
  dplyr::filter(Year >= 2007 & Year <= 2012) %>%
  dplyr::select(Year, Div_world_mean)

world_trend_P2 <- data.frame('World' = summary(lm(data = dplyr::filter(trend_data, Year >= 2007 & Year <= 2012), 
                                                  formula = World_mean ~ Year))$coefficients['Year',])

########################################
#### PRINT TABLE 2: Equity Period 2 ####
########################################
print_table_2_P2 <- sapply(c(world_trend_P2, print_trend_P2), rbind) %>% t()

print_table_2_P2 <- data.frame(print_table_2_P2) %>%
  tibble::rownames_to_column(., var = 'Country') %>%
  tibble::as_tibble() 

names(print_table_2_P2) <- c('Country', 'Estimate', 'Std error', 'T stats', 'p value')


####################

#### PERIOD 3 ####
panel_trend_P3 <- nest_panel_P3 %>%
  dplyr::filter(NA_frac_P3 < 0.8) %>%
  dplyr::select(Country, trend_P3) %>%
  dplyr::mutate('Coef_year' = purrr::map(trend_P3, func_trend_print))

print_trend_P3 <- panel_trend_P3$Coef_year
names(print_trend_P3) <- panel_trend_P3$Country

world_trend_P3 <- Div_ind_full_wide %>%
  dplyr::filter(Year > 2012) %>%
  dplyr::select(Year, Div_world_mean)

world_trend_P3 <- data.frame('World' = summary(lm(data = dplyr::filter(trend_data, Year > 2012), 
                                                  formula = World_mean ~ Year))$coefficients['Year',])

########################################
#### PRINT TABLE 2: Equity Period 3 ####
########################################
print_table_2_P3 <- sapply(c(world_trend_P3, print_trend_P3), rbind) %>% t()

print_table_2_P3 <- data.frame(print_table_2_P3) %>%
  tibble::rownames_to_column(., var = 'Country') %>%
  tibble::as_tibble() 

names(print_table_2_P3) <- c('Country', 'Estimate', 'Std error', 'T stats', 'p value')





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

##### Period-wise trends #####

panel_common_dev_p1 <- panel_common_dev %>% dplyr::filter(., Year < 2007 & Year >= 1986)
trend_dev_p1 <- func_div_trend_NW(panel_common_dev_p1, formula = form_trend)
panel_common_dev_p2 <- panel_common_dev %>% dplyr::filter(., Year <= 2012 & Year >= 2007)
trend_dev_p2 <- func_div_trend_NW(panel_common_dev_p2, formula = form_trend)
panel_common_dev_p3 <- panel_common_dev %>% dplyr::filter(., Year > 2012)
trend_dev_p3 <- func_div_trend_NW(panel_common_dev_p3, formula = form_trend)

#############################################################################
## Trend for the subsample of non-developed (emerging) countries (TABLE 4) ##
#############################################################################

name_country_emerging <- dplyr::setdiff(name_country_full, name_country_developed)

panel_common_emerg <- panel_common %>% dplyr::filter(., Country %in% name_country_emerging)
trend_emerg <- func_div_trend_NW(panel_common_emerg, formula = form_trend)

panel_common_emerg_pre <- panel_common_emerg %>% dplyr::filter(., Year < 2000)
# trend_emerg_pre <- func_div_trend_NW(panel_common_emerg_pre, formula = form_trend)

panel_common_emerg_post <- panel_common_emerg %>% dplyr::filter(., Year >= 2000)
trend_emerg_post <- func_div_trend_NW(panel_common_emerg_post, formula = form_trend)

##### Period-wise trends #####

panel_common_emerg_p1 <- panel_common_emerg %>% dplyr::filter(., Year < 2007 & Year >= 1986)
trend_emerg_p1 <- func_div_trend_NW(panel_common_emerg_p1, formula = form_trend)
panel_common_emerg_p2 <- panel_common_emerg %>% dplyr::filter(., Year <= 2012 & Year >= 2007)
trend_emerg_p2 <- func_div_trend_NW(panel_common_emerg_p2, formula = form_trend)
panel_common_emerg_p3 <- panel_common_emerg %>% dplyr::filter(., Year > 2012)
trend_emerg_p3 <- func_div_trend_NW(panel_common_emerg_p3, formula = form_trend)

########################################################################
################ FOR PRINTING OUTPUTS (TABLE 4) ########################
########################################################################

trend_dev_emerg_pre_post <- rbind('Developed' = func_trend_print(trend_dev),
                                  'Emerging'  = func_trend_print(trend_emerg),
                                  'Developed Pre 2000' = func_trend_print(trend_dev_pre),
#                                  'Emerging Pre 2000' = func_trend_print(trend_emerg_pre),
                                  'Developed Post 2000' = func_trend_print(trend_dev_post),
                                  'Emerging Post 2000' = func_trend_print(trend_emerg_post))

table_4_periods <- rbind('Developed' = func_trend_print(trend_dev),
                         'Emerging'  = func_trend_print(trend_emerg),
                         'Developed_period_1' = func_trend_print(trend_dev_p1),
                         'Developed_period_2' = func_trend_print(trend_dev_p2),
                         'Developed_period_3' = func_trend_print(trend_dev_p3),
                         'Emerging_period_1' = func_trend_print(trend_emerg_p1),
                         'Emerging_period_2' = func_trend_print(trend_emerg_p2),
                         'Emerging_period_3' = func_trend_print(trend_emerg_p3))

table_4_print <- table_4_periods %>%
  as.data.frame() %>%
  tibble::rownames_to_column(., var = 'Sample')

#########################################################################


################################################
### Computing OLS for each country (TABLE 7) ###
################################################

### Full sample of countries

nest_panel_common_ols <- nest_panel_common %>%
  dplyr::mutate('data_no_full_NA' = purrr::map(data, func_rm_full_NA)) %>%
  dplyr::mutate('num_col' = purrr::map_dbl(data_no_full_NA, ncol)) %>%
  dplyr::filter(num_col == 9) %>%
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

# name_country_frontier <- c('Bangladesh', 'Botswana', 'Bulgaria', "Cote d'Ivoire", 
#                            'Ecuador', 'Ghana', 'Jamaica', 'Jordan',  'Kazakhstan',
#                            'Kenya', 'Lebanon', 'Mauritius', 'Namibia', 'Nigeria', 
#                            'Oman', 'Pakistan', 'Panama', 'Romania', 'Sri Lanka', 
#                            'Trinidad', 'Tunisia', 'Ukraine', 'Vietnam', 'Zambia')
# 
# name_country_emerging_alt <- dplyr::setdiff(name_country_full, 
#                                             dplyr::union(name_country_developed, 
#                                                          name_country_frontier))
# Developed countries 
# OLS_dev <- func_div_ols(panel_common_dev) 

# Emerging countries (alternative list)
# panel_common_emerg_alt <- dplyr::filter(panel_common, Country %in% name_country_emerging_alt)
# OLS_emerg <- func_div_ols(panel_common_emerg_alt) #emerging countries 

# Frontier countries (after removing overlaps with developed countries)
# panel_common_frontier <- dplyr::filter(panel_common, Country %in% name_country_frontier)
# OLS_frontier <- func_div_ols(panel_common_frontier)

########################################################################
################ FOR PRINTING OUTPUTS (TABLE 7) ########################
########################################################################

# OLS_dev_emerg_frontier <- rbind(OLS_dev$coefficients, 
#                                 OLS_emerg$coefficients, 
#                                 OLS_frontier$coefficients)

# OLS_dev_emerg <- rbind(OLS_dev$coefficients, OLS_emerg$coefficients)



OLS_full_print <- map(nest_panel_common_ols$summary_OLS, 
                      function(ols_summary){return(ols_summary$coefficients)})
# names(OLS_full_print) <- name_country_full
names(OLS_full_print) <- nest_panel_common_ols$Country

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

#############################################################
############ PRINTING TABLE 7 ###############################
#############################################################

# Full
data_ols <- panel_common_2 %>%
  dplyr::rename('Div' = Div_Index)

table_7_ols <- plm::plm(data = data_ols, formula = form_common,
                        model = 'pooling') %>% summary()

print_table_7 <- data.frame(table_7_ols$coefficients) %>%
  tibble::rownames_to_column(., var = 'Variables')

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
# panel_est_emerg_pre <- func_panel_est(form_common, dplyr::filter(panel_common_2,
#                                                                  Country %in% name_country_emerging &
#                                                                    Year < 2000))

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
# panel_agg_econ <- readr::read_csv('Add_Panel_Econ_Risk.csv') %>%
#   dplyr::rename('Agg_econ_risk' = `Aggregate Economic Risk`) %>%
#   dplyr::select(Country, Year, Agg_econ_risk)

panel_agg_econ <- readr::read_csv('Panel_risk_economic_updated_2018.csv') %>%
  dplyr::rename('Agg_econ_risk' = `Aggregate Economic Risk`) %>%
  dplyr::select(Country, Year, Agg_econ_risk)

# Aggregate financial risk
# panel_agg_fin <- readr::read_csv('Add_Panel_Fin_Risk.csv') %>%
#   dplyr::rename('Agg_fin_risk' = `Aggregate Financial Risk`) %>%
#   dplyr::select(Country, Year, Agg_fin_risk)

panel_agg_fin <- readr::read_csv('Panel_risk_financial_updated_2018.csv') %>%
  dplyr::rename('Agg_fin_risk' = `Aggregate Financial Risk`) %>%
  dplyr::select(Country, Year, Agg_fin_risk)

# Liquidity risk
panel_liq <- readr::read_csv('Panel_risk_liq_bond_updated_2018.csv') %>%
  dplyr::rename('Country' = country, 'Year' = year, 'Agg_liq_risk' = index)

## Political risk file read
# panel_political <- readr::read_csv('Political_Risk_201704.csv')

panel_agg_pol_risk <- readr::read_csv('Panel_risk_political_updated_2018.csv') %>%
  dplyr::rename('Agg_pol_risk' = `Aggregate Political Risk`) %>%
  dplyr::select(Country, Year, Agg_pol_risk)


# func_sum_row <- function(data_frame)
# {
#   # This function accepts a panel data frame and
#   # returns its row sums after ignoring the first
#   # two columns
#   
#   temp_data <- data_frame %>%
#     dplyr::select(-c(Country, Year)) 
#   
#   # Compute row sums
#   temp_sum <- apply(temp_data, 1, function(vec){return(sum(vec, na.rm = T))})
#   
#   temp_temp <- data_frame %>%
#     tibble::add_column(., 'Agg_pol_risk' = temp_sum) %>%
#     dplyr::select(Country, Year, Agg_pol_risk)
#   
#   return(temp_temp)
# }
# 
# # Aggregating individual political risk variables
# panel_agg_pol_risk <- func_sum_row(panel_political)

# Developmental indicators
# panel_dev_indicators <- readr::read_csv('Development_Indicators_201704.csv') %>%
#   dplyr::rename('Country' = `Country Name`,
#                 'Country_internet' = `Internet users (per 100 people)`)

dev_ATM <- readr::read_csv('dev_ATM.csv', col_types = cols(.default = col_double()))
dev_broadband <- readr::read_csv('dev_Broadband.csv', col_types = cols(.default = col_double()))
dev_cell <- readr::read_csv('dev_Cell.csv', col_types = cols(.default = col_double()))
dev_education <- readr::read_csv('dev_Education.csv', col_types = cols(.default = col_double()))
dev_enrolment <- readr::read_csv('dev_Enrolment.csv', col_types = cols(.default = col_double()))
dev_expectancy <- readr::read_csv('dev_Expectancy.csv', col_types = cols(.default = col_double()))
dev_hospital <- readr::read_csv('dev_Hospital.csv', col_types = cols(.default = col_double()))
dev_internet <- readr::read_csv('dev_Internet.csv', col_types = cols(.default = col_double()))
dev_literacy <- readr::read_csv('dev_Literacy.csv', col_types = cols(.default = col_double()))
dev_mortality <- readr::read_csv('dev_Mortality.csv', col_types = cols(.default = col_double()))
dev_physicians <- readr::read_csv('dev_Physicians.csv', col_types = cols(.default = col_double()))
dev_research <- readr::read_csv('dev_Research.csv', col_types = cols(.default = col_double()))
dev_servers <- readr::read_csv('dev_Servers.csv', col_types = cols(.default = col_double()))

# In long format
dev_ATM_long <- dev_ATM %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'ATM') %>%
  dplyr::arrange(Country)

dev_broadband_long <- dev_broadband %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Broadband') %>%
  dplyr::arrange(Country)

dev_cell_long <- dev_cell %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Cell') %>%
  dplyr::arrange(Country)

dev_education_long <- dev_education %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Education') %>%
  dplyr::arrange(Country)

dev_enrolment_long <- dev_enrolment %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Enrolment') %>%
  dplyr::arrange(Country)

dev_expectancy_long <- dev_expectancy %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Expectancy') %>%
  dplyr::arrange(Country)

dev_hospital_long <- dev_hospital %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Hospital') %>%
  dplyr::arrange(Country)

dev_internet_long <- dev_internet %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Country_internet') %>%
  dplyr::arrange(Country)

dev_literacy_long <- dev_literacy %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Literacy') %>%
  dplyr::arrange(Country)

dev_mortality_long <- dev_mortality %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Mortality') %>%
  dplyr::arrange(Country)

dev_physicians_long <- dev_physicians %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Physicians') %>%
  dplyr::arrange(Country)

dev_research_long <- dev_research %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Research') %>%
  dplyr::arrange(Country)

dev_servers_long <- dev_servers %>% 
  tidyr::gather(-c('Year'), key = 'Country', value = 'Servers') %>%
  dplyr::arrange(Country)

# Joining all indicators in panel format
panel_dev_indicators <- dev_ATM_long %>%
  dplyr::left_join(., dev_broadband_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_cell_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_education_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_enrolment_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_expectancy_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_hospital_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_internet_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_literacy_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_mortality_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_physicians_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_research_long, by = c('Country', 'Year')) %>%
  dplyr::left_join(., dev_servers_long, by = c('Country', 'Year')) %>%
  dplyr::arrange(Country)


# Nesting by country
nest_panel_dev_indicators <- panel_dev_indicators %>%
  dplyr::group_by(Country) %>%
  tidyr::nest()

year_max = max(panel_common$Year)
year_min = min(panel_common$Year)

func_dev_pc1 <- function(country_df, T = 0.2)
{
  # This function accepts a country dataframe and a tolerance
  # fraction for missing values. It filters years in year_min 
  # to year_max and computes PC1 from columns having more than 
  # fraction T non-missing entries
  
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

nest_panel_dev_indicators <- nest_panel_dev_indicators %>%
  dplyr::mutate('data_rm_full_NA_col' = purrr::map(data, func_rm_full_NA)) %>%
  dplyr::mutate('ncol_no_miss' = purrr::map_dbl(data_rm_full_NA_col, ncol))

# Compute dev pc 1 for each country in our sample
nest_panel_dev_indicators <- nest_panel_dev_indicators %>%
  dplyr::filter(ncol_no_miss > 2) %>%
  #  dplyr::filter(Country %in% name_country_full) %>%
  dplyr::mutate('Dev_PC1' = purrr::map(data, func_dev_pc1))



dev_pc1_long <- nest_panel_dev_indicators %>%
  dplyr::select(Country, Dev_PC1) %>%
  tidyr::unnest(., cols = Dev_PC1)

# Renaming countries to match with existing country names
dev_pc1_wide <- dev_pc1_long %>%
  tidyr::spread(., key = 'Country', value = 'PC1') %>%
  dplyr::rename('Hong Kong' = "Hong Kong SAR, China",
                "Russia" = `Russian Federation`,
                "Slovakia" = `Slovak Republic`,
                "UAE" = `United Arab Emirates`,
                "UK" = `United Kingdom`,
                "US" = `United States`,
                "Venezuela" = `Venezuela, RB`)

panel_dev_pc1 <- dev_pc1_wide %>%
  tidyr::gather(-c('Year'), key = Country, value = 'PC1')

# Country internet (as developmental factor)
panel_country_internet <- panel_dev_indicators %>%
  dplyr::select(Country, Year, Country_internet)


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
  ERM + EZ + Agg_econ_risk + Agg_fin_risk + Agg_pol_risk + Agg_liq_risk

panel_est_common_idio <- func_panel_est(formula = form_common_idio, panel_data = panel_common_idio)

### With risks: economic, political, financial, liquidity + Country internet
form_common_idio_country_internet <- Div ~ TED + VIX + SENT + FEDFUNDS + 
  ERM + EZ + Agg_econ_risk + Agg_fin_risk + Agg_pol_risk + Agg_liq_risk + Country_internet

panel_est_common_idio_country_internet <- func_panel_est(formula = form_common_idio_country_internet, 
                                                         panel_data = panel_common_idio)

### With risks: economic, political, financial, liquidity + Developmental PC1
form_common_idio_dev_pc1 <- Div ~ TED + VIX + SENT + FEDFUNDS + 
  ERM + EZ + Agg_econ_risk + Agg_fin_risk + Agg_pol_risk + Agg_liq_risk + PC1

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


#### PERIOD-WISE SEGREGATION OF RESULTS ###

# Period 1
panel_est_common_idio_country_internet_p1 <- func_panel_est(formula = form_common_idio_country_internet, 
                                                            panel_data = dplyr::filter(panel_common_idio,
                                                                                       Year < 2007))
# Period 2
panel_est_common_idio_country_internet_p2 <- func_panel_est(formula = form_common_idio_country_internet, 
                                                            panel_data = dplyr::filter(panel_common_idio,
                                                                                       Year <= 2012 & Year >= 2008))
# Period 3
panel_est_common_idio_country_internet_p3 <- func_panel_est(formula = form_common_idio_country_internet, 
                                                            panel_data = dplyr::filter(panel_common_idio,
                                                                                       Year > 2012))

##################################################################################
################# Balanced panel estimations #####################################
################# Table 4 and 5 in appendices ####################################
##################################################################################

panel_common_idio_balanced <- plm::make.pbalanced(na.omit(panel_common_idio), 
                                                  balance.type = 'shared.individuals')

### TABLE 4 ###

### No dev
panel_est_com_idio_bal <- func_panel_est(form_common_idio, panel_common_idio_balanced)

### Country internet
panel_est_com_idio_country_internet_bal <- func_panel_est(form_common_idio_country_internet, 
                                                          panel_common_idio_balanced)

### PC1
panel_est_com_idio_pc1_bal <- func_panel_est(form_common_idio_dev_pc1, 
                                             panel_common_idio_balanced)


############# TABLE 5 APPENDIX ##################

### PERIOD-WISE SEGREGATION ###

# Period 1
panel_est_com_idio_pc1_bal_p1 <- func_panel_est(form_common_idio_dev_pc1,
                                                dplyr::filter(panel_common_idio_balanced,
                                                              Year < 2007))

# Period 2
panel_est_com_idio_pc1_bal_p2 <- func_panel_est(form_common_idio_dev_pc1,
                                                dplyr::filter(panel_common_idio_balanced,
                                                              Year <= 2012 & Year >= 2007))

# Period 3
panel_est_com_idio_pc1_bal_p3 <- func_panel_est(form_common_idio_dev_pc1,
                                                dplyr::filter(panel_common_idio_balanced,
                                                              Year > 2012))




### Fig 6 construction ###

# Developed countries
country_dev <- dplyr::intersect(colnames(Div_ind_full_wide[, -c(1,2)]), name_country_developed)

Div_ind_dev_wide <- Div_ind_full_wide %>%
  dplyr::select(Year, all_of(country_dev))
Div_dev_mean <- apply(Div_ind_dev_wide[, -1], 1, function(vec){return(mean(vec, na.rm = T))})
Div_ind_dev_wide <- Div_ind_dev_wide %>%
  tibble::add_column('Div_mean_dev' = Div_dev_mean) %>%
  dplyr::select(Year, Div_mean_dev, everything())

# Emerging countries (non-developed)
country_emerg <- dplyr::intersect(colnames(Div_ind_full_wide[, -c(1,2)]), name_country_emerging)

Div_ind_emerg_wide <- Div_ind_full_wide %>%
  dplyr::select(Year, all_of(country_emerg))
Div_emerg_mean <- apply(Div_ind_emerg_wide[, -1], 1, function(vec){return(mean(vec, na.rm = T))})
Div_ind_emerg_wide <- Div_ind_emerg_wide %>%
  tibble::add_column('Div_mean_emerg' = Div_emerg_mean) %>%
  dplyr::select(Year, Div_mean_emerg, everything())

### Figure 6 ###

Fig_6_data <- Div_ind_dev_wide %>%
  dplyr::select(Year, Div_mean_dev) %>%
  dplyr::full_join(., Div_ind_emerg_wide[, c(1,2)], by = 'Year')
Fig_6 <- matplot(Fig_6_data$Year, Fig_6_data[,-1], type = 'l', 
                 xlab = 'Years', ylab = 'Equity diversification')
grid()
