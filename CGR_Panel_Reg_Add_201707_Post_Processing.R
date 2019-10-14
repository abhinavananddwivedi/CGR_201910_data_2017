### July 2017 script which takes the parsed LHS and RHS data which it
### post-processes 

### Libraries ##############################################################

library(tidyverse)
library(plm)
library(lmtest)
library(lubridate)

###

### Directory Management ###################################################

# folder <- "July_2017"
# subfolder <- "Data_July_2017"
# file_path <- paste0(folder, "/", subfolder, "/")

###

### Parse LHS and RHS data tidily ###########################################

file_script <- "CGR_Panel_Reg_Add_201707_Tidyread.R" # Read tidily
#file_path_script <- paste0(folder, "/", file_script)

#source(file_path_script, echo = F) # Read and parse original data files
source(file_script, echo = F) # Read and parse original data files

##fefe

### Post Processing and Estimation #########################################

year_dev <- 1980:2016

### LHS variables in long format (panel format) ############################

## Equity

LHS_equity_long <- LHS_equity %>%
  dplyr::rename(`United States` = US,
                `United Kingdom` = UK,
                `United Arab Emirates` = UAE
                ) %>%
  tidyr::gather("Country", "Diversification_Equity", Argentina:Zambia) %>%
  dplyr::select(Country, everything()) %>%
  dplyr::mutate(Year = as.integer(Year))
  

## Bond

LHS_bond_long <- LHS_bond %>%
  dplyr::rename(`United Kingdom` = UK,
                `United States` = US
                ) %>%
  tidyr::gather("Country", "Diversification_Bond", Australia:`United States`) %>%
  dplyr::select(Country, everything()) %>%
  dplyr::mutate(Year = as.integer(Year))


## REIT

LHS_REIT_long <- LHS_REIT %>%
  dplyr::rename(`United Kingdom` = UK,
                `United States` = US
                ) %>%
  tidyr::gather("Country", "Diversification_REIT", Australia:`United States`) %>%
  dplyr::select(Country, everything()) %>%
  dplyr::mutate(Year = as.integer(Year))


### RHS variables ################################################################

## Common RHS independent variables

year_US_bond_spread <- lubridate::year(RHS_US_bond_spread$Year)

RHS_US_bond_spread <- RHS_US_bond_spread %>%
  dplyr::select(`BAA-AAA`) %>%
  cbind(year_US_bond_spread) %>%
  dplyr::rename(Year = year_US_bond_spread) %>%
  dplyr::select(Year, everything()) %>%
  dplyr::mutate(Year = as.integer(Year))

RHS_common <- dplyr::full_join(RHS_common, 
                               RHS_US_bond_spread, 
                               by = "Year"
                               ) %>%
  dplyr::arrange(Year)

## Idiosyncratic RHS independent variables

# The principal component for the developmental indicators

RHS_dev_pc_df <- RHS_dev_pc_df %>% 
  cbind(., year_dev) %>%
  tibble::as_tibble() %>%
  dplyr::rename(Year = year_dev) %>%
  dplyr::select(Year, everything())

# The developmental PC in long format (panel format)

RHS_dev_pc_long <- RHS_dev_pc_df %>%
  tidyr::gather("Country", "Dev_PC1", Afghanistan:Zimbabwe) %>%
  dplyr::select(Country, everything())

#

RHS_idio_new_1 <- dplyr::full_join(RHS_credit_GDP, 
                                 RHS_mkt_cap_GDP, 
                                 by = c("Country", "Year")
                                 ) %>%
  dplyr::full_join(., RHS_num_pub_firms, 
                   by = c("Country", "Year")
                   ) %>%
  dplyr::full_join(., RHS_trade_GDP,
                   by = c("Country", "Year")
                   ) %>%
  dplyr::full_join(., RHS_turnover_domest_total,
                   by = c("Country", "Year")
                   ) %>%
  dplyr::mutate(Year = as.integer(Year))

RHS_idio_new_2 <- dplyr::full_join(RHS_fin_risk,
                                   RHS_econ_risk,
                                   by = c("Country", "Year")
                                   ) %>%
  dplyr::full_join(., RHS_pol_risk,
                   by = c("Country", "Year")
                   ) %>%
  dplyr::full_join(., RHS_life_exp,
                   by = c("Country", "Year")
                   ) %>%
  dplyr::full_join(., RHS_internet_idio,
                   by = c("Country", "Year")
                   )

RHS_idio_new_3 <- dplyr::full_join(RHS_eq_liq, 
                                   RHS_bond_liq,
                                   by = c("Country", 
                                          "Year"
                                          )
                                   ) %>% 
  dplyr::full_join(., RHS_REIT_liq,
                   by = c("Country", 
                          "Year"
                          )
                   )


RHS_idio <- dplyr::full_join(RHS_idio_new_1,
                             RHS_idio_new_2,
                             by = c("Country",
                                    "Year"
                                    )
                             ) %>%
  dplyr::full_join(., RHS_idio_new_3,
                   by = c("Country",
                          "Year"
                          )
                   )

## Join the developmental PC too

RHS_idio <- dplyr::full_join(RHS_idio,
                             RHS_dev_pc_long,
                             by = c("Country",
                                    "Year")
                             )

###

### Combine LHS and RHS variables in panel format

## Equity

Panel_equity <- dplyr::left_join(LHS_equity_long,
                                 RHS_common,
                                 by = "Year"
                                 ) %>%
  dplyr::left_join(., RHS_idio,
                   by = c("Country", "Year")
                   ) %>%
  dplyr::rename(US_bond_spread = `BAA-AAA`,
                LHS_div_eq = Diversification_Equity
                ) %>%
  dplyr::select(-c(Bond_Liq, REIT_Liq)) %>%
  dplyr::mutate(Market_cap_to_GDP = as.numeric(Market_cap_to_GDP),
                Num_public_firms = as.numeric(Num_public_firms),
                Turnover_Domest_Total = as.numeric(Turnover_Domest_Total)
                )

## Bond

Panel_bond <- dplyr::left_join(LHS_bond_long, 
                               RHS_common, 
                               by = "Year"
                               ) %>%
  dplyr::left_join(., RHS_idio,
                   by = c("Country", "Year")
                   ) %>%
  dplyr::rename(US_bond_spread = `BAA-AAA`,
                LHS_div_b = Diversification_Bond
                ) %>%
  dplyr::select(-c(Equity_Liq, REIT_Liq)) %>%
  dplyr::mutate(Market_cap_to_GDP = as.numeric(Market_cap_to_GDP),
                Num_public_firms = as.numeric(Num_public_firms),
                Turnover_Domest_Total = as.numeric(Turnover_Domest_Total)
                )

## REIT

Panel_REIT <- dplyr::left_join(LHS_REIT_long, 
                               RHS_common, 
                               by = "Year"
                               ) %>%
  dplyr::left_join(., RHS_idio,
                   by = c("Country", "Year")
                   ) %>%
  dplyr::rename(US_bond_spread = `BAA-AAA`,
                LHS_div_r = Diversification_REIT
                ) %>%
  dplyr::select(-c(Bond_Liq, Equity_Liq)) %>%
  dplyr::mutate(Market_cap_to_GDP = as.numeric(Market_cap_to_GDP),
                Num_public_firms = as.numeric(Num_public_firms),
                Turnover_Domest_Total = as.numeric(Turnover_Domest_Total)
                )

### Pooled Correlation Matrices for panels ########################

# Equity: Pearson and Spearman correlations
panel_equity_cor_P <- Panel_equity %>%
  dplyr::select(-c(Country, Year)) %>%
  cor(., use = "complete.obs", method = "pearson")

# Bond

panel_bond_cor_P <- Panel_bond %>%
  dplyr::select(-c(Country, Year)) %>%
  cor(., use = "complete.obs", method = "pearson")

# REIT

panel_REIT_cor_P <- Panel_REIT %>%
  dplyr::select(-c(Country, Year)) %>%
  cor(., use = "complete.obs", method = "pearson")


###

### Avg Correlation Matrices for Panels ###################################

name_country_equity <- levels(as.factor(Panel_equity$Country))
name_country_bond <- levels(as.factor(Panel_bond$Country))
name_country_REIT <- levels(as.factor(Panel_REIT$Country))

# Equity
temp_cor_eq <- list()
world_cor_avg_eq <- list()

for (i in 1:length(name_country_equity))
{
  temp_eq <- Panel_equity[which(Panel_equity$Country == name_country_equity[i]), -c(1,2)]
  temp_cor_eq[[i]] <- cor(temp_eq, use = "na.or.complete")
}

temp_cor_array_eq <- simplify2array(temp_cor_eq)

world_cor_avg_eq <- apply(temp_cor_array_eq, c(1,2), mean, na.rm=T) #Matrix average

# Bond
temp_cor_b <- list()
world_cor_avg_b <- list()

for (i in 1:length(name_country_bond))
{
  temp_b <- Panel_bond[which(Panel_bond$Country == name_country_bond[i]), -c(1,2)]
  temp_cor_b[[i]] <- cor(temp_b, use = "na.or.complete")
}

temp_cor_array_b <- simplify2array(temp_cor_b)

world_cor_avg_b <- apply(temp_cor_array_b, c(1,2), mean, na.rm=T) #Matrix average

# REIT

temp_cor_r <- list()
world_cor_avg_r <- list()

for (i in 1:length(name_country_REIT))
{
  temp_r <- Panel_REIT[which(Panel_REIT$Country == name_country_REIT[i]), -c(1,2)]
  temp_cor_r[[i]] <- cor(temp_r, use = "na.or.complete")
}

temp_cor_array_r <- simplify2array(temp_cor_r)

world_cor_avg_r <- apply(temp_cor_array_r, c(1,2), mean, na.rm=T) #Matrix average


###

### Write out panel as .csv files #########################################

# Equity
file_panel_equity <- "Panel_equity.csv"
file_path_panel_equity <- paste0(folder, "/", file_panel_equity)
readr::write_csv(Panel_equity, file_path_panel_equity)

# Bond
file_panel_bond <- "Panel_bond.csv"
file_path_panel_bond <- paste0(folder, "/", file_panel_bond)
readr::write_csv(Panel_bond, file_path_panel_bond)

# REIT
file_panel_REIT <- "Panel_REIT.csv"
file_path_panel_REIT <- paste0(folder, "/", file_panel_REIT)
readr::write_csv(Panel_REIT, file_path_panel_REIT)

### Write out correlation matrices: pooled and averaged

## Pooled
# Equity
file_cor_equity_pool <- "Equity_Cor_Pooled.csv"
file_path_cor_equity_pool <- paste0(folder, "/", file_cor_equity_pool)
readr::write_csv(tibble::as_tibble(panel_equity_cor_P), 
                 file_path_cor_equity_pool
                 )

# Bond
file_cor_bond_pool <- "Bond_Cor_Pooled.csv"
file_path_cor_bond_pool <- paste0(folder, "/", file_cor_bond_pool)
readr::write_csv(tibble::as_tibble(panel_bond_cor_P), 
                 file_path_cor_bond_pool
                 )

# REIT
file_cor_REIT_pool <- "REIT_Cor_Pooled.csv"
file_path_cor_REIT_pool <- paste0(folder, "/", file_cor_REIT_pool)
readr::write_csv(tibble::as_tibble(panel_REIT_cor_P), 
                 file_path_cor_REIT_pool
                 )

## Avg
# Equity
file_cor_equity_avg <- "Equity_Cor_Avg.csv"
file_path_cor_equity_avg <- paste0(folder, "/", file_cor_equity_avg)
readr::write_csv(tibble::as_tibble(world_cor_avg_eq), 
                 file_path_cor_equity_avg
                 )

# Bond
file_cor_bond_avg <- "Bond_Cor_Avg.csv"
file_path_cor_bond_avg <- paste0(folder, "/", file_cor_bond_avg)
readr::write_csv(tibble::as_tibble(world_cor_avg_b), 
                 file_path_cor_bond_avg
                 )

# REIT
file_cor_REIT_avg <- "REIT_Cor_Avg.csv"
file_path_cor_REIT_avg <- paste0(folder, "/", file_cor_REIT_avg)
readr::write_csv(tibble::as_tibble(world_cor_avg_r), 
                 file_path_cor_REIT_avg
                 )


##############################################
