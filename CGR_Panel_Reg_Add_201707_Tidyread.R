### July 2017 script with additional panel variables #######################

### Read and parse LHS diversification indices: Equity+Bond+REIT as well as
### RHS explanatory variables: Old (April 2017) and New (July 2017)

### Libraries ##############################################################

library(tidyverse)
library(plm)
library(lmtest)

###

### Directory Management ###################################################

# folder <- "July_2017"
 #folder <- "Data_July_2017"
 #file_path <- paste0(folder, "/")

###

### LHS: Read and parse the LHS dependent variables: Diversification indices
### for equity, bonds and REITs

Year_LHS = c(1984:2016)

## Equity ##################################################################

file_LHS_equity <- "LHS_CGR_panel_equity.csv"
file_path_LHS_equity <- paste0(file_path, file_LHS_equity)

# LHS_equity <- readr::read_csv(file_path_LHS_equity,
#                               na = c("", "NA")
#                               )

LHS_equity <- readr::read_csv(file_path_LHS_equity,
                              na = c("", "NA"))

LHS_equity <- LHS_equity %>%
  dplyr::select(-c(AVERAGE,
                   (ncol(LHS_equity)-3):ncol(LHS_equity)
                   )
                ) %>%
  dplyr::filter(Year %in% Year_LHS)

##

## Bond ######################################################################

file_LHS_bond <- "LHS_CGR_panel_bond.csv"
file_path_LHS_bond <- paste0(file_path, file_LHS_bond)

LHS_bond <- readr::read_csv(file_path_LHS_bond,
                            na = c("", "NA")
                            ) %>%
  dplyr::select(-AVERAGE) %>%
  dplyr::filter(Year %in% Year_LHS)

##

## REIT ######################################################################

file_LHS_REIT <- "LHS_CGR_panel_reit.csv"
file_path_LHS_REIT <- paste0(file_path, file_LHS_REIT)

LHS_REIT <- readr::read_csv(file_path_LHS_REIT,
                            na = c("", "NA")
                            ) %>%
  dplyr::select(-AVERAGE) %>%
  dplyr::filter(Year %in% Year_LHS)


##
###

### RHS Explanatory Variables: Read and Parse #################################

### RHS OLD: Read and parse old RHS data files ################################

## Common RHS Variables: TED, VIX etc. 

file_common_RHS <- "RHS_common.csv"
file_path_common_RHS <- paste0(file_path, file_common_RHS)

RHS_common <- readr::read_csv(file_path_common_RHS,
                              na = c("", "NA")
                              ) %>%
  dplyr::select(c(Year:SENT, 
                  FEDFUNDS, 
                  INTERNET, 
                  `1992 - ERM`,
                  `2009-10 - EUROZONE`
                  )
                ) %>%
  dplyr::rename(ERM = `1992 - ERM`,
                Euro = `2009-10 - EUROZONE`
                )

## Development Indicators 

file_dev <- "Development_Indicators_201704.csv"
file_path_dev <- paste0(file_path, file_dev)

RHS_dev_indi <- readr::read_csv(file_path_dev,
                                na = c("", "NA")
                                ) %>%
  dplyr::rename(Country = `Country Name`) %>%
  dplyr::rename(Life_Exp = `Life expectancy at birth total (years)`) %>%
  dplyr::rename(Internet_Usage = `Internet users (per 100 people)`)

# Extract life expectancy at birth

RHS_life_exp <- RHS_dev_indi %>%
  dplyr::select(Country, Year, Life_Exp)

# Extract Internet usage for each country

RHS_internet_idio <- RHS_dev_indi %>%
  dplyr::select(Country, Year, Internet_Usage)
  
# Extract the first principal component from the developmental indicators

temp_name <- RHS_dev_indi %>%
  dplyr::distinct(Country)

temp_year <- RHS_dev_indi %>%
  dplyr::distinct(Year)

RHS_dev_pc <- list()
temp_col <- -c(1,2)

for (i in 1:nrow(temp_name))
{
  temp_row <- which(RHS_dev_indi$Country == as.character(temp_name[i,1])) 
  temp_country <- RHS_dev_indi[temp_row, temp_col] # Collect country i development indicators
  
  temp_col_full_NA <- colSums(is.na(temp_country)) == nrow(temp_country) # Full NA column
  temp_country <- temp_country[, -temp_col_full_NA] # Remove full NA columns
  #temp_country <- temp_country[, colSums(is.na(temp_country)) < 13] # Remove part NA columns
  temp_country <- temp_country[, colSums(is.na(temp_country)) < 19] # Remove part NA columns
  
  if (ncol(temp_country) >= 2) # Compute PCs only if there are more than 2 columns else NULL
  {
    temp_country <- as.matrix(temp_country)
    
    for(j in 1:ncol(temp_country)) #For each column
    {
      # Locate NA entries and replace by column medians (no NA for PC computation)
      temp_country[is.na(temp_country[, j]), j] <- median(temp_country[, j], na.rm=T) 
    }
    
    temp_country_PC <- prcomp(temp_country, center = T, scale. = T)
    RHS_dev_pc[[i]] <- temp_country_PC$x[, 1] # Principal Component 1
    
  }
  else
  {
    RHS_dev_pc[[i]] <- NULL
  }
}

names(RHS_dev_pc) <- levels(as.factor(RHS_dev_indi$Country)) # Attach country name to development PC1

RHS_dev_pc_df <- do.call(cbind, RHS_dev_pc) # Convert as data frame

## Political Indicators 

file_pol <- "Political_Risk_201704.csv"
file_path_pol <- paste0(file_path, file_pol)

# Aggregate Political Risk = Sum of individual component indicators
Agg_Pol_Risk <- readr::read_csv(file_path_pol,
                                na = c("", "NA")
                                ) %>%
  dplyr::select(-c(Country, Year)) %>%
  rowSums()
  
RHS_pol_risk <- readr::read_csv(file_path_pol, 
                                na = c("", "NA")
                                ) %>%
  dplyr::select(Country, Year) %>%
  cbind(Agg_Pol_Risk) %>%
  tibble::as_tibble()

##

###

## RHS NEW: Read and parse new RHS data files ################################

# New variables in different .csv files
file_fin_risk <- "Add_Panel_Fin_Risk.csv"
file_credit_GDP <- "Add_Panel_Pvt_Credit_to_GDP.csv"
file_econ_risk <- "Add_Panel_Econ_Risk.csv"
file_GDP_per_capita <- "Add_Panel_GDP_per_capita.csv"
file_eq_liq <- "Add_Panel_Liq.csv"
file_mkt_cap_GDP <- "Add_Panel_Mkt_Cap_to_GDP.csv"
file_num_pub_firms <- "Add_Panel_Num_Public_Firms.csv"
file_trade_GDP <- "Add_Panel_Trade_to_GDP.csv"
file_turnover_domest_total <- "Add_Panel_Turnover_Domes_Total_Mkt.csv"
file_US_bond_spread <- "Add_Panel_US_Corp_Bond_Spread.csv"

file_path_fin_risk <- paste0(file_path, file_fin_risk)
file_path_credit_GDP <- paste0(file_path, file_credit_GDP)
file_path_econ_risk <- paste0(file_path, file_econ_risk)
file_path_GDP_per_capita <- paste0(file_path, file_GDP_per_capita)
file_path_eq_liq <- paste0(file_path, file_eq_liq)
file_path_mkt_cap_GDP <- paste0(file_path, file_mkt_cap_GDP)
file_path_num_pub_firms <- paste0(file_path, file_num_pub_firms)
file_path_trade_GDP <- paste0(file_path, file_trade_GDP)
file_path_turnover_domest_total <- paste0(file_path, file_turnover_domest_total)
file_path_US_bond_spread <- paste0(file_path, file_US_bond_spread)

Year_1960_2016 <- as.character(1960:2016) # For selecting columns for gathering

RHS_fin_risk <- readr::read_csv(file_path_fin_risk, 
                                na = c("", "NA")
                                ) %>%
  dplyr::select(Country, Year, `Aggregate Financial Risk`) %>%
  dplyr::rename(Agg_Fin_Risk = `Aggregate Financial Risk`)

RHS_credit_GDP <- readr::read_csv(file_path_credit_GDP, 
                                  na = c("", "NA", "..")
                                  ) %>% 
  dplyr::select(-c(`Series Name`, `Series Code`, `Country Code`)) %>%
  dplyr::rename(Country = `Country Name`) %>%
  tidyr::gather(Year_1960_2016,
                key = "Year", 
                value = "Credit_to_GDP"
                ) %>%
  dplyr::arrange(Country)

RHS_econ_risk <- readr::read_csv(file_path_econ_risk, 
                                 na = c("", "NA", "..")
                                 ) %>%
  dplyr::select(Country, Year, `Aggregate Economic Risk`) %>%
  dplyr::rename(Agg_Econ_Risk = `Aggregate Economic Risk`)

RHS_GDP_per_capita <- readr::read_csv(file_path_GDP_per_capita, 
                                      na = c("", "NA", "..")
                                      ) %>%
  dplyr::select(-c(`Series Name`, `Series Code`, `Country Code`)) %>%
  dplyr::rename(Country = `Country Name`) %>%
  tidyr::gather(Year_1960_2016,
                key = "Year", 
                value = "GDP_per_capita"
                ) %>%
  dplyr::arrange(Country) %>%
  dplyr::mutate(Year = as.integer(Year))

## Liquidity: Equity, new

RHS_eq_liq <- readr::read_csv(file_path_eq_liq, 
                           na = c("", "NA")
                           ) %>%
  dplyr::rename(Country = country,
                Year = year,
                Equity_Liq = index
                ) 

# Old Liquidity Indicators for Bonds and REITs

# Bonds

file_RHS_bond_liq <- "Liquidity_Bonds.csv"
file_path_RHS_bond_liq <- paste0(file_path, file_RHS_bond_liq)

RHS_bond_liq <- readr::read_csv(file_path_RHS_bond_liq,
                                na = c("", "NA")
                                ) %>%
  dplyr::rename(Country = country,
                Year = year,
                Bond_Liq = index
                ) 

# REITs

file_RHS_REIT_liq <- "Liquidity_REITs_2.csv"
file_path_RHS_REIT_liq <- paste0(file_path, file_RHS_REIT_liq)

RHS_REIT_liq <- readr::read_csv(file_path_RHS_REIT_liq,
                                na = c("", "NA")
                                ) %>%
  dplyr::rename(Country = country,
                Year = year,
                REIT_Liq = index
                ) 
##

RHS_mkt_cap_GDP <- readr::read_csv(file_path_mkt_cap_GDP, 
                                   na = c("", "NA", "..")
                                   ) %>%
  dplyr::select(-c(`Series Name`, `Series Code`, `Country Code`)) %>%
  dplyr::rename(Country = `Country Name`) %>%
  tidyr::gather(Year_1960_2016,
                key = "Year", 
                value = "Market_cap_to_GDP"
                ) %>%
  dplyr::arrange(Country) 
  
RHS_num_pub_firms <- readr::read_csv(file_path_num_pub_firms, 
                                     na = c("", "NA", "..")
                                     ) %>%
  dplyr::select(-c(`Series Name`, `Series Code`, `Country Code`)) %>%
  dplyr::rename(Country = `Country Name`) %>%
  tidyr::gather(Year_1960_2016,
                key = "Year", 
                value = "Num_public_firms"
                ) %>%
  dplyr::arrange(Country) 

RHS_trade_GDP <- readr::read_csv(file_path_trade_GDP, 
                                 na = c("", "NA", "..")
                                 ) %>%
  dplyr::select(-c(`Series Name`, `Series Code`, `Country Code`)) %>%
  dplyr::rename(Country = `Country Name`) %>%
  tidyr::gather(Year_1960_2016,
                key = "Year", 
                value = "Trade_to_GDP"
                ) %>%
  dplyr::arrange(Country) 

RHS_turnover_domest_total <- readr::read_csv(file_path_turnover_domest_total, 
                                             na = c("", "NA", "..")
                                             ) %>%
  dplyr::select(-c(`Series Name`, `Series Code`, `Country Code`)) %>%
  dplyr::rename(Country = `Country Name`) %>%
  tidyr::gather(Year_1960_2016,
                key = "Year", 
                value = "Turnover_Domest_Total"
                ) %>%
  dplyr::arrange(Country) 

RHS_US_bond_spread <- readr::read_csv(file_path_US_bond_spread, 
                                      na = c("", "NA"), 
                                      skip = 10
                                      ) %>%
  dplyr::select(observation_date, `BAA-AAA`) %>%
  dplyr::rename(Year = observation_date)

## RHS: NEW reading and parsing ends
  




