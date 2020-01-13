library(tidyverse)

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

