#Loss & Damage Data Foundation

setwd("~/Projects/loss_and_damages")

library(dplyr)

#load in base data which includes country, designation, and country codes/abbreviation
base <- read.csv("data/country_economy_designation.csv") %>% filter(country != c('Andorra', 'West Bank and Gaza')) %>% 
  filter(!(country == 'China' & designation != 'advanced_economy'))

#load in emission data
emissions <- read.csv('data/GCB2022v27_MtCO2_flat.csv') %>% 
  rename(iso = ISO.3166.1.alpha.3) %>%
  filter(Country != 'Global')

emissions_cumulative <- emissions %>% 
  group_by(Country, iso) %>%
  summarise(country_emissions = sum(Total)) %>%
  ungroup() %>%
  mutate(global_emissions = sum(country_emissions, na.rm = T),
         emissions_share = country_emissions / global_emissions) %>%
  arrange(desc(emissions_share))


#load in vulnerability and readiness data from ND
vulnerability <- read.csv('data/resources/vulnerability/vulnerability.csv') 

# Loop through each column and remove "X" characters
colnames(vulnerability) <- gsub("X", "", colnames(vulnerability))

vulnerability_2021 <- vulnerability %>% select(iso = ISO3, vulnerability_2021 = `2021`)

#load in readiness data
readiness <- read.csv('data/resources/readiness/readiness.csv') 
colnames(readiness) <- gsub("X", "", colnames(readiness))

readiness_2021 <- readiness %>% select(iso = ISO3, readiness_2021 = `2021`)


#GDP processing
gdp_per_capita <- read.csv('data/gdp-per-capita-worldbank.csv') %>% 
  filter(Year == 2021 & Code != "") %>%
  mutate(per_capita_avg = mean(GDP.per.capita..PPP..constant.2017.international..., na.rm = T)) %>%
  mutate(per_capita_multiplier = GDP.per.capita..PPP..constant.2017.international... / per_capita_avg) %>%
  select(iso = Code, gdp_per_capita_2021 = GDP.per.capita..PPP..constant.2017.international..., per_capita_multiplier)

country_pop <- read.csv('data/country_pop.csv', skip = 5) %>% select(iso = ABW, population = X94483)

gdp <- read.csv('data/gdp_processed.csv')%>%
  select(iso = Country.Code, gdp_2021 = X2021)

gdp_f <- gdp %>%
  left_join(gdp_per_capita, by = 'iso') %>%
  left_join(country_pop, by = 'iso') %>%
  mutate(gdp_f = ifelse(is.na(gdp_2021), gdp_per_capita_2021 * population/1000, gdp_2021),
         log_multiplier = ifelse(log(per_capita_multiplier, base = 10) < -0.5, -0.5,
                                 ifelse(log(per_capita_multiplier, base = 10)>0.5, 0.5, log(per_capita_multiplier, base = 10)))
         ) %>%
  mutate(log_multiplier_f = ifelse(is.na(log_multiplier), 1, 1 + log_multiplier))  %>%
  mutate(gdp_reformed = gdp_f * log_multiplier_f) %>%
  mutate(gdp_weight = gdp_reformed / sum(gdp_reformed, na.rm = T)) %>%
  select(iso, gdp_reformed, gdp_2021)

#Renewable as a share of total energy
renewable <- read.csv('data/Energy_Transition.csv') %>% filter(Indicator == 'Electricity Generation') %>%
  select(country = Country, iso = ISO3, Technology, Energy_Type, Unit, Source, gigawatt_hours_2020 = F2020)

renewable_grp <- renewable %>% 
  group_by(country, iso, Energy_Type) %>%
  summarise(gigawatt_hours_2020 = sum(gigawatt_hours_2020, na.rm = T))

renewable_grp2 <- renewable_grp %>% 
  group_by(country) %>%
  summarise(total_energy = sum(gigawatt_hours_2020))

renewable_grp3 <- left_join(renewable_grp, renewable_grp2, by = 'country') %>%
  filter(Energy_Type == 'Total Renewable') %>%
  mutate(renewable_share_2020 = gigawatt_hours_2020 / total_energy) %>%
  select(iso, renewable_share_2020)

#Climate Change Damages
exposure <- read.csv('data/Climate-driven_INFORM_Risk.csv') %>% 
  filter(Indicator == 'Climate-driven Hazard & Exposure') %>%
  select( iso = ISO3, env_exposure_2022 = F2022)


#Join all data
joined <- base %>% 
  left_join(emissions_cumulative, by = 'iso') %>% 
  left_join(readiness_2021, by = 'iso') %>% 
  left_join(vulnerability_2021, by = 'iso') %>% 
  left_join(gdp_f, by = 'iso') %>% 
  left_join(renewable_grp3, by = 'iso') %>% 
  left_join(exposure, by = 'iso') 

min_max_cols <- c('emissions_share', 'readiness_2021', 'vulnerability_2021', 'gdp_reformed', 'renewable_share_2020', 'env_exposure_2022')

min_max_scaling <- function(data, columns) {
  scaled_data <- data
  for (col in columns) {
    min_val <- min(data[[col]], na.rm = T)
    max_val <- max(data[[col]], na.rm = T)
    scaled_data[[paste0(col, "_scaled")]] <- (data[[col]] - min_val) / (max_val - min_val)
  }
  return(scaled_data)
}

impute_median <- function(data) {
  numeric_cols <- sapply(data, is.numeric)
  for (col in colnames(data)[numeric_cols]) {
    median_value <- median(data[[col]], na.rm = TRUE)
    data[[col]][is.na(data[[col]])] <- median_value
  }
  return(data)
}

joined_imputed <- impute_median(joined)

# Perform min-max scaling on selected columns
scaled_data <- min_max_scaling(joined_imputed, min_max_cols) %>% mutate(renewable_share_2020_scaled = 1 - renewable_share_2020_scaled)

#na_countries <- scaled_data %>% filter(country.x %in% c("Aruba", "Kosovo","St. Vincent and the Grenadines", "Syria","Turkmenistan" ,"Tuvalu","Venezuela","Yemen","Eritrea","Kiribati",    "South Sudan" ))

write.csv(scaled_data, 'data/ld_processed.csv')


