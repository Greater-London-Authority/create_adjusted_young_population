## script to get local authority level population data using gp data to allocate subregion estimates

## 0. libraries and functions
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)

functions_to_read <- list.files("R/functions")

lapply(
  paste0("R/functions/", functions_to_read),
  FUN = source
)

year_mye_last <- 2024
year_gp_data_start <- 2015
year_pupil_data_start <- 2015
max_age_pupil_data <- 15
min_age_pupil_data <- 5
age_adult <- 18 # we don't want the adjustments to spill over to adult ages in the backseries

# age/year/cohorts are segmented and handled differently
# segments are labelled A, B, C, D, E, F

cohorts_A <- (year_pupil_data_start - max_age_pupil_data):(year_pupil_data_start - (min_age_pupil_data + 1))
cohorts_B <- (year_pupil_data_start - min_age_pupil_data):(year_mye_last - min_age_pupil_data)
cohorts_C <- (year_mye_last - (min_age_pupil_data - 1)):year_mye_last
cohorts_D <- (year_pupil_data_start - max_age_pupil_data):(year_mye_last - min_age_pupil_data)
cohorts_E <- (year_pupil_data_start - max_age_pupil_data):(year_mye_last - age_adult) #(max_age_pupil_data + 1))

ages_A <- 0:max_age_pupil_data
ages_B <- 0:min_age_pupil_data
ages_C <- 0:(min_age_pupil_data - 1)
ages_D <- min_age_pupil_data:max_age_pupil_data
ages_E <- max_age_pupil_data:age_adult

cohorts_AB1 <- (year_pupil_data_start - max_age_pupil_data):year_pupil_data_start
ages_AB1 <- 0:max_age_pupil_data

## 1. reading in the data and lookups

mye_2001_24 <- readRDS("data/intermediate/mye_2001_24_rev.rds") %>%
  filter(grepl("E", gss_code)) 

population_subregion <- readRDS("data/intermediate/adjusted_population_subregion.rds") %>%
  rename(itl221cd = gss_code, itl221nm = gss_name)

pupil_pop_lad <- readRDS("data/intermediate/total_pupils_lad_5_15_2015_2024.rds") %>%
  rename(pupils = value)

la_itl_lookup <- read_csv("lookups/la_itl_lookup_all.csv") %>%
  select(-ladxxnm)
la_itl_lookup <- unique(la_itl_lookup[, c("ladxxcd", "itl221cd")]) ## the xx is in there because this lookup contains several years of local authorities, across a few updates

lookup_lad_names <- mye_2001_24 %>%
  select(gss_code, gss_name) %>%
  distinct()

sf_pupils_residence_lad <- readRDS("data/intermediate/resident_pupils_state.rds") %>%
  rename(pupils = value)

pupil_codes_names <- sf_pupils_residence_lad %>%
  select(gss_code, pupil_name = gss_name) %>%
  distinct()

retain_districts_pupil_data <- pupil_codes_names %>%
  left_join(lookup_lad_names, by = "gss_code") %>%
  filter(gss_name == pupil_name) %>%
  pull(gss_code)

sf_pupils_filtered <- sf_pupils_residence_lad %>%
  filter(gss_code %in% retain_districts_pupil_data)

## 2. age on and apply components to births to get the population aged 5 without international migration

modelled_flows_null <- mye_2001_24 %>%
  filter(component %in% c("international_net", "international_out", "international_in")) %>%
  mutate(value = 0)

population_no_international <- create_pop_series(mye_coc = mye_2001_24, 
                                                 modelled_flows = modelled_flows_null,
                                                 yr_start = 2001,
                                                 yr_end = year_mye_last,
                                                 age_max = 90)

## 3. initial population 0-15 for 2015 onwards based on disaggregation of subregional data
# segments D, B2, and C

gp_data <- readRDS("data/intermediate/gp_sya_lad.rds") %>% 
  mutate(year = year(extract_date)) %>%
  filter(month(extract_date) == 7,
         year %in% year_gp_data_start:year_mye_last,
         sex != "persons",
         age <= max_age_pupil_data) %>%
  select(gss_code, gss_name, year, sex, age, value) %>%
  left_join(la_itl_lookup, by = c("gss_code" = "ladxxcd")) %>%
  group_by(itl221cd, year, age, sex) %>%
  mutate(itl_total_value = sum(value)) %>%
  ungroup()

scaling_factors <- gp_data %>% # creating the scaling factors, by dividing the value of each la-sex-age cell by the itl annual totals
  mutate(scaling_factor = value/itl_total_value) %>%
  select(itl221cd, gss_code, gss_name, year, sex, age, scaling_factor)

population_lad_DB2C <- population_subregion %>%
  filter(component == "population") %>%
  filter(age <= max_age_pupil_data, 
         year >= year_gp_data_start) %>%
  left_join(scaling_factors, by = c("itl221cd", "year", "age", "sex")) %>%
  mutate(value = value * scaling_factor) %>%
  select(-c(scaling_factor, itl221cd, itl221nm))

rm(scaling_factors)

## 3. calculate consistent flows for initial LAD populations

births_cohort <- mye_2001_24 %>%
  filter(component == "births",
         age == 0) %>%
  mutate(age = -1) %>%
  filter(year > year_gp_data_start) %>%
  select(gss_code, gss_name, sex, cohort = year, age, value)

total_net_flows_DB2C <- population_lad_DB2C %>%
  mutate(cohort = year - age) %>%
  select(-year) %>%
  bind_rows(births_cohort) %>%
  arrange(gss_code, sex, cohort, age) %>%
  group_by(gss_code, sex, cohort) %>%
  mutate(prev_value = lag(value),
         total_net = value - prev_value) %>%
  filter(age >= 0) %>%
  na.omit() %>%
  ungroup() %>%
  mutate(year = cohort + age) %>%
  select(-c(cohort, prev_value, value, component)) %>%
  arrange(gss_code, sex, year, age) 

internal_net_DB2C <- mye_2001_24 %>%
  filter(component == "internal_net") %>%
  filter(age <= max_age_pupil_data) %>%
  select(gss_code, year, sex, age, internal_net = value)

international_net_flows_DB2C <- total_net_flows_DB2C %>%
  left_join(internal_net_DB2C, by = c("gss_code", "year", "sex", "age")) %>%
  mutate(international_net = total_net - internal_net) %>%
  select(-c(total_net, internal_net))

modelled_flows_DB2C <- mye_2001_24 %>%
  filter(component %in% c("international_in", "international_out")) %>%
  filter(age <= max_age_pupil_data) %>%
  mutate(value = case_when(
    component %in% c("international_in", "international_out") & value < 0.1 ~ 0.1,
    TRUE ~ value
  )) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  rename(base_inflow = international_in, 
         base_outflow = international_out) %>%
  right_join(international_net_flows_DB2C, by = c("gss_code", "gss_name", "sex", "age", "year")) %>%
  mutate(model_flows = optimise_gross_flows(base_inflow, base_outflow, international_net)) %>%
  unnest_wider(col = model_flows) %>%
  select(-c(base_inflow, base_outflow)) %>%
  rename(international_in = inflow, international_out = outflow) %>%
  pivot_longer(cols = c("international_in", "international_out", "international_net"), 
               names_to = "component",
               values_to = "value")

hybrid_international <- mye_2001_24 %>%
  filter(component %in% c("international_in", "international_out", "international_net")) %>%
  rename(mye_value = value) %>%
  left_join(modelled_flows_DB2C, by = NULL) %>%
  mutate(value = case_when(
    is.na(value) ~ mye_value,
    TRUE ~ value
  )) %>%
  select(-mye_value)

# 4. set population minimum as resident state-funded pupils - this creates a new segment D
#TODO this is currently problematic as pupil data is not actually on local authority geography

population_lad_D <- population_lad_DB2C %>%
  filter(between(age, min_age_pupil_data, max_age_pupil_data)) %>%
  group_by(across(-any_of(c("value", "sex")))) %>%
  mutate(persons = sum(value)) %>%
  ungroup() %>%
  mutate(prop_sex = value/persons) %>%
  left_join(sf_pupils_filtered, by = NULL) %>%
  mutate(persons = case_when(
    pupils > persons ~ pupils,
    TRUE ~ persons
  )) %>%
  mutate(value = prop_sex * persons) %>%
  select(-c(pupils, prop_sex, persons))

# 5. recalculate birth to age 5 flows for updated population

residual_difference_B <- population_no_international %>%
  filter(age == min_age_pupil_data,
         component == "population",
         year >= year_pupil_data_start) %>%
  rename(pop_no_int = value) %>%
  select(-c(component, gss_name)) %>%
  left_join(population_lad_D, by = c("gss_code", "age", "sex", "year")) %>%
  mutate(residual_change = value - pop_no_int) %>%
  mutate(cohort = year - age) %>%
  select(-c(value, year, age, pop_no_int, component))

base_flows_B <- hybrid_international %>%
  mutate(value = case_when(
    component %in% c("international_in", "international_out") & value < 0.1 ~ 0.1,
    TRUE ~ value
  )) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  mutate(cohort = year - age) %>%
  filter(cohort %in% cohorts_B,
         age <= min_age_pupil_data) %>%
  select(year, cohort, sex,
         gss_code, gss_name,
         m_in = international_in,
         m_out = international_out,
         k_net = international_net)

message("Segment B flows")

modelled_flows_B <- fit_migration_flows(residual_difference_B, base_flows_B)

## 6. adjust flows for cohorts without a residual 
# here we're making the choice to use data for 2015:2019 cohorts to adjust the net flows for later cohorts

net_flow_adjustments_C <- modelled_flows_DB2C %>%
  filter(age <= min_age_pupil_data) %>%
  rename(original_value = value) %>%
  right_join(modelled_flows_B, by = NULL) %>%
  filter(year-age >= 2016) %>%
  filter(component %in% c("international_net")) %>%
  group_by(across(-any_of(c("year", "value", "original_value")))) %>%
  summarise(value = mean(value),
            original_value = mean(original_value),
            .groups = "drop") %>%
  mutate(net_adjustment = value - original_value) %>%
  select(-c(value, original_value, component))

modelled_flows_C <- modelled_flows_DB2C %>%
  mutate(cohort = year - age) %>%
  filter(age < min_age_pupil_data,
         cohort %in% cohorts_C) %>%
  mutate(value = case_when(
    component %in% c("international_in", "international_out") & value < 0.1 ~ 0.1,
    TRUE ~ value
  )) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  left_join(net_flow_adjustments_C, by = NULL) %>%
  mutate(international_net = international_net + net_adjustment) %>%
  select(-c(net_adjustment, cohort)) %>%
  rename(base_inflow = international_in, 
         base_outflow = international_out) %>%
  mutate(model_flows = optimise_gross_flows(base_inflow, base_outflow, international_net)) %>%
  unnest_wider(col = model_flows) %>%
  select(-c(base_inflow, base_outflow)) %>%
  rename(international_in = inflow, international_out = outflow) %>%
  pivot_longer(cols = c("international_in", "international_out", "international_net"), 
               names_to = "component",
               values_to = "value")


## 8. calculate implied annual net change for updated segment D

cohort_change_D <- population_lad_D %>%
  select(-component) %>%
  mutate(cohort = year - age) %>%
  select(-year) %>%
  filter(age %in% ages_D) %>%
  arrange(gss_code, sex, cohort, age) %>%
  group_by(gss_code, sex, cohort) %>%
  mutate(prev_value = lag(value),
         total_net = value - prev_value) %>%
  ungroup() %>%
  mutate(year = age + cohort) %>%
  filter(age > min_age_pupil_data, 
         year > year_pupil_data_start) %>%
  ungroup() %>%
  select(-c(cohort, prev_value, value)) 

## 9. get modelled flows consistent with annual pupil change, using MYE internal flows as-are

internal_net_D <- mye_2001_24 %>%
  filter(component == "internal_net") %>%
  filter(between(age, min_age_pupil_data + 1, max_age_pupil_data)) %>%
  rename(internal_net = value) %>%
  select(-component)

net_flows_D <- cohort_change_D %>%
  select(-gss_name) %>%
  left_join(internal_net_D, by = c("gss_code", "year", "sex", "age")) %>%
  mutate(international_net = total_net - internal_net) %>%
  select(-c(total_net, internal_net))

message("Segment D flows")

modelled_flows_D <- modelled_flows_DB2C %>%
  filter(component %in% c("international_in", "international_out")) %>%
  mutate(value = case_when(
    component %in% c("international_in", "international_out") & value < 0.1 ~ 0.1,
    TRUE ~ value
  )) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  rename(base_inflow = international_in, base_outflow = international_out) %>%
  right_join(net_flows_D, by = c("gss_code", "gss_name", "sex", "age", "year")) %>%
  mutate(model_flows = optimise_gross_flows(base_inflow, base_outflow, international_net)) %>%
  unnest_wider(col = model_flows) %>%
  select(-c(base_inflow, base_outflow)) %>%
  rename(international_in = inflow, international_out = outflow) %>%
  pivot_longer(cols = c("international_in", "international_out", "international_net"), 
               names_to = "component",
               values_to = "value")

#-----

# 10. segment E

start_cohort_E <- population_lad_D %>%
  mutate(cohort = year - age) %>%
  filter(age == max_age_pupil_data,
         cohort %in% cohorts_E) %>%
  select(gss_code, gss_name, sex, cohort, start_cohort = value)

end_cohort_E <- mye_2001_24 %>%
  mutate(cohort = year - age) %>%
  filter(between(year, year_pupil_data_start + 1, year_mye_last),
         cohort %in% cohorts_E,
         component == "population",
         age == age_adult) %>%
  # group_by(gss_code, gss_name, sex, cohort) %>%
  # filter(age == max(age)) %>%
  # ungroup() %>%
  select(gss_code, gss_name, sex, cohort, end_cohort = value)

residual_difference_E <- mye_2001_24 %>%
  mutate(cohort = year - age) %>%
  filter(between(age, max_age_pupil_data + 1, age_adult),
         cohort %in% cohorts_E,
         component == "internal_net") %>%
  group_by(gss_code, sex, cohort) %>%
  summarise(change = sum(value), .groups = "drop") %>%
  left_join(start_cohort_E, by = NULL) %>%
  left_join(end_cohort_E, by = NULL) %>%
  mutate(residual_change = end_cohort - (start_cohort + change)) %>%
  select(-c(start_cohort, end_cohort, change))

base_flows_E <- hybrid_international %>%
  mutate(value = case_when(
    component %in% c("international_in", "international_out") & value < 0.1 ~ 0.1,
    TRUE ~ value
  )) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  mutate(cohort = year - age) %>%
  filter(cohort %in% cohorts_E,
         between(age, max_age_pupil_data + 1, age_adult)) %>%
  select(year, cohort, sex,
         gss_code, gss_name,
         m_in = international_in,
         m_out = international_out,
         k_net = international_net)

message("Segment E flows")

modelled_flows_E <- fit_migration_flows(residual_difference_E, base_flows_E)

# 11. population for earlier years - segment A

residual_difference_A <- population_no_international %>%
  select(-gss_name) %>%
  mutate(cohort = year - age) %>%
  filter(year <= year_gp_data_start) %>%
  filter(cohort %in% cohorts_A) %>%
  filter(component == "population" & year == year_gp_data_start) %>%
  rename(pop_no_international = value) %>%
  left_join(population_lad_D, by = NULL) %>%
  mutate(residual_change = (value - pop_no_international)) %>%
  select(-c(pop_no_international, value, year, age, component))

base_flows_A <- mye_2001_24 %>%
  filter(component %in% c("international_net", "international_out", "international_in")) %>%
  mutate(value = case_when(
    component %in% c("international_in", "international_out") & value < 0.1 ~ 0.1,
    TRUE ~ value
  )) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  mutate(cohort = year - age) %>%
  filter(cohort %in% cohorts_A,
         year <= year_gp_data_start) %>%
  select(year, cohort, sex,
         gss_code, gss_name,
         m_in = international_in,
         m_out = international_out,
         k_net = international_net)

message("Segment A flows")

modelled_flows_A <- fit_migration_flows(residual_difference_A, base_flows_A)



combined_modelled_flows <- bind_rows(modelled_flows_A,
                                     modelled_flows_B,
                                     modelled_flows_C,
                                     modelled_flows_D,
                                     modelled_flows_E) %>%
  arrange(gss_code, sex, component, year, age)

# 12. combine modelled and original international flows

combined_international <- mye_2001_24 %>%
  filter(component %in% c("international_in", "international_out", "international_net")) %>%
  rename(mye_value = value) %>%
  left_join(combined_modelled_flows, by = NULL) %>%
  mutate(value = case_when(
    is.na(value) ~ mye_value,
    TRUE ~ value
  )) %>%
  select(-mye_value)

# 13. rebuild population using combined international flows

modelled_population <- create_pop_series(mye_coc = mye_2001_24, 
                                         modelled_flows = combined_international,
                                         yr_start = 2001,
                                         yr_end = year_mye_last,
                                         age_max = 90)

saveRDS(object = modelled_population,
        file = "data/processed/adjusted_population_lad.rds")

## compare results with other sources
if(FALSE){
  cohorts_df <- expand_grid(age = c(0:15), year = c(2009:2024)) %>%
    mutate(cohort = year - age)
  
  cohort_births <- mye_2001_24 %>%
    filter(component == "births") %>%
    filter(age == 0) %>%
    mutate(cohort = year) %>%
    select(-c(age, year)) %>%
    left_join(cohorts_df, by = "cohort") %>%
    select(-cohort) %>%
    mutate(version = "cohort births")
  
  compare_pop <- modelled_population %>%
    filter(age <= 21,
           component == "population") %>%
    mutate(version = "modelled") %>%
    bind_rows(
      mye_2001_24 %>%
        filter(age <= 21,
               component == "population") %>%
        mutate(version = "original"),
      cohort_births,
      gp_data %>%
        filter(age <= 15) %>%
        select(-c(itl221cd, itl_total_value)) %>%
        mutate(version = "GP")
    ) %>%
    group_by(across(-any_of(c("value", "sex")))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    bind_rows(
      sf_pupils_filtered %>%
        rename(value = pupils) %>%
        mutate(version = "pupils")
    )
  
  
  sel_code <- "E09000005"
  # sel_code <- "E08000025"
  
  compare_pop %>%
    filter(gss_code== sel_code) %>%
    filter(version %in% c("original", "modelled", "cohort births", "pupils", "GP")) %>%
    ggplot(aes(x = year, y = value, colour = version)) +
    geom_line() +
    facet_wrap("age")
  
  compare_pop %>%
    filter(gss_code== sel_code) %>%
    filter(age %in% c(4, 11, 17, 18)) %>%
    filter(version %in% c("original", "modelled", "cohort births", "pupils", "GP")) %>%
    ggplot(aes(x = year, y = value, colour = version)) +
    geom_line() +
    facet_wrap("age")
  
}
