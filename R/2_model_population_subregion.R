
## 0. libraries and functions
library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(readxl)
library(lubridate)

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
cohorts_E <- (year_pupil_data_start - max_age_pupil_data):(year_mye_last - age_adult)

ages_A <- 0:max_age_pupil_data
ages_B <- 0:min_age_pupil_data
ages_C <- 0:(min_age_pupil_data - 1)
ages_D <- min_age_pupil_data:max_age_pupil_data
ages_E <- max_age_pupil_data:age_adult

## 1. reading in data and lookups

pupil_population_at_itl <- readRDS("data/intermediate/total_pupils_itl_5_15_2015_2024.rds") %>%
  rename(gss_code = itl221cd, pupils = value)

la_itl_lookup <- read_csv("lookups/la_itl_lookup_all.csv")

la_itl_lookup <- unique(la_itl_lookup[, c("ladxxcd", "itl221cd", "itl221nm")]) ## the xx is in there because this lookup contains several years of local authorities, across a few updates

# population estimates by subregion

mye_2001_24 <- readRDS("data/intermediate/mye_2001_24_rev.rds") %>%
  filter(grepl("E", gss_code)) %>%
  left_join(la_itl_lookup, by = c("gss_code" = "ladxxcd")) %>%
  group_by(itl221cd, itl221nm, component, year, sex, age) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  rename(gss_code = itl221cd, gss_name = itl221nm)

# GP counts by subregion

gp_data <- readRDS("data/intermediate/gp_sya_lad.rds") %>% 
  mutate(year = year(extract_date)) %>%
  filter(month(extract_date) == 7,
         year %in% year_gp_data_start:(year_mye_last + 1),
         sex != "persons",
         age <= max_age_pupil_data) %>%
  select(gss_code, year, sex, age, value) %>%
  left_join(la_itl_lookup, by = c("gss_code" = "ladxxcd")) %>%
  group_by(itl221cd, itl221nm, year, sex, age) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  rename(gss_code = itl221cd, gss_name = itl221nm)

## 2. age on and apply components to births to get the population aged 5 without international migration

modelled_flows_null <- mye_2001_24 %>%
  filter(component %in% c("international_net", "international_out", "international_in")) %>%
  mutate(value = 0)

population_no_international <- create_pop_series(mye_coc = mye_2001_24, 
                                        modelled_flows = modelled_flows_null,
                                        yr_start = 2001,
                                        yr_end = year_mye_last,
                                        age_max = 90)

## 3. calculate implied annual net change from GP count and mye births for segments B and C (0 to 5)
# note workaround for under-representation of GP counts at age 0 

births_cohort <- mye_2001_24 %>%
  filter(component == "births",
         age == 0) %>%
  mutate(age = -1) %>%
  filter(year >= year_gp_data_start) %>%
  select(gss_code, gss_name, sex, cohort = year, age, value)

gp_cohort_change_raw <- gp_data %>%
  filter(age <= min_age_pupil_data) %>%
  mutate(cohort = year - age) %>%
  select(-year) %>%
  bind_rows(births_cohort) %>%
  filter(cohort >= 2010) %>%
  arrange(gss_code, sex, cohort, age) %>%
  group_by(gss_code, sex, cohort) %>%
  mutate(prev_value = lag(value),
         total_net = value - prev_value) %>%
  filter(age >= 0) %>%
  na.omit() %>%
  ungroup()
  
gp_cohort_change_0_1 <- gp_cohort_change_raw %>%
  filter(age <= 1) %>%
  group_by(gss_code, sex, cohort) %>%
  mutate(total_net = mean(total_net)) %>%
  ungroup() %>%
  filter(cohort <= 2024)

gp_cohort_change <- gp_cohort_change_raw %>%
  filter(age > 1) %>%
  bind_rows(gp_cohort_change_0_1) %>%
  mutate(year = cohort + age) %>%
  select(-c(cohort, prev_value, value)) %>%
  filter(year <= 2024) %>%
  arrange(gss_code, sex, age, year) 

rm(gp_cohort_change_raw, gp_cohort_change_0_1)

## 4. get modelled flows consistent with annual GP change, using MYE internal flows as-are
# combine with original flows to create hybrid of the two sources

internal_net <- mye_2001_24 %>%
  filter(component == "internal_net") %>%
  filter(age <= min_age_pupil_data) %>%
  select(gss_code, year, sex, age, internal_net = value)

gp_net_flows <- gp_cohort_change %>%
  left_join(internal_net, by = c("gss_code", "year", "sex", "age")) %>%
  mutate(gp_international_net = total_net - internal_net) %>%
  select(-c(total_net, internal_net))

hybrid_international <- mye_2001_24 %>%
  filter(component %in% c("international_in", "international_out", "international_net")) %>%
  mutate(value = case_when(
    component %in% c("international_in", "international_out") & value < 0.1 ~ 0.1,
    TRUE ~ value
  )) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  rename(base_inflow = international_in, base_outflow = international_out, mye_international_net = international_net) %>%
  left_join(gp_net_flows, by = c("gss_code", "gss_name", "sex", "age", "year")) %>%
  mutate(international_net = case_when(
    is.na(gp_international_net) ~ mye_international_net,
    TRUE ~ gp_international_net
  )) %>%
  select(-c(gp_international_net, mye_international_net)) %>%
  mutate(model_flows = optimise_gross_flows(base_inflow, base_outflow, international_net)) %>%
  unnest_wider(col = model_flows) %>%
  select(-c(base_inflow, base_outflow)) %>%
  rename(international_in = inflow, international_out = outflow) %>%
  pivot_longer(cols = c("international_in", "international_out", "international_net"), 
               names_to = "component",
               values_to = "value")

rm(gp_net_flows, gp_cohort_change, internal_net)

## 5. calculate residual for age 5 between population with no international and subregional pupils
# pupils not disaggregated by sex so split residual equally between male and female

residual_difference_A <- population_no_international %>%
  mutate(cohort = year - age) %>%
  filter(component == "population",
         cohort %in% cohorts_A,
         year == year_pupil_data_start) %>%
  group_by(across(-any_of(c("sex", "value")))) %>%
  mutate(total_pop = sum(value)) %>%
  ungroup() %>%
  left_join(pupil_population_at_itl, by = c("gss_code", "age", "year")) %>%
  mutate(residual_change = (pupils - total_pop)/2) %>%
  mutate(cohort = year - age) %>%
  select(-c(pupils, value, total_pop, year, age, component))

residual_difference_B <- population_no_international %>%
  filter(age == min_age_pupil_data,
         component == "population",
         year >= year_pupil_data_start) %>%
  group_by(across(-any_of(c("sex", "value")))) %>%
  mutate(total_pop = sum(value)) %>%
  ungroup() %>%
  left_join(pupil_population_at_itl, by = c("gss_code", "age", "year")) %>%
  mutate(residual_change = (pupils - total_pop)/2) %>%
  mutate(cohort = year - age) %>%
  select(-c(pupils, value, total_pop, year, age, component))

## 6. adjust flows for cohorts where there is a 2015 age 6 to 15 residual available - segment A

base_flows_A <- hybrid_international %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  mutate(cohort = year - age) %>%
  filter(year <= year_pupil_data_start,
         cohort %in% unique(residual_difference_A$cohort)) %>%
  select(year, cohort, sex,
         gss_code, gss_name,
         m_in = international_in,
         m_out = international_out,
         k_net = international_net)

message("Segment A")

modelled_flows_A <- fit_migration_flows(residual_difference_A, base_flows_A)

## 7. adjust flows for cohorts where there is a age 5 residual available - segment B

base_flows_B <- hybrid_international %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  mutate(cohort = year - age) %>%
  filter(cohort %in% unique(residual_difference_B$cohort),
         age <= min_age_pupil_data) %>%
  select(year, cohort, sex,
         gss_code, gss_name,
         m_in = international_in,
         m_out = international_out,
         k_net = international_net)

message("Segment B")

modelled_flows_B <- fit_migration_flows(residual_difference_B, base_flows_B)


## 8. adjust flows for cohorts without a residual - segment C
# note: we're making the choice to use data for 2017:2019 cohorts to rescale the gross flows for later cohorts

gross_flow_scaling_factors_0_5 <- hybrid_international %>%
  filter(age <= min_age_pupil_data) %>%
  rename(original_value = value) %>%
  right_join(modelled_flows_B, by = NULL) %>%
  filter(year-age >= 2017) %>%
  filter(component %in% c("international_in", "international_out")) %>%
  group_by(across(-any_of(c("year", "value", "original_value")))) %>%
  summarise(value = sum(value),
            original_value = sum(original_value),
            .groups = "drop") %>%
  mutate(scaling_factor = value/original_value) %>%
  select(-c(value, original_value))

message("Segment C flows")

modelled_flows_C <- hybrid_international %>%
  mutate(cohort = year - age) %>%
  filter(age <= min_age_pupil_data,
         component %in% c("international_in", "international_out"),
         cohort %in% cohorts_C) %>%
  left_join(gross_flow_scaling_factors_0_5, by = NULL) %>%
  mutate(value = value * scaling_factor) %>%
  select(-scaling_factor) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  mutate(international_net = international_in - international_out) %>%
  pivot_longer(cols = c("international_in", "international_out", "international_net"),
               names_to = "component",
               values_to = "value") 

rm(gross_flow_scaling_factors_0_5)

## 8. calculate implied annual net change from age 5 to age 15 from pupil count data - segment D

pupil_cohort_change <- pupil_population_at_itl %>%
  rename(value = pupils) %>%
  mutate(cohort = year - age) %>%
  select(-year) %>%
  filter(age %in% ages_D) %>%
  arrange(gss_code, cohort, age) %>%
  group_by(gss_code, cohort) %>%
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
  group_by(gss_code, gss_name, year, age) %>%
  summarise(internal_net = sum(value), .groups = "drop")

pupil_net_flows <- pupil_cohort_change %>%
  left_join(internal_net_D, by = c("gss_code", "year", "age")) %>%
  mutate(international_net = total_net - internal_net) %>%
  select(-c(total_net, internal_net))

pupil_net_flows_sex <- bind_rows(
  pupil_net_flows %>%
    mutate(sex = "female",
           international_net = international_net/2),
  pupil_net_flows %>%
    mutate(sex = "male",
           international_net = international_net/2)
)

message("Segment D flows")

modelled_flows_D <- mye_2001_24 %>%
  filter(component %in% c("international_in", "international_out")) %>%
  mutate(value = case_when(
    component %in% c("international_in", "international_out") & value < 0.1 ~ 0.1,
    TRUE ~ value
  )) %>%
  pivot_wider(names_from = "component", values_from = "value") %>%
  rename(base_inflow = international_in, base_outflow = international_out) %>%
  right_join(pupil_net_flows_sex, by = c("gss_code", "gss_name", "sex", "age", "year")) %>%
  mutate(model_flows = optimise_gross_flows(base_inflow, base_outflow, international_net)) %>%
  unnest_wider(col = model_flows) %>%
  select(-c(base_inflow, base_outflow)) %>%
  rename(international_in = inflow, international_out = outflow) %>%
  pivot_longer(cols = c("international_in", "international_out", "international_net"), 
               names_to = "component",
               values_to = "value")


## 10. combine modelled and mye flows

combined_modelled_flows <- bind_rows(modelled_flows_A,
                                     modelled_flows_B,
                                     modelled_flows_C,
                                     modelled_flows_D) %>%
  select(-cohort)


combined_international <- mye_2001_24 %>%
  filter(component %in% c("international_in", "international_out", "international_net")) %>%
  rename(mye_value = value) %>%
  left_join(combined_modelled_flows, by = NULL) %>%
  mutate(value = case_when(
    is.na(value) ~ mye_value,
    TRUE ~ value
  )) %>%
  select(-mye_value)

## 11. build population consistent with modelled international flows

modelled_population <- create_pop_series(mye_coc = mye_2001_24, 
                                         modelled_flows = combined_international,
                                         yr_start = 2001,
                                         yr_end = year_mye_last,
                                         age_max = 90)

saveRDS(object = modelled_population,
        file = "data/intermediate/adjusted_population_subregion.rds")

#####-------------------------------------

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
    filter(age <= 15,
           component == "population") %>%
    mutate(version = "modelled") %>%
    bind_rows(
      mye_2001_24 %>%
        filter(age <= 15,
               component == "population") %>%
        mutate(version = "original"),
      cohort_births,
      gp_data %>% filter(age <= 15) %>%
        mutate(version = "GP")
    ) %>%
    group_by(across(-any_of(c("value", "sex")))) %>%
    summarise(value = sum(value), .groups = "drop") %>%
    bind_rows(
      pupil_population_at_itl %>%
        rename(value = pupils) %>%
        mutate(version = "pupils")
    )
  
  sel_code <- "TLI3"
  
  compare_pop %>%
    filter(gss_code== sel_code) %>%
    filter(version %in% c("original", "modelled", "cohort births", "pupils", "GP")) %>%
    ggplot(aes(x = year, y = value, colour = version)) +
    geom_line() +
    facet_wrap("age")
  
}