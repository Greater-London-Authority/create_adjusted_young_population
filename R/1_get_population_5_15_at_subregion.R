
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


## 1. reading in the data and lookups
tmp_zip <- tempfile(fileext = ".zip")
download.file("https://content.explore-education-statistics.service.gov.uk/api/releases/63491b17-2037-4533-b719-d3656aaf6ed5/files?fromPage=ReleaseUsefulInfo", 
              tmp_zip, 
              mode = "wb")

unzip(tmp_zip, 
      files = c("data/spc_cbm.csv", "data/spc_pupils_age_and_sex.csv"),
      exdir = tempdir())

spc_cbm_data <- read_csv(file.path(tempdir(), "data/spc_cbm.csv"))

#spc_cbm_data_11_19 <- read_csv("https://data.london.gov.uk/download/ex9jd/jz3/resident_pupils_lea_ncy_2011_2019_from_cbm.csv")

# download.file("https://data.london.gov.uk/download/ex9jd/jz3/resident_pupils_lea_ncy_2011_2019_from_cbm.csv", 
#               destfile = "data/raw/resident_pupils_lea_ncy_2011_2019_from_cbm.csv", 
#               mode = "wb")

spc_cbm_data_11_19 <- read_csv("data/raw/resident_pupils_lea_ncy_2011_2019_from_cbm.csv")


ind_schools <- read_csv(file.path(tempdir(), "data/spc_pupils_age_and_sex.csv"))

la_itl_lookup <- fread("lookups/la_itl_lookup_all.csv")

la_county_lookup <- fread("lookups/la_county_lookup.csv")

new_old_la_lookup <- fread("lookups/old_new_las_lookup.csv") # this is a lookup that, I believe, will be needed solely to deal with the strange collection of geographies that come out of the data that is released for the school census. A mix, across different updates, of local authorities, districts, unitary authorities, and counties. 


## 2. extract resident pupils in state-funded schools

resident_pupils_state_20_onwards <- spc_cbm_data %>%
  select(time_period, geographic_level, country_code, country_name,
         region_code, region_name, boarder,
         gss_name = la_name, gss_code = new_la_code,
         ncyear, value = resident_headcount) %>%
  mutate(year = as.numeric(str_sub(time_period, 1, 4))) %>%
  filter(!ncyear %in% c("Total", "X", "x"),
         country_code != "z",
         boarder == "Total") %>%
  mutate(age = case_when(
    ncyear == "Reception" ~ 4,
    TRUE ~ as.numeric(ncyear) + 4
  )) %>%
  select(-c(time_period, ncyear)) %>%
  mutate(gss_name = case_when(
    geographic_level == "National" ~ country_name,
    geographic_level == "Regional" ~ region_name,
    TRUE ~ gss_name
  )) %>%
  mutate(gss_code = case_when(
    geographic_level == "National" ~ country_code,
    geographic_level == "Regional" ~ region_code,
    TRUE ~ gss_code
  )) %>%
  group_by(gss_code, gss_name, year, age) %>%
  summarise(value = sum(value), .groups = "drop") %>%
  arrange(year, gss_code, age)

resident_pupils_state_20_onwards <- resident_pupils_state_20_onwards[!grepl("E12|E92", resident_pupils_state_20_onwards$gss_code), ]

resident_pupils_11_19 <- spc_cbm_data_11_19 %>%
  mutate(age = nc_year + 4, year = acd_year - 1) %>% ## acd year was given in the format of just 4 digits, and I'm quite sure now that it's actually the second year in the xxxx/yyyy format. So if the year given is 2019, for example, I need to change it to 2018 to get it in the same year format as the pupil data for 2020-onwards. 
  rename(gss_code = lea9, gss_name = lea_name, value = count) %>%
  select(gss_code, gss_name, year, age, value)

setdiff(resident_pupils_11_19$gss_code, resident_pupils_state_20_onwards$gss_code) ## some differences in codes between the two datasets, pre 2020 and 2020 onwards, but it'll be taken care of below in section 3. Mostly by new_old_la_lookup
setdiff(resident_pupils_state_20_onwards$gss_code, resident_pupils_11_19$gss_code)

## HERE, where stopped
resident_pupils_state <- rbind(resident_pupils_state_20_onwards, resident_pupils_11_19) %>%
  filter(year >= 2015)


## 3. combine both datasets to get the la-level population estimates

  ### 3.1. some cleaning of the independent schools dataset, to get it in the same format as resident pupils
pupils_independent <- ind_schools %>%
  mutate(year = as.numeric(substr(time_period, 1, 4))) %>%
  filter(year >= 2015 & phase_type_grouping == "Independent school" & geographic_level == "Local authority" & sex == "Total") %>%
  select(new_la_code, la_name, year, age, headcount)

colnames(pupils_independent) <- c("gss_code", "gss_name", "year", "age", "value")

pupil_data <- rbind(resident_pupils_state, pupils_independent)

pupil_data <- pupil_data[, c("gss_code", "year", "age", "value")] # getting rid of gss name


## 4. aggregating the data to subregions
### overall, this is a complex process, far more difficult than it should be. The data from Schools, Pupils, and their Characteristics come out in a mix of local authorities, unitary authorities, and even counties, with several different versions across the years. 
### three steps needed, which I should try to cut down a bit. The first is updating an assortment of old local authorities to new ones. These are an odd few which were hard to track down and didn't seem to appear in many lookups (but it may be possible to do with in the same step as step 3). The second step was getting rid of all on the county-level codes in the dataset, and instead assigning the area to one of its constituent local authorities (which is fine if we're aggregating up to subregion). The third step is the aggregation to subregions itself. The lookup we use for this contains all of the local authority codes across all updates back to 2021 together - in this way, it picks up both old and new local authorities. Have checked and all are covered.  
### this section was mostly lifted and amended from the subregional pupil projections repo. So definitely needs to be amended and harmonised and so on.

  ### 4.1. updating a few of the local authority codes in the dataset - some of the older ones that were hard to chase down in the new lookups
pupil_data <- convert_geographies_to_latest(
  data = pupil_data,
  lookup = new_old_la_lookup,
  geog_from_data = "gss_code",
  geog_from_lookup = "old_las", 
  geog_to_lookup = "new_las",
  count_names = c("value")
)

state_funded_pupils_residence <- convert_geographies_to_latest(
  data = resident_pupils_state,
  lookup = new_old_la_lookup,
  geog_from_data = "gss_code",
  geog_from_lookup = "old_las", 
  geog_to_lookup = "new_las",
  count_names = c("value")
)
  
  ### 4.2. converting the local education codes that appear as county codes
  ### what we will do is change it to the code corresponding to any of its constituent local authorities. It doesn't matter which one - they will all map onto the correct itl
  ### roughly repurposing old code to do this

counties_to_replace <- unique(pupil_data$new_las)[!(unique(pupil_data$new_las) %in% unique(la_itl_lookup$ladxxcd))] # extracting counties in the pupil dataset, which won't have any match in the la-itl lookup
 
counties_to_replace <- counties_to_replace[counties_to_replace != "E10000021"] # removing Northamptonshire. This one has to be fixed using a separate process.
 
    #### looping through each county code in the dataset, and changing that county code to one of (any of) its constituent local authorities
for(i in counties_to_replace){
  la_to_replace <- la_county_lookup[cty21cd == i, lad21cd][1] # taking the  first local authority listed beside the county to replace, to replace that county. 
  pupil_data[new_las == i, new_las := la_to_replace]
}

for(i in counties_to_replace){
  la_to_replace <- la_county_lookup[cty21cd == i, lad21cd][1] # taking the  first local authority listed beside the county to replace, to replace that county. 
  state_funded_pupils_residence[new_las == i, new_las := la_to_replace]
}

    #### for Northampton, we need to just change it manually to one of its constituent local authorities. Because E10000021 was split into E06000061 and E06000062 (Northamptonshire was split into North and West). The only solution I can think of code them both as Northampton before it was split, either with a new code or the pre-split code.
pupil_data[new_las == "E10000021", new_las := "E06000061"]
 
pupil_data_at_la <- copy(pupil_data) # because we'll need to save it at la-level too, and we're aggregating it to subregions below as the main output of this script. And I'd rather save everything we need at the section at the end, rather than save things at unexpected places in the script. 

state_funded_pupils_residence[new_las == "E10000021", new_las := "E06000061"]

state_funded_pupils_residence <- copy(state_funded_pupils_residence) # because we'll need to save it at la-level too, and we're aggregating it to subregions below as the main output of this script. And I'd rather save everything we need at the section at the end, rather than save things at unexpected places in the script. 



  ### 4.3. aggregating the local authorities to subregions
pupil_data <- aggregate_geographies_2(
  data = pupil_data, 
  lookup = la_itl_lookup,
  geog_from_data = "new_las",
  geog_from_lookup = "ladxxcd",
  geog_to_lookup = "itl221cd",
  count_names = c("value")
)

## it was all a difficult process, and the code is very messy, but it worked. Need to make the code better. 


## 5. final small fixes on the datasets, saving the outputs

pupil_data <- pupil_data[age %in% 5:15, ] 
pupil_data[, age := as.numeric(age)]

pupil_data_at_la <- pupil_data_at_la[age %in% 5:15, ]
pupil_data_at_la[, age := as.numeric(age)]
colnames(pupil_data_at_la)[1] <- "gss_code"


state_funded_pupils_residence <- state_funded_pupils_residence[age %in% 5:15, ]
state_funded_pupils_residence[, age := as.numeric(age)]
colnames(state_funded_pupils_residence)[1] <- "gss_code"



saveRDS(object = pupil_data_at_la,
        file = "data/intermediate/total_pupils_lad_5_15_2015_2024.rds") ## not automated yet, years hardcoded etc...won't take long to do

saveRDS(object = pupil_data,
        file = "data/intermediate/total_pupils_itl_5_15_2015_2024.rds")

saveRDS(object = state_funded_pupils_residence,
        file = "data/intermediate/resident_pupils_state.rds")
