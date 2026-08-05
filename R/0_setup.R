## set up

## 0. libraries and functions
functions_to_read <- list.files("R/functions")

lapply(
  paste0("R/functions/", functions_to_read),
  FUN = source
)

library(readxl)

## 1. create folders we need that we don't have

check_and_create_dir("data")
check_and_create_dir("data/raw")
check_and_create_dir("data/intermediate")
check_and_create_dir("data/processed")


## 2. downloading data in folders created above
## a few instances where we can't download data directly from the london datastore into a script, meaning we have a save it in a local folder and read it in from that folder. Doing all of them here, because they take a while and we only need to do it once. 
## basically, it's when files are up there in rds format. For some reason can no longer read them in directly. 
## maybe londondatar2 will fix this when it's working

## also, using the function to download the myes here. Would rather get rid of that though eventually. 

download.file(url = "https://data.london.gov.uk/download/e561x/f47d4a48-4b03-447e-a9ac-918a8d0eecab/gp_sya_lad.rds",
              destfile = "data/intermediate/gp_sya_lad.rds")

#download.file(url = "https://data.london.gov.uk/download/ex9jd/ba752f34-0b54-4184-9251-8e2e94ae97ee/full_modelled_estimates_series_EW(2023_geog).rds",
#              destfile = "data/intermediate/mye_2001_24_rev.rds")


url_current_mye <- "https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/estimatesofthepopulationforenglandandwales/mid2011tomid2025detailedtimeseries/myebtablesenglandwales20112025.xlsx" # DR - changed to 2025 version of myes

fetch_and_clean_mye_data(url_raw = url_current_mye,
                         fpath_raw = "data/raw/mye_2011_on(2023_geog).xlsx",
                         fpath_clean = "data/intermediate/mye_2011_on(2023_geog).rds",
                         sheet_name = "MYEB2")





