library(dplyr)
library(tidyr)
library(foreach)
library(parallel)
library(doParallel)

source("R/functions/calc_probability_of_net_flow.R")
source("R/functions/optimise_gross_flows.R")
source("R/functions/find_annual_net.R")
source("R/functions/adjust_single_year_net.R")

#takes the mid-year estimates with components of change, returns a set of modelled international migration
#flows that are consistent with:
#   - the populations at the specified start and end periods
#   - the original births, deaths, and domestic migration estimates
#   - all of the change over the period being explained by traditional components of change (i.e. no UPC)


fit_migration_flows <- function(residual_change_cohort, annual_international) {
  
  time_start <- Sys.time()
  message("Fitting migration flows")
  
  # fit annual flows
  
  #create vector of areas to fit.
  #'sample' is used so that the codes are not in their default order (i.e. bunched by region).
  #this speeds up the total run time as it more evenly distributes London
  #authorities across the parallel threads.  London authorities tend to be
  #slower to process as they have larger volumes of international migration and larger residuals
  
  area_list <- sample(unique(residual_change_cohort$gss_code))
  
  n_cores <- 3 * detectCores()/4 # Or manually set this to the number of cores you want to use
  cl <- makeCluster(n_cores)
  registerDoParallel(cl)
  n <- floor(length(area_list)/n_cores)
  a <- list()
  
  for(i in 1:(n_cores-1)){ a[[i]] <- ((i*n)-(n-1)):(i*n) }
  a[[n_cores]] <- ((n_cores*n)-(n-1)):length(area_list)
  
  pkgs <- c("dplyr","tidyr")
  local_pkgs <- c("find_annual_net",
                  # "unroll_max_cohort",
                  "optimise_gross_flows",
                  "calc_probability_of_net_flow",
                  "adjust_single_year_net")
  
  residual_lad_list <- a %>%
    lapply(FUN = function(x){
      filter(residual_change_cohort, gss_code %in% area_list[x])
    })
  
  annual_international_list <- a %>%
    lapply(FUN = function(x){
      filter(annual_international , gss_code %in% area_list[x])
    })
  
  parallel_output <- foreach(i = 1:n_cores,
                             .combine=bind_rows,
                             .packages=pkgs,
                             .export=local_pkgs) %dopar% {
                               
                               output <- list()
                               
                               for(j in a[[i]]){
                                 
                                 residual_lad <- filter(residual_lad_list[[i]], gss_code == area_list[j])
                                 base_flow_lad <- filter(annual_international_list[[i]], gss_code == area_list[j])
                                 
                                 residual_list <- split(residual_lad, f = list(residual_lad$sex,
                                                                               residual_lad$cohort))
                                 
                                 base_flow_list <- split(base_flow_lad, f = list(base_flow_lad$sex,
                                                                                 base_flow_lad$cohort))
                                 
                                 modelled_net_list <- mapply(find_annual_net,
                                                             residual = residual_list,
                                                             base_flows_year = base_flow_list,
                                                             jump_scale = 16,
                                                             SIMPLIFY = FALSE)
                                 
                                 output[[j]] <- modelled_net_list %>%
                                   bind_rows %>%
                                   mutate(age = year - cohort) %>%
                                   select(gss_code, gss_name, year, age, sex, netflow = k_net, m_in, m_out) %>%
                                   mutate(model_flows = optimise_gross_flows(m_in, m_out, netflow)) %>%
                                   unnest_wider(col = model_flows) %>%
                                   select(-c(m_in, m_out))
                               }
                               
                               bind_rows(output)
                             }
  
  modelled_flows <- parallel_output %>%
    rename(international_in = inflow,
           international_out = outflow) %>%
    select(-netflow) %>%
    mutate(international_net = international_in - international_out) %>%
    pivot_longer(cols = c("international_net", "international_in", "international_out"),
                 names_to = "component")
  
  message("Flow fitting complete: ", Sys.time(), "\nDuration: ", round(Sys.time() - time_start, 2))
  
  return(modelled_flows)
}