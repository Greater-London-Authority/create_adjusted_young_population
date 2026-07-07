library(dplyr)

source("R/functions/calc_probability_of_net_flow.R")
source("R/functions/adjust_single_year_net.R")

#Find the most likely series of annual net flows to match the overall change in the size of a
#cohort over the period for a given set of annual base gross flows (priors)

find_annual_net <- function(residual, base_flows_year, jump_scale = 16){
  
  target_total <- residual$residual_change
  
  probs_year <- base_flows_year %>%
    mutate(m_net = m_in - m_out) %>%
    mutate(k_net = m_in - m_out) %>%
    mutate(p_k = calc_probability_of_net_flow(k_net, m_in, m_out))
  
  #Iteration limit was added as there are cases where current algorithm doesn't converge to 0
  #This limit is based on the number of steps taken to converge if each is size 1
  max_iterations <- 1.5 * abs(target_total - sum(probs_year$m_net))
  
  probs_year <- adjust_single_year_net(probs_year, target_total, jump_scale)
  
  #repeatedly call 'adjust_single_year_net' until the sum of the net flows across all years
  #matches the total change between start and end points
  
  j <- 1
  
  while((abs(target_total - sum(probs_year$k_net)) > 0.3) & (j < max_iterations)){
    
    probs_year <- adjust_single_year_net(probs_year, target_total, jump_scale)
    j <- j + 1
  }
  
  return(probs_year)
}