library(dplyr)

source("R/functions/calc_probability_of_net_flow.R")

#Makes an adjustment to a single year's net migration figure
#Designed to be called repeatedly by find_annual_net() until target net for decade acheived
#Calculates direction and size of overall adjustment required to reach target end population
#Identifies which year adjustment can be made to with lowest 'cost'
#Applies adjustment to maximum value of: 'overall adjustment required'/jump_scale
#Smaller values of jump_scale generally give faster convergence, but may prove less reliable

adjust_single_year_net <- function(probs_year, target_total, jump_scale = 10){
  
  distance_from_target <- target_total - sum(probs_year$k_net)
  direction_to_target <- replace_na(distance_from_target/abs(distance_from_target), 1)
  
  #int_adjust is the size and direction of the change to make to a netflow
  int_adjust <- direction_to_target * ceiling(abs(distance_from_target/jump_scale))
  
  #TODO review the way that the 'least-cost' option is selected
  adj_probs <- probs_year %>%
    mutate(k_adj = k_net + int_adjust) %>%
    mutate(p_adj = calc_probability_of_net_flow(k_adj, m_in, m_out)) %>%
    mutate(p_k = calc_probability_of_net_flow(k_net, m_in, m_out)) %>%
    mutate(marginal_prop_cost = -p_adj/p_k) %>%
    mutate(cost_rank = rank(marginal_prop_cost, ties.method = "random")) %>%
    mutate(k_net = case_when(
      cost_rank == 1 ~ k_adj,
      TRUE ~ k_net
    ))
  
  return(adj_probs)
}