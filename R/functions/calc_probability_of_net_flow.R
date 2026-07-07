calc_probability_of_net_flow <- function(k_net, base_inflow, base_outflow){
  
  mean_net <- base_inflow - base_outflow
  sd_net <- sqrt(base_inflow + base_outflow)
  prob_k <- pnorm(k_net + 0.5, mean_net, sd_net) - pnorm(k_net - 0.5, mean_net, sd_net)
  
  out_prob <- ifelse(base_inflow + base_outflow > 35 & prob_k != 0,
                     prob_k,
                     exp(2*sqrt(base_inflow*base_outflow)-(base_inflow + base_outflow)) * (base_inflow/base_outflow)^(k_net/2) * besselI(2*sqrt(base_inflow*base_outflow), abs(k_net), expon.scaled = TRUE)
  )
  
  return(out_prob)
}

calc_probability_of_net_flow <- Vectorize(calc_probability_of_net_flow)