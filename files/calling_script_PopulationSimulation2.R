

library(PopulationSimulation)



first_birth_time  = 1; last_birth_time = 5
time_step = 0.5; max_age = 4; birth_rate = constant_birth_rate; pmtct_birth_rate = constant_pmtct_rate
base_mortality = time_indep_age_linear_base_mortality; incidence = time_indept_age_tent_incidence
excess_mortality = tau_linear_excess_mortality


do_one_simulation(first_birth_time  = 1, last_birth_time = 5,
                  time_step = 1, max_age = 4,  birth_rate = constant_birth_rate, 
                  base_mortality = time_indep_age_linear_base_mortality, 
                  pmtct_birth_rate = constant_pmtct_rate,
                  incidence = time_indept_age_tent_incidence,
                  excess_mortality = tau_linear_excess_mortality)


do_one_simulation(first_birth_time  = 1, last_birth_time = 5,
                  time_step = 0.5, max_age = 4,  birth_rate = constant_birth_rate, 
                  base_mortality = time_indep_age_linear_base_mortality, 
                  pmtct_birth_rate = constant_pmtct_rate,
                  incidence = time_indept_age_tent_incidence,
                  excess_mortality = tau_linear_excess_mortality)
