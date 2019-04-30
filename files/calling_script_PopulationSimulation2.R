

library(PopulationSimulation)

do_one_simulation(first_birth_time  = 1, last_birth_time = 5,
                  time_step = 1, max_age = 4,  birth_rate = constant_birth_rate, 
                  base_mortality = time_indep_age_linear_base_mortality, 
                  incidence = time_indept_age_tent_incidence,
                  excess_mortality = time_indep_age_linear_excess_mortality)


