setwd("/home/laurette/Desktop/Github/PopulationSimulation/R")
files.sources = list.files("/home/laurette/Desktop/Github/PopulationSimulation/R")
sapply(files.sources, source)






Do_one_simulation <- function(first_birth_time = 1985, last_birth_time = 1990, time_step = 1, max_age = 3, 
                              birth_rate = constant_birth_rate, base_mortality = time_indep_age_linear_base_mortality, 
                              incidence = time_indept_age_tent_incidence,
                              excess_mortality = excess_mortality_fun)
  {
  
  #wrapper function to the functions in Population simulation project. 
  
  list_of_times <- seq(first_birth_time, last_birth_time, time_step )
  
  birth_count <- birth_counts(dates_needing_birth_counts = list_of_times,  
                              birth_rate = birth_rate, time_step = time_step)
  
  
  incidence_m <- incidence_matrix(max_age, list_of_times, incidence, time_step)
  
  
  base_mortality_m <- base_mortality_matrix(max_age, list_of_times, base_mortality, time_step)
  
  
  
  survival_prob <- susceptible_cumulative_survival(incidence_matrix = incidence_m, base_mortality_matrix = base_mortality_m,
                                                       time_step)
  
  
  susceptible_pop_counts <- susceptible_population(cumulative_survival_matrix = survival_prob,
                                                   birth_counts = birth_count)
  
  return(susceptible_pop_counts)
  
  
}




# setwd("/home/laurette/Desktop/Github/PopulationSimulation/R")
# files.sources = list.files("/home/laurette/Desktop/Github/PopulationSimulation/R")
# sapply(files.sources, source)



base_mortality_m <- base_mortality_matrix_fun(3,
                                              0:5,
                                              delta = 0.5,
                                              generate_base_mortality_fun = constant_base_mortality_fun)




survival_prob <- susceptible_cumulative_survival_fun(incidence_matrix = incidence_m,
                                                        base_mortality_matrix = base_mortality_m,
                                                        delta = 0.5)













susceptible_pop_counts <- susceptible_population(cumulative_survival_matrix = survival_prob,
                                                birth_counts = birth_count)








infected_0 <- incidence_matrix_exp(incidence_matrix = incidence_m,
                          base_mortality_matrix = base_mortality_m,
                          delta = 0.5) * susceptible_pop_counts





infected_survival_probs <- infected_mortality_array_fun (age_steps = 3, birth_dates = 0:5,
                                                     excess_mortality = excess_mortality_fun,
                                                     base_mortality = base_mortality_fun,
                                                     delta = 0.5)




infected <- infected_population_fun(susceptible_pop_counts = susceptible_pop_counts,
                                    incidence_prob = incidence_m,
                                    survival_probability = infected_survival_probs)

