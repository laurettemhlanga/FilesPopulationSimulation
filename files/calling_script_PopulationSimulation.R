
setwd("H:/FilesPopulationSimulation")
files.sources = list.files("H:/PopulationSimulation/R")
sapply(files.sources, source)

list_of_times = seq(t_1, t_2 , delta)


birth_count <- birth_counts_fun(list_of_times,
                                delta = 0.5,
                                birthrate = constant_birth_rate_fun)


incidence_m <- incidence_matrix_fun(age_steps = 3,
                                    birth_dates = 0:5,
                                    generate_incidence_fun = constant_tent_incidence_fun,
                                    delta = 0.5)


base_mortality_m <- base_mortality_matrix_fun(3,
                                              0:5,
                                              delta = 0.5,
                                              generate_base_mortality_fun =constant_base_mortality_fun)




survival_prob <- susceptible_cumulative_survival_fun(incidence_matrix = incidence_m,
                                                        base_mortality_matrix = base_mortality_m,
                                                        delta = 0.5)








#survival_prob * birth_count




susceptible_pop_counts <- susceptible_population(cumulative_survival_matrix = survival_prob,
                                                birth_counts = birth_count)









infected_0 <- incidence_matrix_exp(incidence_matrix = transform_data(incidence_m),
                          base_mortality_matrix = transform_data(base_mortality_m),
                          delta = 0.5) * susceptible_pop_counts


infected_survival_probs <- infected_mortality_array_fun (age_steps = 3, birth_dates = 0:5,
                                                     excess_mortality = excess_mortality_fun,
                                                     base_mortality = base_mortality_fun,
                                                     delta = 0.5)




infected <- infected_population_fun(susceptible_pop_counts = susceptible_pop_counts,
                                    incidence_prob = incidence_m,
                                    survival_probability = infected_survival_probs)

