
devtools::load_all(".")
library(PopulationSimulation)



birth_count <- birth_counts_fun(total_births = 1000,
                                birth_dates = 1945:1950,
                                delta = 1,
                                birthrate = birth_rate_fun)

incidence_m <- incidence_matrix_fun(age_steps = 3, birth_dates = 0:5, incidence_fun)

base_mortality_m <- base_mortality_matrix_fun(3, 0:5, generate_base_mortality_fun = base_mortality_fun)

survival_prob <- susceptible_cumulative_survival_fun(incidence_matrix = incidence_m,
                                                        base_mortality_matrix = base_mortality_m,
                                                        delta = 1)

susceptible_pop_counts <- susceptible_population_fun(cumulative_survival_matrix = survival_prob,
                                                birth_counts = birth_count)


infected_survival_probs <- infected_mortality_array_fun (age_steps = 3, birth_dates = 1945:1950,
                                                     excess_mortality = excess_mortality_fun,
                                                     base_mortality = base_mortality_fun)

infected <- infected_population_fun(susceptible_pop_counts = susceptible_pop_counts,
                                    incidence_prob = incidence_m,
                                    survival_probability = infected_survival_probs)

