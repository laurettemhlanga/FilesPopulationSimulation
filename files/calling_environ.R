# incidence_estimates <- data.frame(method = c("recency", rep("synthetic_cohort",2), rep("themisa", 3), rep("epp", 3)),
#                                   year = c(2012, 2010, 2006, 2012, 2008, 2005, 2012, 2008, 2005),
#                                   estimate = c(1.72, 1.9, 1.9, 1.47, 1.79, 1.98, 1.52, 1.84, 2.01),
#                                   llevel = c(1.38, 0.8, 0.8, 1.23, 1.49, 1.62, 1.43, 1.76, 1.92),
#                                   ulevel = c(2.06, 3.1, 3.3, 1.72, 2.09, 2.34, 1.62, 1.93, 2.10))
#
#
#
# x = probability_surviving_infected(max_age = 3,
#                                list_of_times = 1985:1990,
#                                time_step = 1,
#                                excess_mortality = time_indep_age_linear_excess_mortality,
#                                base_mortality = time_indep_age_linear_base_mortality)
#
# incidence_matrix_exp()
#
#
# infected_population(susceptible = Y$susceptible_count[,-ncol(Y$susceptible_count)],
#                     incidence_matrix_mod = mod_incidence,
#                     cumulative_infected_survival = x)




maximum_inc <- function(tim)
  {

  return( ifelse(list_of_times <= 0, 0.006,
         ifelse( list_of_times <= 18, 0.006 + 0.123 * (list_of_times - 12),
                 ifelse(list_of_times <= 24, 0.8,
                        ifelse(list_of_times <= 31, 0.8 - (0.05 * (list_of_times -24)), 0)))))
}


# norm_fac <-  function(list_of_times,
#                       beta,
#                       sigm2)
# {
#
#   return(sqrt(2 * pi* sigm2) *(exp((beta - ((sigm2)/2)) * maximum_inc(list_of_times))))
# }



incidence_mahiane <- function(age, list_of_times, age_debut = 0,
                              beta = 2.3,
                              sigm2 = 0.5)
  {

  # to get reasonable prevalence multiply the realised incidence by 0.1

  norm_fac <- sqrt(2 * pi* sigm2) *(exp((beta - ((sigm2)/2)) * maximum_inc(list_of_times)))

  incidence <- 0.1 * ((norm_fac / ((age - age_debut) * sqrt(2 * pi * sigm2))) *
    exp(-(((log(age - age_debut) - beta)^2) / (2 * sigm2))))

  return(incidence)
}


#
# survival_data <- data.frame(age = 15:49, shape = rep(2, length(15:49)),
#                             scale = c(rep(16, 5), rep(15.4, 5),
#                                       rep(14.1, 5), rep(12.1, 5),
#                                       rep(11, 5), rep(10.1, 5),
#                                       rep(7.9, 5)),
#                             median = c(rep(13.3, 5), rep(12.8, 5),
#                                          rep(11.7, 5), rep(10, 5),
#                                          rep(9.1, 5), rep(8.4, 5),
#                                          rep(6.6, 5)))

survival_prob_infected <- function(matrix_of_ages,
                                   matrix_of_times,
                                   times_since_i,
                                   shape = 2,
                                   max_survival = 16, min_survival = 6.6,
                                   age_max = 50, age_min = 0)
  {

  scale <- max_survival -
    ((max_survival - min_survival)/(age_min - age_max)) * matrix_of_ages

  probability <-  exp(-(times_since_i/scale) ^ shape)

return(probability)

}


probability_surviving_infected(max_age = 3,
                               list_of_birth_times = 1:5,
                               time_step = 1,
                               excess_mortality = survival_prob_infected,
                               base_mortality = time_indep_age_linear_base_mortality)


survival_prob_infected(15, times_since_i =20 )





Y = do_one_simulation(first_birth_time = 12,
                  last_birth_time = 17,
                  time_step = 1,
                  max_age = 5,
                  incidence = incidence_mahiane,
                  birth_rate = constant_birth_rate,
                  base_mortality = time_indep_age_linear_base_mortality,
                  excess_mortality = survival_prob_infected_mahiane)


Y = do_one_simulation(first_birth_time = 12,
                      last_birth_time = 17,
                      time_step = 1,
                      max_age = 5,
                      incidence = incidence_mahiane,
                      birth_rate = constant_birth_rate,
                      base_mortality = time_indep_age_linear_base_mortality,
                      excess_mortality = survival_prob_infected_mahiane)
