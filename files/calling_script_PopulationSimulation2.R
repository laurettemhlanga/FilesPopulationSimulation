
rm(list = ls())

library(PopulationSimulation)



x = do_one_simulation(first_birth_time  = 1980, last_birth_time = 2000,
                  time_step = 0.5, max_age = 50,  birth_rate = constant_birth_rate, 
                  base_mortality = time_indep_age_linear_base_mortality, 
                  pmtct_birth_rate = constant_pmtct_rate,
                  incidence = incidence_mahiane,
                  excess_mortality = tau_linear_excess_mortality)





population_status <- extract_population_status_chunk(survey_date = c(1992, 1996, 2000),
                                                          first_birth_date = 1980,
                                                          time_step = 0.5,
                                                          population = x)




population_prevalence_1986 <- calculate_prevalence(population_at_date = population_status[[3]],
                                         keeping_infection_time = F)



population_prevalence_1996 <- calculate_prevalence(population_at_date = population_status_1986[[2]],
                                                   keeping_infection_time = F)


population_prevalence_2000 <- calculate_prevalence(population_at_date = population_status_1986[[1]],
                                                   keeping_infection_time = F)

prevalences <- data.frame(ages = )




inclusion_exclusion_criteria(required_ages = 2:4,
                             prevalence = population_prevalence_1986,
                             time_step = 0.5,
                             radii = 1)





recent_positive <- probability_recent_among_positive(time_step = 0.5,
                                                     type = "weibull",
                                                     population_at_date = population_status_1986[[3]],
                                                     probability_of_recent_infection = probability_of_recently_infected,
                                                     keeping_infection_time = TRUE)





##############################################################################################################################################
###################################################SURVEYS####################################################################################
surveydate_2 = 1988
surveydate_1 = 1986
time_step = 0.5


corresponding_indices <- which(!is.nan(population_prevalence_1986)) + ((surveydate_2 - surveydate_1) / time_step)

cohort_1988 <- population_prevalence_1988[corresponding_indices[corresponding_indices <= length(population_prevalence_1986)]]

cohort_1986 <- population_prevalence_1986[which(!is.nan(population_prevalence_1986))][1:length(cohort_1988)]




z = age_partitioned_prevalence(overall_prevalence1 = cohort_1986,
                               overall_prevalence2 = cohort_1988,
                               sigma_prevalence = rep( 0.0002, length(cohort_1986)),
                               cluster_number =  1:5,
                               cluster_size =  c(800, 300, 325, 1000, 1008),
                               ages = 4:6)


z[(z$age == 4), ]






survey_data <- pps_survey_selfweighting_structured(cluster_number =  1:5,
                                                   cluster_size =   c(800, 300, 325, 1000, 1008),
                                                   ages = 15,
                                                   num_cluster_sample = 2,
                                                   ind_per_cluster = 100,
                                                   overall_prevalence1 = cohort_1986,
                                                   overall_prevalence2 = cohort_1988,
                                                   sigma_prevalence = rep( 0.0002, length(cohort_1986)))



# z = age_partitioned_prevalence(overall_prevalence1 = population_prevalence_1986,
#                               overall_prevalence2 = population_prevalence_1988,
#                               sigma_prevalence = c(0.00019, 0.00022, 0.00021, 0.0002),
#                               cluster_number =  1:4,
#                               cluster_size =  c(800, 300, 325, 1000),
#                               ages = 15:18)







Survey_pps(cluster_number =  1:4,
           cluster_size =   c(800, 300, 325, 1000),
           num_cluster_sample = 2,
           ind_per_cluster = 100,
           overall_prevalence1 = c(0.195, 0.247, 0.3115, 0.35),
           overall_prevalence2 = c(0.25, 0.26, 0.343, 0.387),
           sigma_prevalence = c(0.00019, 0.00022, 0.00021, 0.0002))







filter(z, cluster_id == c(1,3))



#######################################################################################################################################################################
###################################################Individual Data#####################################################################################################


example <- collapsed_population[42, ncol(collapsed_population)]


example <- example[which(is.na(example) == FALSE)]


individual_data <- data.frame(individual_data_id = 1:200,
                              age  = rep(5, 200),
                              hiv_status = sample(x = c(0,1), size = 200, replace = T, 
                                                  prob = c(1 - (collapsed_population[42, ncol(collapsed_population)]), 
                                                                                                collapsed_population[42, ncol(collapsed_population)])))

individual_data$time_since_infection <- ifelse(individual_data$hiv_status == 1,
                                               (sample(x = (1:length(6:(ncol(example)-1))) * time_step, replace = TRUE , size = sum(individual_data$hiv_status), 
                                               prob = example[, (6:(ncol(example)-1))])), 0)

####################################################################################################################################################################

library(dplyr)

 cross_sectional <- filter(collapsed_population, cohort_calender_date == 5)
 
 birth_cohort <- filter(collapsed_population_prevalence, cohort_d_o_b == 1, cohort_age == 3.5)
 
 
 new_data_p1 <- collapsed_population %>%
   filter(cohort_calender_date == 3.5)
   
 
 new_data_p2 <- collapsed_population %>%
   filter(cohort_calender_date == 6.5)
 
 a = array(1:12, dim = c(2,2,3))
 

 
 
 
 extract_age_prevalence <- function(time_step,
                                    first_birth_time,
                                    current_date_parsed,
                                    data_structure, 
                                    bin_size){
   
   
  # #time_since_infection <- seq(time_step, dim(data_structure)[2] * time_step, time_step )
  #  
  # prevalence <- array(NA, dim = dim(data_structure))
  #  
  #  for (tsi_index in 1:dim(data_structure)[3]){
  #  
  #    prevalence[ , , tsi_index]<- data_structure[ , , tsi_index] / rowSums(data_structure, dims = 2, na.rm = T)
  #  
  #  }
   
   #matrix_index <- (row(total_prevalence) - col(total_prevalence))
   
   ages <- seq(time_step, dim(data_structure)[2] * time_step, time_step )
   
   list_of_date_births <- seq(first_birth_time, first_birth_time + max(ages), time_step)
   
   current_date = matrix(NA, nrow = length(list_of_date_births), ncol = length(ages))
   ages_current = matrix(NA, nrow = length(list_of_date_births), ncol = length(ages))
   
   
   time_since_infection <- seq(time_step, dim(data_structure)[2] * time_step, time_step )
   
   prevalence <- array(NA, dim = dim(data_structure))
   
   for (tsi_index in 1:dim(data_structure)[3]){ # finds/calculates prevalence 
     
     prevalence[ , , tsi_index]<- data_structure[ , , tsi_index] / rowSums(data_structure, dims = 2, na.rm = T)
     
   }
   
   
   for (time_index in seq_along(list_of_date_births)){ # each possible entry in the matrix is given actual current date make a separate function
     
     for (age_index in seq_along(ages)){
       
       current_date[time_index, age_index] <- list_of_date_births[time_index] + ages[age_index]
       
       ages_current[time_index, age_index] <- ages[age_index]
     }
     
   }
   
   current_date_element_position <- which(current_date == current_date_parsed)
   age_at_date <- as.vector(ages_current)[current_date_element_position]
   
   prevalence_age_at_date = list()
   
   for (tsi_index in 1:dim(data_structure)[3]){ # extracts the prevalence at each required calender time 
     
   prevalence_at_date <-  prevalence[, , tsi_index][current_date_element_position]
   
   prevalence_age_at_date[[tsi_index]] <- data.frame(age = age_at_date, prevalence = prevalence_at_date)
   
   }
   
   return(data.frame(age = age_at_date,
                     prevalence = prevalence_at_date))
   
   
 }
 
 
 ##################################################################################################################################
 extract_population_date<- function(population,
                                    time_step,
                                    first_birth_time) 
   
   #assigns the current date to the aerial view of the simulated popualtion 
   
 {
   ages <- seq(time_step, dim(population)[2] * time_step, time_step )
   
   time_since_infection <- ages
   
   list_of_date_births <- seq(first_birth_time, first_birth_time + max(ages), time_step)
   
   current_date = matrix(NA, nrow = length(list_of_date_births), ncol = length(ages))
   ages_current = matrix(NA, nrow = length(list_of_date_births), ncol = length(ages))
   
   
   for (time_index in seq_along(list_of_date_births)){
     
     for (age_index in seq_along(ages)){
       
       current_date[time_index, age_index] <- list_of_date_births[time_index] + ages[age_index]
       
       ages_current[time_index, age_index] <- ages[age_index]
     }
     
   }
   return(list(ages_current, current_date))
  }
 ##################################################################################################################################
 
 extract_population_date<- function(population,
                                    survey_date,
                                    look_up_list
                                    )
   # extracts the population status at the required calender time 
 {
   current_date <- look_up_list[[2]]
   ages_current <- look_up_list[[1]]
   
   current_date_element_position <- which(current_date == floor(survey_date))
   age_at_date <- as.vector(ages_current)[current_date_element_position]
   
   
   current_date_element_position <- which(current_date == survey_date)
   age_at_date <- as.vector(ages_current)[current_date_element_position]
   
   population_age_at_date = data.frame(age = age_at_date)
   
   for (tsi_index in 1:dim(population)[3]){ 
     
     population_at_date <-  population[, , tsi_index][current_date_element_position]
     
     #population_age_at_date[[tsi_index]] <- data.frame(age = age_at_date, population _count = population_at_date)
     
     population_age_at_date <- cbind(population_age_at_date,  population_at_date)
     
   }
   
   return(data.frame(population_age_at_date))
   
   
 }
 