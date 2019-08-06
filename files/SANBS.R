#blood donors 
library(PopulationSimulation)
library(inctools)

prevalence <- function(default = T,
                       constant,
                       gradient,
                       time){
  
  if (isTRUE(default)){
    
    prevalence <- rep(constant, length(time))
    
  }else{
  
    prevalence <- constant + (gradient * time)
  
  }
  return(prevalence)
} 

#prevalence(constant = 0.001, time  = 1:5)




prevalence_recency <- function( prevalence_donors,
                                probability_testing_receny){
  
  prevalencerecency <- probability_testing_receny * prevalence_donors
  
  return(prevalencerecency)
}





dataset <- data.frame(donation_time = 1:20, 
                      donors = rep(100000, 20))


dataset$prevalence <- prevalence(constant = 0.0166, time  = 1:20)


dataset$prevalence <- prevalence_recency(prevalence_donors = prevalence(constant = 0.0166, time  = 1:20),
                                         probability_testing_receny = probability_of_recently_infected(time_in_years = 5,
                                                                                                       type = "step"))












MDRI <- 271
FRR <- 0.002

incprops(PrevH = 0.2, RSE_PrevH = 0.028, PrevR = 0.1, RSE_PrevR = 0.094, BS_Count = 10000, Boot = F,
         alpha = 0.05, MDRI = 200, RSE_MDRI = 0.05, FRR = 0.01, RSE_FRR = 0.2,
         BigT = 730)


