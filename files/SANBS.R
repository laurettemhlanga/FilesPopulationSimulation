#blood donors

rm(list = ls())


library(PopulationSimulation)
library(inctools)

prevalence <- function(default = T,
                       constant = 0.02,
                       gradient,
                       time){

  if (isTRUE(default)){

    prevalence <- rep(constant, length(time))

  }else{

    prevalence <- constant + (gradient * time)

  }
  return(prevalence)
}



prevalence_recency <- function(default = T,
                               constant = 0.05,
                               gradient,
                               time){
  
  if (isTRUE(default)){
    
    prevalence <- rep(constant, length(time))
    
  }else{
    
    prevalence <- constant + (gradient * time)
    
  }
  return(prevalence)
}






prevalence_dataset <- function(donationtimes = seq(1,365, 0.25), #0.0025
                               prevalence_func = prevalence,
                               prevalences_recency_func = prevalence_recency
                               ){
  
  #takes in the donationdates, donorpopulation, prevalences, and prevalencerecency. outputs a dataframe of input vectors
  #

  prevalencedataset <- data.frame(donationtimes = donationtimes)

  for (times in seq_along(donationtimes)){
  
  prevalencedataset$HIV_status[times] = sample(x = c(1,0),size = 1, prob = c(prevalence_func(time = donationtimes[times]),
                                                                      1 - prevalence_func(time = donationtimes[times])))
  
  prevalencedataset$HIVrecency_status[times] = ifelse(prevalencedataset$HIV_status[times] = 1, 
                                                      sample(x = c(1,0), 
                                                             size = 1,
                                                             prob = c(prevalences_recency_func(time = prevalencedataset$donationtimes[times]),
                                                                      1 - prevalences_recency_func(time = prevalencedataset$donationtimes[times]))), 0)
                                                      
  
  }
  
  
  return(prevalencedataset)
}















# MDRI <- function(func, 
#                  lower, 
#                  upper,
#                  type,...){
# #theoratical mean duration of recent infection, wrapper function to R integrate and 
# #returns the 
#   mdri <- integrate(func,
#                     lower = lower, 
#                     upper = upper, type = type)$value
#   return(mdri)
# }



incprops(PrevH = 0.2, RSE_PrevH = 0.028, PrevR = 0.1, RSE_PrevR = 0.094, BS_Count = 10000, Boot = F,
         alpha = 0.05, MDRI = 200, RSE_MDRI = 0.05, FRR = 0.01, RSE_FRR = 0.2,
         BigT = 730)


