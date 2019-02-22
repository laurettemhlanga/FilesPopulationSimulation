#written by laurette Mhlanga an adaptation on the sampling with pps 
#psu to achieve equal weights in a 
#for every individual 

library(tidyverse)

partition_prevalence <- function(overall_prevalence1,
                                 overall_prevalence2,
                                 sigma_prevalence,
                                 cluster_number,
                                 cluster_size
#function that divides/partitions  the overall prevalence and assigns to clusters
#into given clusters                                                                  
                                 
){
  
  CoV_overall =  sigma_prevalence/overall_prevalence1
  
  cluster_prevalences = rnorm(length(cluster_number), 
                              mean = overall_prevalence1, 
                              sd = sigma_prevalence)
  # to avoid negative prevalences we check is there are any negatives in the prevalences vector and if they are we replace
  #the negative values by the mean of the simulated cluster_prevalences
  
  if (any(cluster_prevalences < 0) == T){
    
    cluster_prevalences = replace(cluster_prevalences, which(cluster_prevalences<0), mean(cluster_prevalences))
    
  }else{
    
    cluster_prevalences = cluster_prevalences 
  }
  
  
  
  cluster_CoV = sd(cluster_prevalences) / mean(cluster_prevalences)
  
  cluster_prevalences_R_Adj = mean( cluster_prevalences ) + (cluster_prevalences - mean(cluster_prevalences )) * (CoV_overall / cluster_CoV)
  
  
  Cluster_factor = sum(cluster_size * cluster_prevalences_R_Adj) / sum(cluster_size)
  cluster_prevalences_final1 = (cluster_prevalences_R_Adj / Cluster_factor ) * overall_prevalence1
  
  cluster_prevalences_final2 = (cluster_prevalences_final1)*(overall_prevalence2 / overall_prevalence1)
  
  prevcheck = c(sum(cluster_size * cluster_prevalences_final1) / sum(cluster_size), sum(cluster_size * cluster_prevalences_final2) / sum(cluster_size))
  
  return(list(prevcheck, data.frame( cluster_id = cluster_number, 
                                     cluster_prevalence_t1 = cluster_prevalences_final1, 
                                     cluster_prevalence_t2 = cluster_prevalences_final2 )))
  
}

age_partitioned_prvalence <-  function(prevalence_1,
                                       prevalence_2,
                                       sigma_prevalence_1,
                                       cluster_number,
                                       cluster_size,
                                       ages
                                       ){

#functions takes in a vector of prevalences from time 1 and time 2 and partitions to the prevalences to 
#the number of regions. this a wrapper function to partition_prevalence
#runs a loop  through a a vector of prevalences. 

partitioned_age_prevalance <-  as.list(rep(NA, length(ages)))
 
counter = 1

for (counter in 1:length(ages)){
  
  
  partitioned_age_prevalance[[counter]] <- partition_prevalence(overall_prevalence1 = prevalence_1[counter],
                                                     overall_prevalence2 = prevalence_2[counter],
                                                     sigma_prevalence = sigma_prevalence_1[counter],
                                                     cluster_number = (1:length(cluster_size)), 
                                                     cluster_size)[[2]]
  
  partitioned_age_prevalance[[counter]] <- cbind(age = rep(ages[counter], length(cluster_size)), 
                                                 partitioned_age_prevalance[[counter]])
  
  counter = counter + 1 
  
}


  partitioned_age_prevalance <- bind_rows(partitioned_age_prevalance)
  
  return(partitioned_age_prevalance)
  
}
 



##example
age_partitioned_prvalence(prevalence_1 = c(0.195, 0.247, 0.3115, 0.35),
                          prevalence_2 = c(0.25, 0.26, 0.343, 0.387),
                          sigma_prevalence_1 = c(0.00019, 0.00022, 0.00021, 0.0002), 
                          cluster_number =  1:4,
                          cluster_size =  c(800, 300, 325, 1000),
                          ages = 15:18)


