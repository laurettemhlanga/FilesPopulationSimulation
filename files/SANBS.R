#blood donors

rm(list = ls())


library(PopulationSimulation)
library(inctools)

linear_prevalence <- function(intercept = 0.4,
                       gradient = 0,
                       time){

    prevalence <- intercept + (gradient * time)

  return(prevalence)
}



linear_prevalence_recency <- function(intercept = 0.5,
                               gradient = 0,
                               time){
  
  prevalence <- intercept + (gradient * time)
  
  return(prevalence)
}










































prevalence_dataset <- function(donationtimes = seq(1,1000, 1), 
                               prevalence_func = linear_prevalence,
                               prevalences_recency_func = linear_prevalence_recency
                               ){
  
#look at usage if apply functions to optimize code 

  prevalencedataset <- data.frame(donationtimes = donationtimes)
  
 # HIVrecency_status <- sapply(prevalencedataset, function(x) sample(c(1,0), size = 1 , prob = c(prevalence(time = x), 1 - prevalence(time = x))))

  for (times in seq_along(donationtimes)){
  
  prevalencedataset$HIV_status[times] = sample(x = c(1,0),size = 1, prob = c(prevalence_func(time = donationtimes[times]),
                                                                      1 - prevalence_func(time = donationtimes[times])))
  
  prevalencedataset$HIVrecency_status[times] = ifelse(prevalencedataset$HIV_status[times] == 1, 
                                                      sample(x = c(1,0), 
                                                             size = 1,
                                                             prob = c(prevalences_recency_func(time = prevalencedataset$donationtimes[times]),
                                                                      1 - prevalences_recency_func(time = prevalencedataset$donationtimes[times]))), NaN)
                                                      
  
  }
  
  
  return(prevalencedataset)
}




niterations<-10

prevalence_func = linear_prevalence
prevalences_recency_func = linear_prevalence_recency

donationtimes <- seq(1,100,1)
time_threshold <- 50

MDRI <- 0.5
big_T <- 730
FRR <- 0.01


incidence_estimates <- list()

for (iterations in 1:niterations){
  
  individualdata <- prevalence_dataset(donationtimes = donationtimes,
                                       prevalence_func = prevalence_func,
                                       prevalences_recency_func =prevalences_recency_func)
  
  
  data1 <- individualdata[individualdata$donationtimes <= time_threshold, ] # subsett
  data2 <- individualdata[individualdata$donationtimes > time_threshold,] # subsett
  
  prev_1 <- mean(data1$HIV_status)
  sigma_prev_1 <- sqrt(( prev_1 *(1 -  prev_1 ))/nrow(data1))
  pre_R_1 <- sum(data1$HIVrecency_status, na.rm = T)/sum(data1$HIV_status) #dealing with missing values 
  sigma_prev_R_1 <-  sqrt((  pre_R_1 *(1 - pre_R_1 ))/nrow(data1))
  
  prev_2 <- mean(data2$HIV_status)
  sigma_prev_2 <- sqrt(( prev_2 *(1 -  prev_2 ))/nrow(data2))
  pre_R_2 <-  sum(data2$HIVrecency_status, na.rm = T)/sum(data2$HIV_status)
  sigma_prev_R_2 <- sqrt((  pre_R_2 *(1 - pre_R_2 ))/nrow(data2))
  
  
  incidence_estimates[[iterations]] = incprops(PrevH = c(prev_1, prev_2), RSE_PrevH = c(sigma_prev_1, sigma_prev_2),
                                               PrevR = c(pre_R_1, pre_R_2), RSE_PrevR = c(sigma_prev_R_1, sigma_prev_R_2 ), 
                                               BS_Count = 10000, Boot = F, alpha = 0.05, MDRI = MDRI, RSE_MDRI = 0, 
                                               FRR = FRR, RSE_FRR = 0, BigT = big_T)$Incidence.Statistics
  
  
  
  
  #incidence_estimates <- rbind(incidence_estimates, incidence_estimates)
  
  # something <- incprops()
  # make data frema with (IPE_1/2, Isigma_1/2, DeltaIPE, DeltaILow, delta_I_upper, p value)
  # add all these estiamtes to growing data.frame(
  
}

# 
# variosu summaries
# 
# make histogra of p values
# compre to standad normalizePath()






prevalence_data <- data.frame(prevalences = numeric(), prevelences_recency = numeric())

for (iterations in 1:niterations){
  
  individualdata <- prevalence_dataset(donationtimes = donationtimes,
                                       prevalence_func = prevalence_func,
                                       prevalences_recency_func = prevalence_recency
  )
  
  prevalence_data <- rbind(prevalence_data, data.frame(prevalences = mean(individualdata$HIV_status), 
                                                       prevalences_recency = (mean(individualdata$HIVrecency_status))))
  
  
  
}

point_estimate <- data.frame(mean_prev = mean( prevalence_data$prevalences),
                             mean_prev_rece = mean(prevalence_data$prevalences_recency),
                             sd_prev = sd( prevalence_data$prevalences),
                             sd_prev_rece = sd(prevalence_data$prevalences_recency))

return(point_estimate)
}

























Prevalence_data <- function(donationtimes = seq(1,365, 0.25), 
                            prevalence_func = prevalence,
                            prevalences_recency_func = prevalence_recency,
                            niterations = 100
                            ){
  
  prevalence_data <- data.frame(prevalences = numeric(), prevelences_recency = numeric())
  
  for (iterations in 1:niterations){
    
    individualdata <- prevalence_dataset(donationtimes = donationtimes,
                                            prevalence_func = prevalence_func,
                                            prevalences_recency_func = prevalence_recency
                                            )
    
    prevalence_data <- rbind(prevalence_data, data.frame(prevalences = mean(individualdata$HIV_status), 
                                                         prevalences_recency = (mean(individualdata$HIVrecency_status))))
    
    
    
  }
  
 point_estimate <- data.frame(mean_prev = mean( prevalence_data$prevalences),
                              mean_prev_rece = mean(prevalence_data$prevalences_recency),
                              sd_prev = sd( prevalence_data$prevalences),
                              sd_prev_rece = sd(prevalence_data$prevalences_recency))
  
 return(point_estimate)
}



dataset <- Prevalence_data()



  
  
  
incidence <-   incprops(PrevH = dataset$mean_prev, RSE_PrevH = dataset$sd_prev, PrevR = dataset$mean_prev_rece, 
                        RSE_PrevR = dataset$sd_prev_rece, BS_Count = 10000, Boot = F,
                        alpha = 0.05, MDRI = 200, RSE_MDRI = 0.05, FRR = 0.001, RSE_FRR = 0,
                        BigT = 730)
 




