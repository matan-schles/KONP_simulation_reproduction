library(survival)
library(KONPsurv)
library(parallel)
library(doParallel)
library(foreach)
library(doRNG)

Scenario_Null_4_light<-function(n) # 25% for for all groups
{
  trt<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
  U1 <- runif(n/4) 
  U2 <- runif(n/4) 
  U3 <- runif(n/4)
  U4 <- runif(n/4)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  event_time<-c(T1,T2,T3,T4)
  C1 <- rlnorm(n/4,1.1,0.25)
  C2 <- rlnorm(n/4,1.1,0.25)
  C3 <- rlnorm(n/4,1.1,0.25)
  C4 <- rlnorm(n/4,1.1,0.25)
  censor_time<-c(C1,C2,C3,C4)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_Null_4_Medium<-function(n) # 50% for all groups
{
  trt<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
  U1 <- runif(n/4) 
  U2 <- runif(n/4) 
  U3 <- runif(n/4)
  U4 <- runif(n/4)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  event_time<-c(T1,T2,T3,T4)
  C1 <- rlnorm(n/4,0,0.25)
  C2 <- rlnorm(n/4,0,0.25)
  C3 <- rlnorm(n/4,0,0.25)
  C4 <- rlnorm(n/4,0,0.25)
  censor_time <- c(C1,C2,C3,C4)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 24% for trt=3,4 and 59% for trt=1,2

Scenario_Null_4_part_diff<-function(n) 
{
  trt<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
  U1 <- runif(n/4) 
  U2 <- runif(n/4) 
  U3 <- runif(n/4)
  U4 <- runif(n/4)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  event_time<-c(T1,T2,T3,T4)
  C1_unif<-runif(n/4,0,10)
  C1_exp<-rexp(n/4,0.85)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/4,0,10)
  C2_exp<-rexp(n/4,0.85)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3<-runif(n/4,0,10)
  C4<-runif(n/4,0,10)
  censor_time <- c(C1,C2,C3,C4)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


# 58% for trt==1, 41% fro trt==2, 24% for trt==3, 18% for trt==4
Scenario_Null_4_all_diff<-function(n) 
{
  trt<-c(rep(1,n/4),rep(2,n/4),rep(3,n/4),rep(4,n/4))
  U1 <- runif(n/4) 
  U2 <- runif(n/4) 
  U3 <- runif(n/4)
  U4 <- runif(n/4)
  T1 <- (1/U1)-1
  T2 <- (1/U2)-1
  T3 <- (1/U3)-1
  T4 <- (1/U4)-1
  event_time<-c(T1,T2,T3,T4)
  C1_unif<-runif(n/4,0,10)
  C1_exp<-rexp(n/4,0.85)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif <- runif(n/4,0,10)
  C2_exp <- rexp(n/4,0.25)
  C2 <- ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  C3 <- runif(n/4,0,10)
  C4 <- rlnorm(n/4,1.5,0.25)
  censor_time <- c(C1,C2,C3,C4)
  delta <- as.numeric(event_time<=censor_time)
  time <- ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}

scenarios <- ls()


other_tests <- function(time,delta,trt)
{
  
  procedure_time <- proc.time()
  
  #Making the trt vector be equal to 0 and 1
  trt_ex <- rep(NA,length(trt))
  trt_unq <- unique(trt)
  trt_ex[trt==trt_unq[1]] <- 0
  trt_ex[trt==trt_unq[2]] <- 1
  trt <- trt_ex
  
  n <- length(time)
  
  
  ##############
  ## Log-Rank ##
  ##############
  #Do the logrank test on time, delta and trt
  fit <- survdiff(Surv(time, delta) ~ trt , rho=0)
  #Save the Pvalue
  Pvalue_logrank <- 1 - pchisq(fit$chisq, 1)
  
  
  ############################
  ## Peto-Prentice-Wilcoxon ##
  ############################
  fit <- survdiff(Surv(time, delta) ~ trt , rho=1)
  #Save the Pvalue
  Pvalue_ppw <- 1 - pchisq(fit$chisq,1)
  
  
  pv_list <- list(Pvalue_logrank = Pvalue_logrank, Pvalue_ppw = Pvalue_ppw)
  
  run_time <- ((proc.time()-procedure_time)["elapsed"])
  print(paste("running time for other tests with n=",n,"is",run_time,"seconds"))
  return(pv_list)
}

power_of_test <- function(iterations,imputations,permutations,n,scenario)
{
  procedure_time <- proc.time()
  nr.sim.cores = detectCores()-1
  cl = makeCluster(nr.sim.cores,outfile="") #open nr.sim.cores instances of R
  registerDoParallel(cl)
  res=foreach(core = 1:nr.sim.cores, .packages = c('KONPsurv',"survival","parallel"),
              .export =c(scenarios,"other_tests") ,.options.RNG=1234,
              .errorhandling ="pass" ) %dorng% {
                
                p_pearson <- c()
                p_lr <- c()
                p_cauchy <- c()
                
                # p_pearson_first <- c() #meant for the pv obatained from one imputation
                # p_lr_first <- c()
                
                
                p_logrank <- c()
                p_ppw <- c() #peto prentice wilcoxon

                
                # censorship_1<-c()
                # censorship_2<-c()

                
                
                for (j in 1:ceiling((iterations)/nr.sim.cores))
                {
                  data <- (get(scenario))(n)
                  
                  trt<-data$trt
                  time<-data$time
                  delta<-data$delta
                  
                  K <- length(unique(trt))
                  
                  
                  while(min(sapply(1:K,function(x){sum(delta[trt==x])}))<2) #  #making sure we have at least 2 events in each group
                  {
                    data <- (get(scenario))(n)
                    
                    trt<-data$trt
                    time<-data$time
                    delta<-data$delta
                  }
                  
                  # censorship_1[j] <- 1-mean(delta[trt==1])
                  # censorship_2[j] <- 1-mean(delta[trt==2])
                  
                  a <- konp_test(time = time,status = delta,group = trt,n_perm = permutations,n_impu = imputations)
                  
                  p_pearson[j] <- a$pv_chisq
                  p_lr[j] <- a$pv_lr
                  p_cauchy[j] <- a$pv_cauchy
                  
                  # p_pearson_first[j] <- a$pv_chisq_first
                  # p_lr_first[j] <- a$pv_lr_first
                  
                  
                  b <- other_tests(time = time,delta = delta,trt = trt) #Tests that are not HHG-KM and YP
                  
                  p_logrank[j] <- b$Pvalue_logrank
                  p_ppw[j] <- b$Pvalue_ppw
                  
                }# end of loop for iteration
                  
                return(data.frame(p_pearson=p_pearson, p_lr=p_lr,
                                  p_cauchy=p_cauchy,
                                  #p_pearson_first=p_pearson_first,p_lr_first=p_lr_first,
                                  p_logrank=p_logrank, p_ppw=p_ppw))
              }#end of paralleling
  stopCluster(cl)
  
  saveRDS(res,"results_list.rds")
  
  res <- do.call("rbind",res) #unites all the lists given from different cores into one dataframe
  
  
  power_vec <- apply(res,2,function(x){sum(x<=0.05)/(length(x))})
  # names(power_vec) <- colnames(res)
  res_vec <- c(n,power_vec)
  names(res_vec)[1] <- c("n")

  if (sum(list.files()=="power_results.csv")>0)
  {
    power_results <- read.csv("power_results.csv",header = T)
    res_df <-  as.data.frame(t(res_vec))
    res_df$scenario <- scenario
    power_results <- rbind(power_results,res_df)
  }else
  {
    power_results <- as.data.frame(t(res_vec))
    power_results$scenario <- scenario
  }
  
  write.csv(power_results,"./power_results.csv",row.names = F)
  
  run_time <- (proc.time()-procedure_time)["elapsed"]
  print(paste("Total run time for all tests with n=",n,",",iterations,"iterations,",permutations,"permutations, and",
              imputations,"imputations","is:",run_time))
}


#sink("./running time.txt")



for (n in c(100,200,300,400))
{
  for (scen in scenarios)
  {
    power_of_test(iterations = 2*10^3,imputations = 1,permutations = 10^3,n = n,scenario = scen)
  }
}

