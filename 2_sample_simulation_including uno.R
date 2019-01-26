  library(survival)
  library(KONPsurv)
  library(YPmodel)
  library(survAWKMT2)
  library(parallel)
  library(doParallel)
  library(foreach)
  library(doRNG)
  
  Scenario_Null_light<-function(n) # 25% for trt=2 and 25% for trt=1
  {
    trt<-c(rep(1,n/2),rep(2,n/2))
    U1 <- runif(n/2) 
    U2 <- runif(n/2) 
    T1 <- (1/U1)-1
    T2 <- (1/U2)-1
    event_time<-c(T1,T2)
    C1 <- rlnorm(n/2,1.1,0.25)
    C2 <- rlnorm(n/2,1.1,0.25)
    censor_time<-c(C1,C2)
    delta<-as.numeric(event_time<=censor_time)
    time<-ifelse(delta==1,event_time,censor_time)
    return(data.frame(trt=trt,time=time,delta=delta))
  }
  
  
  Scenario_Null_Medium<-function(n) # 50% for trt=2 and 50% for trt=1
  {
    trt<-c(rep(1,n/2),rep(2,n/2))
    U1 <- runif(n/2) 
    U2 <- runif(n/2) 
    T1 <- (1/U1)-1
    T2 <- (1/U2)-1
    event_time<-c(T1,T2)
    C1 <- rlnorm(n/2,0.0,0.25)
    C2 <- rlnorm(n/2,0,0.25)
    censor_time<-c(C1,C2)
    delta<-as.numeric(event_time<=censor_time)
    time<-ifelse(delta==1,event_time,censor_time)
    return(data.frame(trt=trt,time=time,delta=delta))
  }
  
  
  # 40% for trt=2 and 59% for trt=1
  
  Scenario_Null_diff<-function(n) 
  {
    trt<-c(rep(1,n/2),rep(2,n/2))
    U1 <- runif(n/2) 
    U2 <- runif(n/2) 
    T1 <- (1/U1)-1
    T2 <- (1/U2)-1
    event_time<-c(T1,T2)
    C1_unif<-runif(n/2,0,10)
    C1_exp<-rexp(n/2,0.85)
    C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
    C2_unif <- runif(n/2,0,10)
    C2_exp <- rexp(n/2,0.25)
    C2 <- ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
    censor_time <- c(C1,C2)
    delta <- as.numeric(event_time<=censor_time)
    time <- ifelse(delta==1,event_time,censor_time)
    return(data.frame(trt=trt,time=time,delta=delta))
  }
  
  
  # 24% for trt=2 and 59% for trt=1
  Scenario_Null_diff_much<-function(n) 
  {
    trt<-c(rep(1,n/2),rep(2,n/2))
    U1 <- runif(n/2) 
    U2 <- runif(n/2) 
    T1 <- (1/U1)-1
    T2 <- (1/U2)-1
    event_time<-c(T1,T2)
    C1_unif<-runif(n/2,0,10)
    C1_exp<-rexp(n/2,0.85)
    C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
    C2<-runif(n/2,0,10)
    censor_time<-c(C1,C2)
    delta<-as.numeric(event_time<=censor_time)
    time<-ifelse(delta==1,event_time,censor_time)
    return(data.frame(trt=trt,time=time,delta=delta))
  }
  
  scenarios <- ls()
  
  
  other_tests <- function(time,delta,trt)
  {
    
    #Making the trt vector be equal to 0 and 1
    trt_ex <- rep(NA,length(trt))
    trt_unq <- unique(trt)
    trt_ex[trt==trt_unq[1]] <- 0
    trt_ex[trt==trt_unq[2]] <- 1
    trt <- trt_ex
    
    n <- length(time)
    procedure_time <- proc.time()
    
    
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
    
    
    ##################
    ## Uno ##
    ##################
    dat_mat <- data.frame(time=time,status=delta,arm=trt)
    tau <- max(time[delta==1]) # same as definded in Uno example
    a <- AWKMT2(indata = dat_mat,tau = tau, nmethod=10^3, test = "2_side",seed = NULL)
    pvalue_Uno <- list(Pv_Uno_V1_crude = a$crude_pvalue_T1_2_side, Pv_Uno_V2_crude = a$crude_pvalue_T2_2_side,
                    Pv_Uno_V1_bona_fide = a$bona_fide_pvalue_T1_2_side,
                    Pv_Uno_V2_bona_fide = a$bona_fide_pvalue_T2_2_side)
    
    
    
    ##################
    ## Pepe-Fleming ##
    ##################
    
    
    #In order to understand this code one must read pepe fleming description.pdf
    
    n0 <- sum(trt==0)
    n1 <- sum(trt==1)
    p0 <- n0/n
    p1 <- n1/n
    
    
    se<-vector()
    wkm<-vector()
    
    fit0 <- survfit(Surv(time[trt==0], 1-delta[trt==0]) ~ 1)
    fit1 <- survfit(Surv(time[trt==1], 1-delta[trt==1]) ~ 1)
    
    km.c0 <- fit0$surv
    km.c1 <- fit1$surv
    time.0 <- fit0$time
    time.1 <- fit1$time
    
    kmc_1_stepfun <- stepfun(time.1,c(1,km.c1),right = T)
    sc0 <- kmc_1_stepfun(time[trt==0]) #sc2 represents censorship survivor estimate of the obseravtions from group 0 by the KM created on group 1
    kmc1<-c(kmc_1_stepfun(time[trt==1]),sc0) #a vector for the km estimates for censorship (created from group 1) for group 1 and then group 0
    
    
    kmc_0_stepfun <- stepfun(time.0,c(1,km.c0),right = T)
    sc1 <- kmc_0_stepfun(time[trt==1]) #sc1 represents censorship survivor estimate of the obseravtions from group 1 by the KM created on group 0
    kmc0 <- c(sc1,kmc_0_stepfun(time[trt==0])) #a vector for the km estimates for censorship (created from group 0) for group 1 and then group 0
    
    
    dat.ord<-cbind(c(time[trt==1],time[trt==0]),kmc1,kmc0)
    dat.ord<-dat.ord[order(dat.ord[,1]),] #same data.frame as above sorted by time
    
    kmc1.m <- dat.ord[,2] #m stands for minus (from pepe fleming description.pdf)
    kmc0.m <- dat.ord[,3] #m stands for minus (from pepe fleming description.pdf)
    
    w<-kmc1.m*kmc0.m/(p1*kmc1.m+p0*kmc0.m) #weight for the statistic, if the size of both groups is equal p0=p1=0.5
    
    fit0 <- survfit(Surv(time[trt==0], delta[trt==0]) ~ 1)
    fit1 <- survfit(Surv(time[trt==1], delta[trt==1]) ~ 1)
    km.s0 <- fit0$surv
    km.s1 <- fit1$surv
    
    km_1_stepfun <- stepfun(time.1,c(1,km.s1),right = T)
    st0 <- km_1_stepfun(time[trt==0]) #st0 represents the survivor estimate of the obseravtions from group 0 by the KM created on group 1
    kms1 <- c(km_1_stepfun(time[trt==1]),st0) #a vector for the km estimates (created from group 1) for group 1 and then group 0
    
    
    km_0_stepfun <- stepfun(time.0,c(1,km.s0),right = T)
    st1 <- km_0_stepfun(time[trt==1]) #st1 represents the survivor estimate of the obseravtions from group 1 by the KM created on group 0
    kms0 <- c(st1,km_0_stepfun(time[trt==0])) #a vector for the km estimates (created from group 0) for group 1 and then group 0
    
    
    dat.ord <- cbind(c(time[trt==1],time[trt==0]),kms1,kms0)
    dat.ord <- dat.ord[order(dat.ord[,1]),]  #same data.frame as above sorted by time
    diff.t <- diff(c(0,dat.ord[,1]))
    wkm <- sum(w*(dat.ord[,2]-dat.ord[,3])*diff.t) # This expression distributes ~N(0,(n*sigma^2)/4), when n1=n2
    
    fit <- survfit(Surv(time,delta)~1)
    km_all_stepfun <- stepfun(fit$time,c(1,fit$surv),right = F)
    km.all <- km_all_stepfun(dat.ord[,1])
    km.all.m <- c(1,km.all[1:(n-1)])#m stands for minus (from pepe fleming description.pdf)
    diff.km <- -diff(c(1,km.all))
    w[is.na(w)] <- 0
    km.all.m <- c(1,km.all[1:(n-1)])#m stands for minus (from pepe fleming description.pdf)
    temp <- ifelse((w==0)|(km.all==0)|(km.all.m==0),0,diff.km*(rev(cumsum(rev(w*km.all.m*diff.t))))^2/(km.all*km.all.m*w))
    # rev(cumsum(rev())) is for doing integration inside integration
    # diff.km is the width of each part in the integration\sum (the integration is dS(t))
    #in the inside integral it's km.all.m since we stop after last time, and begin in time 0 
    se_pulled <- sqrt(sum(temp,na.rm=TRUE))/sqrt(n0*n1/n)
    
    pvalue_pf_pulled <- 2*(1-pnorm(abs(wkm/se_pulled)))
    
    
    pv_list <- list(Pvalue_logrank = Pvalue_logrank,
                 Pvalue_ppw = Pvalue_ppw, pvalue_pf_pulled = pvalue_pf_pulled,
                 Pv_Uno_V1_crude=pvalue_Uno$Pv_Uno_V1_crude,Pv_Uno_V2_crude=pvalue_Uno$Pv_Uno_V2_crude,
                 Pv_Uno_V1_bona_fide=pvalue_Uno$Pv_Uno_V1_bona_fide,Pv_Uno_V2_bona_fide=pvalue_Uno$Pv_Uno_V2_bona_fide)
    
    run_time <- ((proc.time()-procedure_time)["elapsed"])
    print(paste("running time for n=",n,"is",run_time,"seconds"))
    
    return(pv_list)
  }
  
  
  #This function is only for 1 imputation
  yp_all <- function(time,delta,trt,imputed_altern_delta_vec,imputed_altern_time_vec,ptrt_mat)
  {
    
    procedure_time <- proc.time()
    
    #Making the trt vector be equal to 0 and 1
    trt_ex <- rep(NA,length(trt))
    trt_unq <- unique(trt)
    trt_ex[trt==trt_unq[1]] <- 0
    trt_ex[trt==trt_unq[2]] <- 1
    trt <- trt_ex
    
    n <- length(time)
    
    #First
    dat_mat <- data.frame(V1=time,V2=delta,V3=trt)
    Estimate <- YPmodel.estimate(dat_mat)
    Adlgrk <- YPmodel.adlgrk(dat_mat,Estimate = Estimate)
    Pvalue_yp1 <- as.numeric(Adlgrk[1])
    names(Pvalue_yp1) <- NULL
    
    
    trt2 <- 1-trt
    
    #Second
    dat_mat2 <- data.frame(V1=time,V2=delta,V3=trt2)
    Estimate <- YPmodel.estimate(dat_mat2)
    Adlgrk <- YPmodel.adlgrk(dat_mat2,Estimate = Estimate)
    Pvalue_yp2 <- as.numeric(Adlgrk[1])
    names(Pvalue_yp2) <- NULL
    
    pv_min <- min(c(Pvalue_yp1,Pvalue_yp2)) #The test statistic
    
    pv_min_perm <- c() #The vector for permuted test statistic
    
    n.perm <- ncol(ptrt_mat)
    
    for (p in 1:n.perm)
    {
      
      ptrt <- ptrt_mat[,p]
      time_perm <- ifelse(ptrt==trt,time,imputed_altern_time_vec)
      delta_perm <- ifelse(ptrt==trt,delta,imputed_altern_delta_vec)
      
      #First
      dat_mat <- data.frame(V1=time_perm,V2=delta_perm,V3=ptrt)
      Estimate <- YPmodel.estimate(dat_mat)
      Adlgrk <- YPmodel.adlgrk(dat_mat,Estimate = Estimate)
      Pvalue_yp1_p <- as.numeric(Adlgrk[1])
      
      ptrt2 <- 1-ptrt
      
      #Second
      dat_mat <- data.frame(V1=time_perm,V2=delta_perm,V3=ptrt2)
      Estimate <- YPmodel.estimate(dat_mat)
      Adlgrk <- YPmodel.adlgrk(dat_mat,Estimate = Estimate)
      Pvalue_yp2_p <- as.numeric(Adlgrk[1])
      
      pv_min_perm[p] <- min(c(Pvalue_yp1_p,Pvalue_yp2_p))
    }
    p_min_perm <- sum(pv_min>=pv_min_perm)/(n.perm+1)
    
    pv_list <- list(Pvalue_yp1 = Pvalue_yp1,  Pvalue_yp2 = Pvalue_yp2,
                    Pvalue_yp_perm = p_min_perm)
    
    run_time <- ((proc.time()-procedure_time)["elapsed"])
    print(paste("running time for yp test with n=",n,"is",run_time,"seconds"))
    
    return(pv_list)
  }
  
  
  
  
  
  power_of_test <- function(iterations,imputations,permutations,n,scenario)
  {
    procedure_time <- proc.time()
    nr.sim.cores = detectCores()-1
    cl = makeCluster(nr.sim.cores,outfile="") #open nr.sim.cores instances of R
    registerDoParallel(cl)
    res=foreach(core = 1:nr.sim.cores, .packages = c('KONPsurv',"survival","survAWKMT2","YPmodel","parallel"),
                .export =c(scenarios,"other_tests","yp_all") ,.options.RNG=1234,
                .errorhandling ="pass" ) %dorng% {
                  
                  p_pearson <- c()
                  p_lr <- c()
                  p_cauchy <- c()
                  
                  # p_pearson_first <- c() #meant for the pv obatained from one imputation
                  # p_lr_first <- c()
                  
                  
                  p_logrank <- c()
                  p_yp1 <- c() # yang and prentice with one definition of trt
                  p_yp2 <- c() # yang and prentice with second definition of trt
                  p_yp_perm <- c() # yang and prentice permutation test based on min pv
                  
                  p_ppw <- c() #peto prentice wilcoxon
                  p_pf <- c() # pepe fleming
                  p_Uno_V1_crude <- c()
                  p_Uno_V1_bona_fide <- c()
                  p_Uno_V2_crude <- c()
                  p_Uno_V2_bona_fide <- c()
                  
                  
                  censorship_control<-c()
                  censorship_treatment<-c()
                  
                  tab_usage <- c()
                  tab_usage_perm <- c()
                  
                  
                  for (j in 1:ceiling((iterations)/nr.sim.cores))
                  {
                    data <- (get(scenario))(n)
                    
                    trt<-data$trt
                    time<-data$time
                    delta<-data$delta
                    
                    
                    while(sum(delta[trt==1])<2 | sum(delta[trt==2])<2) #  #making sure we have at least 2 events in each group
                    {
                      data <- (get(scenario))(n)
                      
                      trt<-data$trt
                      time<-data$time
                      delta<-data$delta
                    }
                    
                    censorship_control[j] <- 1-mean(delta[trt==1])
                    censorship_treatment[j] <- 1-mean(delta[trt==2])
                    
                    
                    a <- KONPsurv:::konp_2_sample_impu_output(time = time,status = delta,group = trt,n_perm = permutations,n_impu = imputations)
                    
                    
                    #d <- yp_all(time,delta,trt,a$imputed_altern_delta_mat[,1],
                    #            a$imputed_altern_time_mat[,1],a$ptrt_mat)
                    d <- tryCatch(yp_all(time,delta,trt,a$imputed_altern_status_mat[,1],
                                         a$imputed_altern_time_mat[,1],a$p_group_mat),error=function(e) e) # yp_all gets an error in about 5,000 iterations, this causes the function to not stop if we get and error
                    
                    while ( inherits( d , "error")) # if yp_all had returned an error re-sample the data
                    {
                      data <- (get(scenario))(n)
                      
                      trt<-data$trt
                      time<-data$time
                      delta<-data$delta
                      
                      while(sum(delta[trt==1])<2 | sum(delta[trt==2])<2) #  #making sure we have at least 2 events in each group
                      {
                        data <- (get(scenario))(n)
                        
                        trt<-data$trt
                        time<-data$time
                        delta<-data$delta
                      }
                      a <- KONPsurv:::konp_2_sample_impu_output(time = time,status = delta,group = trt,n_perm = permutations,n_impu = imputations)
                      d <- tryCatch(yp_all(time,delta,trt,a$imputed_altern_staus_mat[,1],
                                           a$imputed_altern_time_mat[,1],a$p_group_mat),error=function(e) e)
                    } # end of checking that yp_all didn't get an error
                    
                    p_pearson[j] <- a$pv_chisq
                    p_lr[j] <- a$pv_lr
                    p_cauchy[j] <- a$pv_cauchy
                    
                    p_yp1[j] <- d$Pvalue_yp1
                    p_yp2[j] <- d$Pvalue_yp2
                    p_yp_perm[j] <- d$Pvalue_yp_perm
                    
                    # p_pearson_first[j] <- a$pv_chisq_first
                    # p_lr_first[j] <- a$pv_lr_first
                    
                    tab_usage[j] <- a$tab_usage
                    tab_usage_perm[j] <- a$tab_usage_perm
                    
                    b <- other_tests(time = time,delta = delta,trt = trt)
                    
                    p_logrank[j] <- b$Pvalue_logrank
                    p_ppw[j] <- b$Pvalue_ppw
                    p_pf[j] <- b$pvalue_pf_pulled
                    p_Uno_V1_crude[j] <- b$Pv_Uno_V1_crude
                    p_Uno_V1_bona_fide[j] <- b$Pv_Uno_V1_bona_fide
                    p_Uno_V2_crude[j] <- b$Pv_Uno_V2_crude
                    p_Uno_V2_bona_fide[j] <- b$Pv_Uno_V2_bona_fide
                    
                  }# end of loop for iteration
                    
                  return(data.frame(p_pearson=p_pearson, p_lr=p_lr,
                                    p_cauchy=p_cauchy,
                                    #p_pearson_first=p_pearson_first,p_lr_first=p_lr_first,
                                    p_logrank=p_logrank, p_yp1=p_yp1, p_yp2=p_yp2,
                                    p_yp_perm=p_yp_perm, p_ppw=p_ppw, p_pf=p_pf,
                                    p_Uno_V1_crude=p_Uno_V1_crude,
                                    p_Uno_V1_bona_fide=p_Uno_V1_bona_fide,
                                    p_Uno_V2_crude=p_Uno_V2_crude,
                                    p_Uno_V2_bona_fide=p_Uno_V2_bona_fide,
                                    censorship_control=censorship_control,
                                    censorship_treatment=censorship_treatment,
                                    tab_usage=tab_usage,tab_usage_perm=tab_usage_perm))
                }#end of paralleling
    stopCluster(cl)
    
    saveRDS(res,"results_list.rds")
    
    res <- do.call("rbind",res) #unites all the lists given from different cores into one dataframe
    
    
    power_vec <- apply(res[,1:(ncol(res)-4)],2,function(x){sum(x<=0.05)/(length(x))})
    res_vec <- c(power_vec,mean(res$censorship_control), mean(res$censorship_treatment),
                 mean(res$tab_usage), mean(res$tab_usage_perm))
    names(res_vec) <- colnames(res)
    res_vec <- c(n,res_vec)
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
  
  #setwd("/home/Malka")
  
  #sink("./running time.txt")
  
  
  for (n in c(100,200,300,400))
  {
    for (scen in scenarios)
    {
      power_of_test(iterations = 2*10^3,imputations = 1,permutations = 10^3,n = n,scenario = scen)
    }
  }