

############################################################################################################
##Scenario Null
############################################################################################################


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




############################################################################################################
##Scenario A 
############################################################################################################


Scenario_A_light<-function(n) #  24% for trt=2 and 44% for trt=1
{
  shape<-0.848944
  scale<-10
  trt<-c(rep(1,n/2),rep(2,n/2))
  #Sample from Uniform distribution
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  #Define T_original to be a vector of samples from weibull distribution
  T_original<-vector()
  T_original<-scale*(((-1)*log(U1))^(1/shape))
  #Define T to be a sample from a distribution that differs from the distribution weibull only in the middle
  T1=T_original
  T1[U1<=0.7 & U1>=0.06]<- 32.33472-47.62456*U1[U1<=0.7 & U1>=0.06]+3.944885
  T2<-scale*(((-1)*log(U2))^(1/shape))
  event_time<-c(T1,T2)
  C1 <- rweibull(n/2,18,16)
  C2 <- rweibull(n/2,18,16)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_A_medium<-function(n) #  49% for trt=2 and 60% for trt=1
{
  shape<-0.848944
  scale<-10
  trt<-c(rep(1,n/2),rep(2,n/2))
  #Sample from Uniform distribution
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  #Define T_original to be a vector of samples from weibull distribution
  T_original<-vector()
  T_original<-scale*(((-1)*log(U1))^(1/shape))
  #Define T to be a sample from a distribution that differs from the distribution weibull only in the middle
  T1=T_original
  T1[U1<=0.7 & U1>=0.06]<- 32.33472-47.62456*U1[U1<=0.7 & U1>=0.06]+3.944885
  T2<-scale*(((-1)*log(U2))^(1/shape))
  event_time<-c(T1,T2)
  C1 <- rweibull(n/2,1.5,9)
  C2 <- rweibull(n/2,1.5,9)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_A_diff<-function(n) #  42% for trt=2 and 58% for trt=1
{
  shape<-0.848944
  scale<-10
  trt<-c(rep(1,n/2),rep(2,n/2))
  #Sample from Uniform distribution
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  #Define T_original to be a vector of samples from weibull distribution
  T_original<-vector()
  T_original<-scale*(((-1)*log(U1))^(1/shape))
  #Define T to be a sample from a distribution that differs from the distribution weibull only in the middle
  T1=T_original
  T1[U1<=0.7 & U1>=0.06]<- 32.33472-47.62456*U1[U1<=0.7 & U1>=0.06]+3.944885
  T2<-scale*(((-1)*log(U2))^(1/shape))
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,2,30)
  C1_exp<-rexp(n/2,0.06)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,2,30)
  C2_exp<-rexp(n/2,0.04)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_A_diff_much<-function(n) #  29% for trt=2 and 58% for trt=1
{
  shape<-0.848944
  scale<-10
  trt<-c(rep(1,n/2),rep(2,n/2))
  #Sample from Uniform distribution
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  #Define T_original to be a vector of samples from weibull distribution
  T_original<-vector()
  T_original<-scale*(((-1)*log(U1))^(1/shape))
  #Define T to be a sample from a distribution that differs from the distribution weibull only in the middle
  T1=T_original
  T1[U1<=0.7 & U1>=0.06]<- 32.33472-47.62456*U1[U1<=0.7 & U1>=0.06]+3.944885
  T2<-scale*(((-1)*log(U2))^(1/shape))
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,2,30)
  C1_exp<-rexp(n/2,0.06)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,2,30)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario B 
############################################################################################################


Scenario_B_light <- function(n) # 26% for trt=2 and 26% for trt=1
{
  shape<-0.848944
  scale<-10
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  #Define T_original to be a vector of samples from weibull distribution
  T_original<-vector()
  T_original<-scale*(((-1)*log(U1))^(1/shape))
  #Define T to be a sample from a distribution that differs from the distribution weibull only in the beginning
  T1=vector()
  T1[U1<1 & U1>=0.94]<-50-50*U1[U1<1 & U1>=0.94]
  T1[U1<0.94 & U1>0.4371734]<-12.34716-9.943788*U1[U1<0.94 & U1>0.4371734]
  T1[U1<=0.4371734]<-T_original[U1<=0.4371734]
  T2<-scale*(((-1)*log(U2))^(1/shape))
  event_time <- c(T1,T2)
  C1 <- rweibull(n/2,10,15)
  C2 <- rweibull(n/2,10,15)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}




Scenario_B_medium <- function(n) # 50% for trt=2 and 59% for trt=1
{
  shape<-0.848944
  scale<-10
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  #Define T_original to be a vector of samples from weibull distribution
  T_original<-vector()
  T_original<-scale*(((-1)*log(U1))^(1/shape))
  #Define T to be a sample from a distribution that differs from the distribution weibull only in the beginning
  T1=vector()
  T1[U1<1 & U1>=0.94]<-50-50*U1[U1<1 & U1>=0.94]
  T1[U1<0.94 & U1>0.4371734]<-12.34716-9.943788*U1[U1<0.94 & U1>0.4371734]
  T1[U1<=0.4371734]<-T_original[U1<=0.4371734]
  T2<-scale*(((-1)*log(U2))^(1/shape))
  event_time <- c(T1,T2)
  C1 <- rweibull(n/2,3,7.5)
  C2 <- rweibull(n/2,3,7.5)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}

Scenario_B_diff <- function(n) # 41% for trt=2 and 54% for trt=1
{
  shape<-0.848944
  scale<-10
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  #Define T_original to be a vector of samples from weibull distribution
  T_original<-vector()
  T_original<-scale*(((-1)*log(U1))^(1/shape))
  #Define T to be a sample from a distribution that differs from the distribution weibull only in the beginning
  T1=vector()
  T1[U1<1 & U1>=0.94]<-50-50*U1[U1<1 & U1>=0.94]
  T1[U1<0.94 & U1>0.4371734]<-12.34716-9.943788*U1[U1<0.94 & U1>0.4371734]
  T1[U1<=0.4371734]<-T_original[U1<=0.4371734]
  T2<-scale*(((-1)*log(U2))^(1/shape))
  event_time <- c(T1,T2)
  C1_unif<-runif(n/2,2,30)
  C1_exp<-rexp(n/2,0.06)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,2,30)
  C2_exp<-rexp(n/2,0.04)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}

Scenario_B_diff_much <- function(n) # 29% for trt=2 and 54% for trt=1
{
  shape<-0.848944
  scale<-10
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  #Define T_original to be a vector of samples from weibull distribution
  T_original<-vector()
  T_original<-scale*(((-1)*log(U1))^(1/shape))
  #Define T to be a sample from a distribution that differs from the distribution weibull only in the beginning
  T1=vector()
  T1[U1<1 & U1>=0.94]<-50-50*U1[U1<1 & U1>=0.94]
  T1[U1<0.94 & U1>0.4371734]<-12.34716-9.943788*U1[U1<0.94 & U1>0.4371734]
  T1[U1<=0.4371734]<-T_original[U1<=0.4371734]
  T2<-scale*(((-1)*log(U2))^(1/shape))
  event_time <- c(T1,T2)
  C1_unif<-runif(n/2,2,30)
  C1_exp<-rexp(n/2,0.06)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,2,30)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


############################################################################################################
##Scenario C 
############################################################################################################


Scenario_C_light<-function(n) #  23% for trt=2 and 24% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-2)*log(U1)
  T1[U1<=0.752]=(-2/3)*(log(U1[U1<=0.752])-0.60)
  T1[U1<=0.3329]=(-1)*log(U1[U1<=0.3329])
  T2<-(-2/3)*log(U2)
  T2[U2<=0.4317]=(-4.5)*log(U2[U2<=0.4317]+0.45)
  T2[U2<=0.3329]=(-1)*log(U2[U2<=0.3329])
  event_time<-c(T1,T2)
  C1 <- runif(n/2,1,2)
  C2 <- runif(n/2,1,2)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_C_medium<-function(n) #  45% for trt=2 and 57% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-2)*log(U1)
  T1[U1<=0.752]=(-2/3)*(log(U1[U1<=0.752])-0.60)
  T1[U1<=0.3329]=(-1)*log(U1[U1<=0.3329])
  T2<-(-2/3)*log(U2)
  T2[U2<=0.4317]=(-4.5)*log(U2[U2<=0.4317]+0.45)
  T2[U2<=0.3329]=(-1)*log(U2[U2<=0.3329])
  event_time<-c(T1,T2)
  C1 <- runif(n/2,0,1.6)
  C2 <- runif(n/2,0,1.6)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_C_diff<-function(n) #  48% for trt=2 and 54% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-2)*log(U1)
  T1[U1<=0.752]=(-2/3)*(log(U1[U1<=0.752])-0.60)
  T1[U1<=0.3329]=(-1)*log(U1[U1<=0.3329])
  T2<-(-2/3)*log(U2)
  T2[U2<=0.4317]=(-4.5)*log(U2[U2<=0.4317]+0.45)
  T2[U2<=0.3329]=(-1)*log(U2[U2<=0.3329])
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.01,3)
  C1_exp<-rexp(n/2,0.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0.01,3)
  C2_exp<-rexp(n/2,0.8)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_C_diff_much<-function(n) #  29% for trt=2 and 54% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-2)*log(U1)
  T1[U1<=0.752]=(-2/3)*(log(U1[U1<=0.752])-0.60)
  T1[U1<=0.3329]=(-1)*log(U1[U1<=0.3329])
  T2<-(-2/3)*log(U2)
  T2[U2<=0.4317]=(-4.5)*log(U2[U2<=0.4317]+0.45)
  T2[U2<=0.3329]=(-1)*log(U2[U2<=0.3329])
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.01,3)
  C1_exp<-rexp(n/2,0.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0.01,3)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario D 
############################################################################################################


Scenario_D_light<-function(n) #  24% for trt=2 and 26% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-(-2)*log(U1)
  T1[U1<=0.807]=(-10)*log(U1[U1<=0.807]+0.15)
  T1[U1<=0.75]=(-2/3)*(log(U1[U1<=0.75])-1.25)
  T1[U1<=0.38]=(-1)*log(U1[U1<=0.38])+0.5
  T2<-(-2/3)*log(U2)
  T2[U2<=0.5619]=(-10)*(log(U2[U2<=0.5619]+0.44))+0.4
  T2[U2<=0.5]=(-2)*log(U2[U2<=0.5]+0.1009)
  T2[U2<=0.38]=(-1)*log(U2[U2<=0.38])+0.5
  event_time<-c(T1,T2)
  C1 <- runif(n/2,1.1,3)
  C2 <- runif(n/2,1.1,3)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_D_medium<-function(n) #  45% for trt=2 and 59% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-(-2)*log(U1)
  T1[U1<=0.807]=(-10)*log(U1[U1<=0.807]+0.15)
  T1[U1<=0.75]=(-2/3)*(log(U1[U1<=0.75])-1.25)
  T1[U1<=0.38]=(-1)*log(U1[U1<=0.38])+0.5
  T2<-(-2/3)*log(U2)
  T2[U2<=0.5619]=(-10)*(log(U2[U2<=0.5619]+0.44))+0.4
  T2[U2<=0.5]=(-2)*log(U2[U2<=0.5]+0.1009)
  T2[U2<=0.38]=(-1)*log(U2[U2<=0.38])+0.5
  event_time<-c(T1,T2)
  C1 <- runif(n/2,0.1,2.1)
  C2 <- runif(n/2,0.1,2.1)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_D_diff<-function(n) #  39% for trt=2 and 58% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-(-2)*log(U1)
  T1[U1<=0.807]=(-10)*log(U1[U1<=0.807]+0.15)
  T1[U1<=0.75]=(-2/3)*(log(U1[U1<=0.75])-1.25)
  T1[U1<=0.38]=(-1)*log(U1[U1<=0.38])+0.5
  T2<-(-2/3)*log(U2)
  T2[U2<=0.5619]=(-10)*(log(U2[U2<=0.5619]+0.44))+0.4
  T2[U2<=0.5]=(-2)*log(U2[U2<=0.5]+0.1009)
  T2[U2<=0.38]=(-1)*log(U2[U2<=0.38])+0.5
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.5,3.5)
  C1_exp<-rexp(n/2,0.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0.5,3.5)
  C2_exp<-rexp(n/2,0.3)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_D_diff_much<-function(n) #  27% for trt=2 and 58% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-(-2)*log(U1)
  T1[U1<=0.807]=(-10)*log(U1[U1<=0.807]+0.15)
  T1[U1<=0.75]=(-2/3)*(log(U1[U1<=0.75])-1.25)
  T1[U1<=0.38]=(-1)*log(U1[U1<=0.38])+0.5
  T2<-(-2/3)*log(U2)
  T2[U2<=0.5619]=(-10)*(log(U2[U2<=0.5619]+0.44))+0.4
  T2[U2<=0.5]=(-2)*log(U2[U2<=0.5]+0.1009)
  T2[U2<=0.38]=(-1)*log(U2[U2<=0.38])+0.5
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.5,3.5)
  C1_exp<-rexp(n/2,0.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0.5,3.5)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


############################################################################################################
##Scenario E (from thesis)
############################################################################################################


Scenario_E_light<-function(n) #  16% for trt=2 and 35% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-1)*log(U1)
  T2<-(-1)*log(U2)
  T2[U2<=0.756]=(-1/2)*(log(U2[U2<=0.756])-0.26)
  event_time<-c(T1,T2)
  C1 <- runif(n/2,0.9,1.2)
  C2 <- runif(n/2,0.9,1.2)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_E_medium<-function(n) #  45% for trt=2 and 57% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-1)*log(U1)
  T2<-(-1)*log(U2)
  T2[U2<=0.756]=(-1/2)*(log(U2[U2<=0.756])-0.26)
  event_time<-c(T1,T2)
  C1 <- runif(n/2,0.1,1.1)
  C2 <- runif(n/2,0.1,1.1)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_E_diff<-function(n) #  47% for trt=2 and 52% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-1)*log(U1)
  T2<-(-1)*log(U2)
  T2[U2<=0.756]=(-1/2)*(log(U2[U2<=0.756])-0.26)
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.01,2.3)
  C1_exp<-rexp(n/2,0.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0.01,2.3)
  C2_exp<-rexp(n/2,0.8)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_E_diff_much<-function(n) #  26% for trt=2 and 52% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-1)*log(U1)
  T2<-(-1)*log(U2)
  T2[U2<=0.756]=(-1/2)*(log(U2[U2<=0.756])-0.26)
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.01,2.3)
  C1_exp<-rexp(n/2,0.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0.01,2.3)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario F 
############################################################################################################


Scenario_F_light<-function(n) #  21% for trt=2 and 29% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1<-1
  beta2<--1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1 <- rlnorm(n/2,1.35,0.25)
  C2 <- rlnorm(n/2,1.35,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_F_medium<-function(n) #  50% for trt=2 and 46% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1<-1
  beta2<--1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1 <- rlnorm(n/2,0.01,0.25)
  C2 <- rlnorm(n/2,0.01,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}




Scenario_F_diff<-function(n) #  42% for trt=2 and 52% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1<-1
  beta2<--1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.5,5)
  C1_exp<-rexp(n/2,0.7)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0.5,5)
  C2_exp<-rexp(n/2,0.25)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_F_diff_much<-function(n) #  31% for trt=2 and 52% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1<-1
  beta2<--1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.5,5)
  C1_exp<-rexp(n/2,0.7)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0.5,5)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario G
############################################################################################################


Scenario_G_light<-function(n) #  20% for trt=2 and 31% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1<--1
  beta2<-1
  #First, sample from uni(0,1) distribution
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1 <- rlnorm(n/2,1.4,0.25)
  C2 <- rlnorm(n/2,1.4,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_G_medium<-function(n) #  40% for trt=2 and 60% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1<--1
  beta2<-1
  #First, sample from uni(0,1) distribution
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1 <- rlnorm(n/2,0.4,0.25)
  C2 <- rlnorm(n/2,0.4,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_G_diff<-function(n) #  46% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1<--1
  beta2<-1
  #First, sample from uni(0,1) distribution
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.5,7)
  C1_exp<-rexp(n/2,0.2)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0.5,7)
  C2_exp<-rexp(n/2,0.4)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_G_diff_much<-function(n) #  26% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1<--1
  beta2<-1
  #First, sample from uni(0,1) distribution
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0.5,7)
  C1_exp<-rexp(n/2,0.2)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0.5,7)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario H 
############################################################################################################

Scenario_H_light<-function(n) #  23% for trt=2 and 29% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  alpha<-1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  S_c = -log(U_c)
  S_t <- vector()
  lim1 <- exp(-((1+alpha)/alpha)*0.5)
  lim2 <- exp(-(((1+alpha)/alpha)*0.5+(alpha/(1+alpha))*(1.5-0.5)))
  S_t[U_t>lim1] = -(alpha/(1+alpha))*log(U_t[U_t>lim1])
  S_t[U_t<=lim1 & U_t>=lim2]  = (-((1+alpha)/alpha))*((((1+alpha)/alpha)-(alpha/(1+alpha)))*0.5+log(U_t[U_t<=lim1 & U_t>=lim2]))
  S_t[U_t<lim2] = (-alpha/(1+alpha))*(log(U_t[U_t<lim2])+alpha/(1+alpha)-(1+alpha)/alpha)
  #observed times
  T1 <- S_c
  T2 <- S_t
  event_time<-c(T1,T2)
  C1 <- rlnorm(n/2,0.25,0.25)
  C2 <- rlnorm(n/2,0.25,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_H_medium<-function(n) #  40% for trt=2 and 60% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  alpha<-1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  S_c = -log(U_c)
  S_t <- vector()
  lim1 <- exp(-((1+alpha)/alpha)*0.5)
  lim2 <- exp(-(((1+alpha)/alpha)*0.5+(alpha/(1+alpha))*(1.5-0.5)))
  S_t[U_t>lim1] = -(alpha/(1+alpha))*log(U_t[U_t>lim1])
  S_t[U_t<=lim1 & U_t>=lim2]  = (-((1+alpha)/alpha))*((((1+alpha)/alpha)-(alpha/(1+alpha)))*0.5+log(U_t[U_t<=lim1 & U_t>=lim2]))
  S_t[U_t<lim2] = (-alpha/(1+alpha))*(log(U_t[U_t<lim2])+alpha/(1+alpha)-(1+alpha)/alpha)
  #observed times
  T1 <- S_c
  T2 <- S_t
  event_time<-c(T1,T2)
  C1 <- rlnorm(n/2,0.-0.7,0.25)
  C2 <- rlnorm(n/2,-0.7,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_H_diff<-function(n) #  35% for trt=2 and 51% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  alpha<-1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  S_c = -log(U_c)
  S_t <- vector()
  lim1 <- exp(-((1+alpha)/alpha)*0.5)
  lim2 <- exp(-(((1+alpha)/alpha)*0.5+(alpha/(1+alpha))*(1.5-0.5)))
  S_t[U_t>lim1] = -(alpha/(1+alpha))*log(U_t[U_t>lim1])
  S_t[U_t<=lim1 & U_t>=lim2]  = (-((1+alpha)/alpha))*((((1+alpha)/alpha)-(alpha/(1+alpha)))*0.5+log(U_t[U_t<=lim1 & U_t>=lim2]))
  S_t[U_t<lim2] = (-alpha/(1+alpha))*(log(U_t[U_t<lim2])+alpha/(1+alpha)-(1+alpha)/alpha)
  #observed times
  T1 <- S_c
  T2 <- S_t
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,2.5)
  C1_exp<-rexp(n/2,0.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,2.5)
  C2_exp<-rexp(n/2,0.25)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_H_diff_much<-function(n) #  28% for trt=2 and 51% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  alpha<-1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  S_c = -log(U_c)
  S_t <- vector()
  lim1 <- exp(-((1+alpha)/alpha)*0.5)
  lim2 <- exp(-(((1+alpha)/alpha)*0.5+(alpha/(1+alpha))*(1.5-0.5)))
  S_t[U_t>lim1] = -(alpha/(1+alpha))*log(U_t[U_t>lim1])
  S_t[U_t<=lim1 & U_t>=lim2]  = (-((1+alpha)/alpha))*((((1+alpha)/alpha)-(alpha/(1+alpha)))*0.5+log(U_t[U_t<=lim1 & U_t>=lim2]))
  S_t[U_t<lim2] = (-alpha/(1+alpha))*(log(U_t[U_t<lim2])+alpha/(1+alpha)-(1+alpha)/alpha)
  #observed times
  T1 <- S_c
  T2 <- S_t
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,2.5)
  C1_exp<-rexp(n/2,0.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,2.5)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}




############################################################################################################
## Scenario I-1
############################################################################################################


Scenario_I_1_light<-function(n) #  25% for trt=2 and 24% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  theta1 <- 2
  theta2 <- 1/2
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1) 
  T2 <- log((theta2/theta1)*U2^(-1/theta2)-theta2/theta1 +1)
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.32)
  C2 <- rexp(n/2,0.32)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_I_1_medium<-function(n) #  47% for trt=2 and 50% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  theta1 <- 2
  theta2 <- 1/2
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1) 
  T2 <- log((theta2/theta1)*U2^(-1/theta2)-theta2/theta1 +1)
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,1)
  C2 <- rexp(n/2,1)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_I_1_diff<-function(n) #  40% for trt=2 and 52% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  theta1 <- 2
  theta2 <- 1/2
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1) 
  T2 <- log((theta2/theta1)*U2^(-1/theta2)-theta2/theta1 +1)
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4)
  C1_exp<-rexp(n/2,0.8)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,4)
  C2_exp<-rexp(n/2,0.4)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_I_1_diff_much<-function(n) #  27% for trt=2 and 52% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  theta1 <- 2
  theta2 <- 1/2
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1) 
  T2 <- log((theta2/theta1)*U2^(-1/theta2)-theta2/theta1 +1)
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4)
  C1_exp<-rexp(n/2,0.8)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,4)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}




############################################################################################################
## Scenario I-2
############################################################################################################


Scenario_I_2_light<-function(n) #  27% for trt=2 and 23% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  theta1 <- 1/2
  theta2 <- 2
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1) 
  T2 <- log((theta2/theta1)*U2^(-1/theta2)-theta2/theta1 +1)
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.3)
  C2 <- rexp(n/2,0.3)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_I_2_medium<-function(n) #  53% for trt=2 and 46% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  theta1 <- 1/2
  theta2 <- 2
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1) 
  T2 <- log((theta2/theta1)*U2^(-1/theta2)-theta2/theta1 +1)
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.85)
  C2 <- rexp(n/2,0.85)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_I_2_diff<-function(n) #  42% for trt=2 and 52% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  theta1 <- 1/2
  theta2 <- 2
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1) 
  T2 <- log((theta2/theta1)*U2^(-1/theta2)-theta2/theta1 +1)
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4)
  C1_exp<-rexp(n/2,0.8)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,4)
  C2_exp<-rexp(n/2,0.25)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_I_2_diff_much<-function(n) #  28% for trt=2 and 52% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  theta1 <- 1/2
  theta2 <- 2
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1) 
  T2 <- log((theta2/theta1)*U2^(-1/theta2)-theta2/theta1 +1)
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4)
  C1_exp<-rexp(n/2,0.8)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,4)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
## Scenario I-3
############################################################################################################


Scenario_I_3_light<-function(n) #  24% for trt=2 and 25% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- log(1/2)
  beta2 <- log(5)
  #First, sample from uni(0,1) distribution
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment # Log logistic with (1,1)
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1 <- rlnorm(n/2,1.2,0.25)
  C2 <- rlnorm(n/2,1.2,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_I_3_medium<-function(n) #  44% for trt=2 and 54% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- log(1/2)
  beta2 <- log(5)
  #First, sample from uni(0,1) distribution
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment # Log logistic with (1,1)
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1 <- rlnorm(n/2,0.25,0.25)
  C2 <- rlnorm(n/2,0.25,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_I_3_diff<-function(n) #  40% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- log(1/2)
  beta2 <- log(5)
  #First, sample from uni(0,1) distribution
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment # Log logistic with (1,1)
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,10)
  C1_exp<-rexp(n/2,0.4)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,10)
  C2_exp<-rexp(n/2,0.25)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_I_3_diff_much<-function(n) #  24% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- log(1/2)
  beta2 <- log(5)
  #First, sample from uni(0,1) distribution
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment # Log logistic with (1,1)
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,10)
  C1_exp<-rexp(n/2,0.4)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,10)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario J_1
############################################################################################################

Scenario_J_1_light<-function(n) #  24% for trt=2 and 23% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rexp(n/2,2) 
  T2[T2>0.25] <- 0.25 + rexp(sum(T2>0.25),0.6) 
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.3)
  C2 <- rexp(n/2,0.3)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_J_1_medium<-function(n) #  47% for trt=2 and 50% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rexp(n/2,2) 
  T2[T2>0.25] <- 0.25 + rexp(sum(T2>0.25),0.6) 
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,1)
  C2 <- rexp(n/2,1)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_J_1_diff<-function(n) #  43% for trt=2 and 54% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rexp(n/2,2) 
  T2[T2>0.25] <- 0.25 + rexp(sum(T2>0.25),0.6) 
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,4)
  C2_exp<-rexp(n/2,0.5)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_J_1_diff_much<-function(n) #  27% for trt=2 and 54% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rexp(n/2,2) 
  T2[T2>0.25] <- 0.25 + rexp(sum(T2>0.25),0.6) 
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,4)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario J_2
############################################################################################################

Scenario_J_2_light<-function(n) #  26% for trt=2 and 23% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rexp(n/2,1) 
  T2[T2>0.1] <- 0.1 + rexp(sum(T2>0.1),1.7) 
  T2[T2>0.45] <- 0.45 + rexp(sum(T2>0.45),0.5) 
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.3)
  C2 <- rexp(n/2,0.3)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_J_2_medium<-function(n) #  49% for trt=2 and 50% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rexp(n/2,1) 
  T2[T2>0.1] <- 0.1 + rexp(sum(T2>0.1),1.7) 
  T2[T2>0.45] <- 0.45 + rexp(sum(T2>0.45),0.5) 
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,1)
  C2 <- rexp(n/2,1)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_J_2_diff<-function(n) #  45% for trt=2 and 54% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rexp(n/2,1) 
  T2[T2>0.1] <- 0.1 + rexp(sum(T2>0.1),1.7) 
  T2[T2>0.45] <- 0.45 + rexp(sum(T2>0.45),0.5) 
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,4)
  C2_exp<-rexp(n/2,0.5)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_J_2_diff_much<-function(n) #  29% for trt=2 and 54% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rexp(n/2,1) 
  T2[T2>0.1] <- 0.1 + rexp(sum(T2>0.1),1.7) 
  T2[T2>0.45] <- 0.45 + rexp(sum(T2>0.45),0.5) 
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,4)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario J_3
############################################################################################################

Scenario_J_3_light<-function(n) #  29% for trt=2 and 20% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1)
  T2[U2>exp(-1/2)] <- 1-sqrt(1+2*log(U2[U2>exp(-1/2)]))
  T2[U2<exp(-1/2)] <- 1+sqrt(-1-2*log(U2[U2<exp(-1/2)]))
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.25)
  C2 <- rexp(n/2,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_J_3_medium<-function(n) #  56% for trt=2 and 43% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1)
  T2[U2>exp(-1/2)] <- 1-sqrt(1+2*log(U2[U2>exp(-1/2)]))
  T2[U2<exp(-1/2)] <- 1+sqrt(-1-2*log(U2[U2<exp(-1/2)]))
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.75)
  C2 <- rexp(n/2,0.75)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_J_3_diff<-function(n) #  48% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1)
  T2[U2>exp(-1/2)] <- 1-sqrt(1+2*log(U2[U2>exp(-1/2)]))
  T2[U2<exp(-1/2)] <- 1+sqrt(-1-2*log(U2[U2<exp(-1/2)]))
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,5)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,5)
  C2_exp<-rexp(n/2,0.3)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_J_3_diff_much<-function(n) #  30% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  T2 <- rep(NA,n/2)
  U2 <- runif(n/2,0,1)
  T2[U2>exp(-1/2)] <- 1-sqrt(1+2*log(U2[U2>exp(-1/2)]))
  T2[U2<exp(-1/2)] <- 1+sqrt(-1-2*log(U2[U2<exp(-1/2)]))
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,5)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,5)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario K_1
############################################################################################################

Scenario_K_1_light<-function(n) #  28% for trt=2 and 23% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  U2 <- runif(n/2,0,1)
  T2 <- log((U2^(-3.4))/4+3/4)/1.7
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.3)
  C2 <- rexp(n/2,0.3)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_K_1_medium<-function(n) #  50% for trt=2 and 47% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  U2 <- runif(n/2,0,1)
  T2 <- log((U2^(-3.4))/4+3/4)/1.7
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.9)
  C2 <- rexp(n/2,0.9)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_K_1_diff<-function(n) #  39% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  U2 <- runif(n/2,0,1)
  T2 <- log((U2^(-3.4))/4+3/4)/1.7
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,5)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,5)
  C2_exp<-rexp(n/2,0.3)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_K_1_diff_much<-function(n) #  26% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  U2 <- runif(n/2,0,1)
  T2 <- log((U2^(-3.4))/4+3/4)/1.7
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,5)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,5)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario K_2
############################################################################################################

Scenario_K_2_light<-function(n) #  25% for trt=2 and 23% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  U2 <- runif(n/2,0,1)
  T2 <- log((U2^(-4))/8+7/8)/2
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.3)
  C2 <- rexp(n/2,0.3)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_K_2_medium<-function(n) #  47% for trt=2 and 50% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  U2 <- runif(n/2,0,1)
  T2 <- log((U2^(-4))/8+7/8)/2
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,1)
  C2 <- rexp(n/2,1)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_K_2_diff<-function(n) #  39% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  U2 <- runif(n/2,0,1)
  T2 <- log((U2^(-4))/8+7/8)/2
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,5)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,5)
  C2_exp<-rexp(n/2,0.45)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_K_2_diff_much<-function(n) #  24% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,1)
  U2 <- runif(n/2,0,1)
  T2 <- log((U2^(-4))/8+7/8)/2
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,5)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,5)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario K_3
############################################################################################################

Scenario_K_3_light<-function(n) #  26% for trt=2 and 25% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,0.4)
  U2 <- runif(n/2,0,1)
  T2 <- log(33*U2^(-2/5)-32)
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.13)
  C2 <- rexp(n/2,0.13)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_K_3_medium<-function(n) #  54% for trt=2 and 47% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,0.4)
  U2 <- runif(n/2,0,1)
  T2 <- log(33*U2^(-2/5)-32)
  event_time<-c(T1,T2)
  C1 <- rexp(n/2,0.35)
  C2 <- rexp(n/2,0.35)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_K_3_diff<-function(n) #  45% for trt=2 and 51% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,0.4)
  U2 <- runif(n/2,0,1)
  T2 <- log(33*U2^(-2/5)-32)
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,10)
  C1_exp<-rexp(n/2,0.3)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,10)
  C2_exp<-rexp(n/2,0.15)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_K_3_diff_much<-function(n) #  24% for trt=2 and 51% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T1 <- rexp(n/2,0.4)
  U2 <- runif(n/2,0,1)
  T2 <- log(33*U2^(-2/5)-32)
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,10)
  C1_exp<-rexp(n/2,0.3)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,10)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


############################################################################################################
##Scenario L
############################################################################################################



Scenario_L_light<-function(n) #  15% for trt=2 and 35% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T2<-rweibull(n/2,0.848944,10) #control
  T1<-rweibull(n/2,0.848944,20) # treatment
  event_time<-c(T1,T2)
  C1<-rweibull(n/2,5,24)
  C2<-rweibull(n/2,5,24)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_L_medium<-function(n) #  41% for trt=2 and 59% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T2<-rweibull(n/2,0.848944,10) #control
  T1<-rweibull(n/2,0.848944,20) # treatment
  event_time<-c(T1,T2)
  C1<-rweibull(n/2,1.5,12)
  C2<-rweibull(n/2,1.5,12)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_L_diff<-function(n) #  44% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T2<-rweibull(n/2,0.848944,10) #control
  T1<-rweibull(n/2,0.848944,20) # treatment
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,40)
  C1_exp<-rexp(n/2,0.025)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,40)
  C2_exp<-rexp(n/2,0.05)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_L_diff_much<-function(n) #  26% for trt=2 and 53% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  T2<-rweibull(n/2,0.848944,10) #control
  T1<-rweibull(n/2,0.848944,20) # treatment
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,40)
  C1_exp<-rexp(n/2,0.025)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,40)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


############################################################################################################
##Scenario M
############################################################################################################



Scenario_M_light<-function(n) #  23% for trt=2 and 28% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-4)*log(U1)
  T1[U1<=0.8825]=2*(((-1)*log(U1[U1<=0.8825]))^(2/3))
  T2<-(-2.2)*log(U2)
  T2[U2<=0.727]=1.5*(((-1)*log(U2[U2<=0.727]))^(2/3))
  event_time<-c(T1,T2)
  C1<-rweibull(n/2,0.9,5.5)
  C2<-rweibull(n/2,0.9,5.5)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_M_medium<-function(n) #  48% for trt=2 and 52% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-4)*log(U1)
  T1[U1<=0.8825]=2*(((-1)*log(U1[U1<=0.8825]))^(2/3))
  T2<-(-2.2)*log(U2)
  T2[U2<=0.727]=1.5*(((-1)*log(U2[U2<=0.727]))^(2/3))
  event_time<-c(T1,T2)
  C1<-rweibull(n/2,0.35,3.4)
  C2<-rweibull(n/2,0.35,3.4)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}




Scenario_M_diff<-function(n) #  39% for trt=2 and 55% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-4)*log(U1)
  T1[U1<=0.8825]=2*(((-1)*log(U1[U1<=0.8825]))^(2/3))
  T2<-(-2.2)*log(U2)
  T2[U2<=0.727]=1.5*(((-1)*log(U2[U2<=0.727]))^(2/3))
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4.5)
  C1_exp<-rexp(n/2,0.25)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,4.5)
  C2_exp<-rexp(n/2,0.14)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_M_diff_much<-function(n) #  29% for trt=2 and 55% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  U1=runif(n/2,0,1)
  U2=runif(n/2,0,1)
  U1=sort(U1)
  U2=sort(U2)
  T1<-vector()
  T1<-(-4)*log(U1)
  T1[U1<=0.8825]=2*(((-1)*log(U1[U1<=0.8825]))^(2/3))
  T2<-(-2.2)*log(U2)
  T2[U2<=0.727]=1.5*(((-1)*log(U2[U2<=0.727]))^(2/3))
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,4.5)
  C1_exp<-rexp(n/2,0.25)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,4.5)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


############################################################################################################
##Scenario N
############################################################################################################


Scenario_N_light<-function(n) #  32% for trt=2 and 16% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 0.5
  beta2 <- 0.5
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1<-rlnorm(n/2,0.75,0.25)
  C2<-rlnorm(n/2,0.75,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_N_medium<-function(n) #  53% for trt=2 and 35% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 0.5
  beta2 <- 0.5
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1<-rlnorm(n/2,-0.1,0.25)
  C2<-rlnorm(n/2,-0.1,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_N_diff<-function(n) #  46% for trt=2 and 56% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 0.5
  beta2 <- 0.5
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,12)
  C1_exp<-rexp(n/2,1.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,12)
  C2_exp<-rexp(n/2,0.4)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_N_diff_much<-function(n) #  21% for trt=2 and 56% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 0.5
  beta2 <- 0.5
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,12)
  C1_exp<-rexp(n/2,1.5)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,12)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario O
############################################################################################################



Scenario_O_light<-function(n) #  40% for trt=2 and 8% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 1
  beta2 <- 1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1<-rlnorm(n/2,0.4,0.25)
  C2<-rlnorm(n/2,0.4,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_O_medium<-function(n) #  64% for trt=2 and 31% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 1
  beta2 <- 1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1<-rlnorm(n/2,-0.6,0.25)
  C2<-rlnorm(n/2,-0.6,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_O_diff<-function(n) #  51% for trt=2 and 49% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 1
  beta2 <- 1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,8)
  C1_exp<-rexp(n/2,2)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,8)
  C2_exp<-rexp(n/2,0.5)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_O_diff_much<-function(n) #  27% for trt=2 and 49% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 1
  beta2 <- 1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,8)
  C1_exp<-rexp(n/2,2)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,8)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario P 
############################################################################################################

Scenario_P_light<-function(n) #  17% for trt=2 and 36% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 1
  beta2 <- 0
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1<-rlnorm(n/2,0.6,0.25)
  C2<-rlnorm(n/2,0.6,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_P_medium<-function(n) #  62% for trt=2 and 38% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 1
  beta2 <- 0
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1<-rlnorm(n/2,-0.5,0.25)
  C2<-rlnorm(n/2,-0.5,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_P_diff<-function(n) #  51% for trt=2 and 45% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 1
  beta2 <- 0
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,8)
  C1_exp<-rexp(n/2,1.2)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,8)
  C2_exp<-rexp(n/2,0.5)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_P_diff_much<-function(n) #  27% for trt=2 and 55% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 1
  beta2 <- 0
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,8)
  C1_exp<-rexp(n/2,2)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,8)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



############################################################################################################
##Scenario Q 
############################################################################################################

Scenario_Q_light<-function(n) #  34% for trt=2 and 24% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 0
  beta2 <- 1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1<-rlnorm(n/2,0.7,0.25)
  C2<-rlnorm(n/2,0.7,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_Q_medium<-function(n) #  54% for trt=2 and 47% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 0
  beta2 <- 1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1<-rlnorm(n/2,-0.15,0.25)
  C2<-rlnorm(n/2,-0.15,0.25)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}



Scenario_Q_diff<-function(n) #  44% for trt=2 and 55% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 0
  beta2 <- 1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,8)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2_unif<-runif(n/2,0,8)
  C2_exp<-rexp(n/2,0.3)
  C2<-ifelse(C2_exp<C2_unif,C2_exp,C2_unif) 
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}


Scenario_Q_diff_much<-function(n) #  27% for trt=2 and 55% for trt=1
{
  trt<-c(rep(1,n/2),rep(2,n/2))
  beta1 <- 0
  beta2 <- 1
  U_t <- sort(runif(n/2))
  U_c <- sort(runif(n/2))
  #Now transform from uniform distribution to the new distribution
  T2 <- (1/U_t)-1  #Treatment
  T1 <- (exp(beta2)/exp(beta1))*((U_c^(1/-exp(beta2)))-1)  #Control
  event_time<-c(T1,T2)
  C1_unif<-runif(n/2,0,8)
  C1_exp<-rexp(n/2,0.9)
  C1<-ifelse(C1_exp<C1_unif,C1_exp,C1_unif)
  C2<-runif(n/2,0,8)
  censor_time<-c(C1,C2)
  delta<-as.numeric(event_time<=censor_time)
  time<-ifelse(delta==1,event_time,censor_time)
  return(data.frame(trt=trt,time=time,delta=delta))
}
