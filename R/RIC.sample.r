#NOTE: load ric.r functions first;

#MASS is required for multivariate normal random variable generation
library("MASS")

if(!exists("ric_regression"))
  stop("You need to load ric.r functions first")


#Generates a sample dataset with follow-up time, number of events, treatment assignment, and three covariates
generate_data<-function(n_data=1000, ln_rate_coeffs=c(intercept=-0.5,tx=log(1/3),c1=log(1),c2=log(2),c3=log(3)),rate_v=1, cov_mu=c(0,0,0), cov_sigma=rbind(c(1,0,0),c(0,1,0),c(0,0,1)))
{
  
  #Generate covariates - we are generating one binary variable for treatment, and one binary variable and two continuous variables as covariates
  tx<-(runif(n_data)>0.5)*1
  
  #Covariates can be correlated see cov_sigma input parameter
  temp<-mvrnorm(n=n_data, mu=cov_mu, Sigma=cov_sigma)
  
  temp[,1]<-(temp[,1]>0.5)*1  #changing the first covariate to binary
    
  #Calculating the actual rate of event for each person
  rate_mu<-exp(ln_rate_coeffs['intercept']
            +ln_rate_coeffs['tx']*tx
            +ln_rate_coeffs['c1']*temp[,1]
            +ln_rate_coeffs['c2']*temp[,2]
            +ln_rate_coeffs['c3']*temp[,3]
         )
  
  #Heterogeneity in rate is modelled with a gamma distribution, giving rise to negative binomial regression
  rates<-rgamma(n_data, rate=rate_mu/rate_v, shape=rate_mu*rate_mu/rate_v)
  
  #Follow-up time is cut at one year
  times<-exp(rnorm(n_data,log(1),0.1))
  times[which(times>1)]<-1
  
  events<-rpois(n_data,rates*times)
  
  out_data<-as.data.frame(cbind(ln_time=log(times),events=events,tx=tx,c1=temp[,1],c2=temp[,2],c3=temp[,3]))
  
  return(out_data)
}


#Generates the risk score (as an example of a marker)
generate_marker<-function(formula=events~tx+c1+c2+c3+offset(ln_time),reg_data)
{
  reg<-glm.nb(formula=formula,data=reg_data,link=log)

  #Risk score is the predicted values from the regression model with tx=0 and follow-up time set to 1
  new_data<-as.data.frame(reg_data)
  new_data[,'tx']<-0
  new_data[,'time']<-1
  new_data[,'ln_time']<-0
  return(predict.glm(reg,newdata=new_data))
}







#Is called by the main code (below) to calculate RIC metrics using different methods
ric=function(marker_formula=events~tx+c1+c2+c3+offset(ln_time),q_formula=events~tx+c1+c2+c3+offset(ln_time),sample_size=1000)
{
  message("lenegd:\n dark line: empirical RIC\n grey line: parameteric approximation of RIC\n emp: empirical \n mfc: method of forced choice: simulating pairs of subjects and a=giving treatment to the one with higher marker value, b=giving both treatment, c) calculating the average benefit of a over b.\n parm: parametric approximation)")
  reg_data<-generate_data(sample_size)
  reg_data[,'x']<-generate_marker(formula=marker_formula,reg_data)
  pred_data<-reg_data
  pred_data[,'ln_time']<-0
  #G-computation
  reg_object<-glm.nb(data=reg_data,formula=q_formula,link=log)
  res<-ric_regression(reg_object,pred_data)
  plot(res$pq[,1],res$pq[,2],type='l',xlab="Proportion treated",ylab="Relative benefit",xlim=c(0,1),ylim=c(0,1))
  text(0.7,0.3,paste("AUCi (emp):",round(res$auci,3)))
  #Parametric RIC (Appendix II) based on estimated mean and covariance matrix of (x,b).
  xb_data<-res$xb_data
  xb_data[,2]<-log(xb_data[,2])
  temp<-ric_parametric(p_x=(0:100)/100,mu_x =mean(xb_data[,1]),sd_x = sd(xb_data[,1]), mu_b = mean(xb_data[,2]), sd_b = sd(xb_data[,2]),rho = cor(xb_data)[1,2],type = "lognormal")
  lines(temp$p_x,temp$q_x,lty=6,col='grey')
  text(0.7,0.2,paste("AUCi (parm):",round(temp$auci,3)))
  #Simulating the method of forced choice: creating pairs of subjects and a=giving treatment to the one with higher marker value, b=giving both treatment, c) calculating the average benefit of a over b
  #see auci_mfc() for details
  text(0.7,0.1,paste("AUCi (mfc):",round(auci_mfc(res$xb),3)))
}




#Main code



#Complete score (includes all covariates)
x11()
ric(marker_formula=events~tx+c1+c2+c3+offset(ln_time))
title("Figure 1")

#Partial score with C2 removed - note that RIC becomes jagged 
x11()
ric(marker_formula=events~tx+c1+c3+offset(ln_time))
title("Figure 2")


#Non-informative marker (c1 is not associated with the outcome) - note that RIC becomes jagged 
x11()
ric(marker_formula=events~tx+c1+offset(ln_time))
title("Figure 3")







ric_parametric_Validate<-function(mu_x = 0,mu_b = 1,sd_x = 1,sd_b = 1,rho = 0.5)
{
  xb_data<-mvrnorm(n=1000, mu=c(mu_x,mu_b), Sigma=rbind(c(sd_x^2,rho*sd_x*sd_b),c(rho*sd_x*sd_b,sd_b^2)))
  o1<-ric_empirical(xb_data)
  plot(o1$pq_data)
  o2<-ric_parametric(p_x=(0:1000)/1000,mu_x,mu_b,sd_x,sd_b,rho,type = "normal")
  lines(o2$p_x,o2$q_x,type="l",col="red")
  
  x<-o2$local_slope
  y<-(o2$q_x[-1]-o2$q_x[-length(o2$q_x)])/(o2$p_x[-1]-o2$p_x[-length(o2$p_x)])
  x<-x[-which(abs(x)==Inf)]
  y<-y[1:length(x)]
  plot(x,y)
  lines(c(-1000,1000),c(-1000,1000),col="red",type="l")
}
