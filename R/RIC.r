#Includes core functions for RIC analysis


#' Calculates the relevant statistics (p(x), q(x), and AUCi) given a random sample of marker and corresponding treatment benefit.
#' @param xb_data: nX2 matrix with first oclumncolumn being the random draws from marker values and the second being an unbiased estimate of treatment benefit at that marker value
#' @param b_bar: expected benefit of treating all v. treating no one. Should be populated with expected benefit of treatment without testing ONLY when the outcome is a policy-relevant metric that includes consequence of testing, otherwise the sample mean of benefits will be used;
#' @return p(x), q(x), and AUCi
#' @export
ric_empirical<-function(xb_data,b_bar=NULL)
{
  o<-order(xb_data[,1])
  x<-xb_data[o,1]
  b<-xb_data[o,2]
  n<-length(x)

  if(is.null(b_bar)) sum_b<-sum(b) else sum_b<-b_bar*n

  p<-rep(NA,n)
  q<-p

  for(i in 1:n)
  {
    p[n-i+1]<-(n-i+1)/n
    q[n-i+1]<-sum(b[i:n])/sum_b
  }

  auci<-sum(q)/length(p)

  return(list(pq_data=cbind(p=c(0,p),q=c(0,q)),auci=auci))
}







#' Returns p(x) and q(x) desired points and AUCi, and local slope when parametric distribution for the joint distribution of marker value and expected treatment benefit is assumed. Note that this function does not need any data. Equations are provided in Appendix II of the paper.
#' @param p_x: points on the x-axis of ric (p(x)): can be scalar or vector
#' @param mu_x: mean of  marker value
#' @param mu_b: mean of  expected treatment benefit
#' @param sd_x: SD of marker value
#' @param sd_b: SD of  expected treatment benefit
#' @param rho: correlation coefficient between marker and benefit. Note: do not use negative value as the underlying assumption (without loss of generality) is that higher marker value is associated with higher treatment benefit;
#' @param type: normal or lognormal
#' @return p(x), q(x), and AUCi
#' @export
ric_parametric<-function(p_x=NA,mu_x,mu_b,sd_x,sd_b,rho,type)
{
  #Recover marker values associated with x-axis;
  x<-(qnorm(1-p_x)*sd_x+mu_x)

  if(type=="normal")
  {
    q_x<-p_x+1/mu_b*rho*sd_b*dnorm((x-mu_x)/sd_x)
    auci<-1/2+rho*sd_b/2/sqrt(pi)/mu_b
    l_x<-1+rho*sd_b/mu_b*((x-mu_x)/sd_x)
  }

  if(type=="lognormal")
  {
    q_x<-1-pnorm((x-mu_x-rho*sd_b*sd_x)/sd_x)
    auci<-pnorm(rho*sd_b/sqrt(2))
    #l_x<-exp(-(x-mu_x)/sd_x*rho*sd_b-(rho*sd_b)^2/2)
    l_x<-exp(rho*sd_b*((x-mu_x)/sd_x-rho*sd_b/2))
  }

  return(list(x=x,p_x=p_x,q_x=q_x,auci=auci,local_slope=l_x))
}








#' GLM-based RIC estimator. Needs a GLM regression object and data to use for G-computation. See the ric.sample file for examples.
#' @param reg_object: a glm regression object (results of model fitting)
#' @param pred_data: data for G-computation. It must have a marker column named x and a treatment column named tx. Note that if there is variable follow-up time they should all be set to a unique value (e.g., one unit of time) in the prediction dataset to estimate rate
#' @return RIC estimates
#' @export
ric_regression<-function(reg_object,pred_data)
{
  #Containing estimates of expected outcomes (event count) if no one is treated
  data_0<-pred_data
  data_0[,'tx']<-0
  pred_0<-reg_object$family$linkinv(predict.glm(reg_object,newdata=data_0))
  #Containing estimates of expected outcomes (event count) if everyone is treated
  data_1<-data_0
  data_1[,'tx']<-1
  pred_1<-reg_object$family$linkinv(predict.glm(reg_object,newdata=data_1))

  #Expected benefit of treating all
  b_bar<-mean(pred_0-pred_1)

  #the first column is biomarker value(x), the second is treatment benefit (b)
  xb_data<-cbind(x=pred_data[,'x'],b=pred_0-pred_1);

  ric<-ric_empirical(xb_data)

  return(list(b_bar=b_bar,xb_data=xb_data,pq_data=ric$pq_data,auci=ric$auci))
}


#' Calculating AUCi using the method of forced choice as explained in the text (Appendix I); loops over all possible pairs and estimates the expected benefit of treating only the one with higher biomarker value
#' @return the expected benefit of treating with higher biomarker value
#' @export
auci_mfc<-function(xb_data)
{
  n<-dim(xb_data)[1]

  count<-0
  b<-0

  for(i in 1:(n-1))
    for(j in (i+1):n)
    {
      winner<-which.max(c(xb_data[i,1],xb_data[j,1]))
      k<-i+(winner-1)*(j-i)
      b<-b+xb_data[k,2]/2
      count<-count+1
    }
  return(b/count/mean(xb_data[,2]))
}









