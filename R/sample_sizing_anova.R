sample_sizing_anova=function(method,means=0,var=0,diff=0,a=0,perc=0,alpha,power){

  if (method=="With means"){
    ss_mean_vec=function(means,var,alpha,power){
      n=2:10000
      overall_mean=mean(means)
      tau=means-overall_mean
      a=length(means)
      fi2_n=n*sum(tau^2)/(a*var)
      power_vector=1-pf(qf(1-alpha,a-1,a*(n-1)),a-1,a*(n-1),a*fi2_n)
      n=n[min(which(power_vector>power))]
      return(n)
    }
    return(ss_mean_vec(means,var,alpha,power))}

  if (method=="Max difference"){
    ss_diff=function(diff,a,var,alpha,power){
      n=2:10000
      fi2_n=n*diff^2/(2*a*var)
      power_vector=1-pf(qf(1-alpha,a-1,a*(n-1)),a-1,a*(n-1),a*fi2_n)
      n=n[min(which(power_vector>power))]
      return(n)
    }
    return(ss_diff(diff,a,var,alpha,power))}

  if (method=="Percentage variance"){
    ss_sd_diff=function(perc,a,alpha,power){
      n=2:10000
      fi2_n=(sqrt((1+0.01*perc)^2-1)*sqrt(n))^2
      power_vector=1-pf(qf(1-alpha,a-1,a*(n-1)),a-1,a*(n-1),a*fi2_n)
      n=n[min(which(power_vector>power))]
      return(n)
    }
    return(ss_sd_diff(perc,a,alpha,power))}

}
