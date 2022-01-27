#convert numbers to ratios
Rate_Calculation<-function(subgroup_n,total_n){
  if(total_n == 0){
    return(0)
  }
  return(subgroup_n/total_n)
}
# Equity Metric derived from Equal Opportunity
Adjusted_Equal_Opportunity<-function (alpha_aeo, beta_aeo, for_plot = FALSE){
  if (!for_plot){
    if (beta_aeo == 0 & alpha_aeo == 0){
      return (-9999999)
    }
    else if (beta_aeo == 0){
      return (-Inf)
    }
    else if (alpha_aeo == 0){
      return (-8888888)
    }
    
  }
  top_aeo <- (beta_aeo-alpha_aeo)
  bottom_aeo <- alpha_aeo*(1-alpha_aeo)
  result_aeo <- top_aeo/bottom_aeo
  return (result_aeo)
}
# Equity Metric derived from Disparate Impact
Log_Disparate_Impact<-function (alpha_di, beta_di, for_plot = FALSE){
  if (!for_plot){
    if (beta_di == 0 & alpha_di == 0){
      return (-9999999)
    }
    else if (beta_di == 0){
      return (-Inf)
    }
    else if (alpha_di == 0){
      return (-8888888)
    }
    
  }
  
  left_di <- beta_di/(1-beta_di)
  right_di <- alpha_di/(1-alpha_di)
  result_di <- log(left_di)-log(right_di)
  return (result_di)
}
# Equity Metric derived from observed rate/ ideal rate
Quality_Metric<-function (alpha_di2, beta_di2, for_plot = FALSE){
  if (!for_plot){
    if (beta_di2 == 0 & alpha_di2 == 0){
      return (-9999999)
    }
    else if (beta_di2 == 0){
      return (-Inf)
    }
    else if (alpha_di2 == 0){
      return (-8888888)
    }
    
  }
  return(log(beta_di2/alpha_di2))
}

# Significance test (two sided)
compare_population_proportion<-function(pb,Xa,Na){
  if (Xa == 0 & pb == 0){
    return (-9999999)
  }
  else if (Xa == 0){
    return (-Inf)
  }
  else if (pb == 0){
    return (-8888888)
  }
  
  X_not<- Na-Xa
  if (Xa>=5 & X_not>=5){
    p_value<-prop.test(x = Xa , n = Na, p = pb,
                       alternative = c("two.sided"),
                       conf.level = 0.95,
                       correct = FALSE)$p.value
  }
  else{
    p_value<-binom.test(x=Xa, Na, p = pb,
                        alternative = c("two.sided"),
                        conf.level = 0.95)$p.value
  }
  
  return(p_value)
}

# transform the significance number into text
whether_significant<-function (p_num, threshold){
  if (p_num == -Inf){
    return ("Absent")
  }
  else if(p_num == -9999999){
    return ("No Info")
  }
  else if(p_num == -8888888){
    return ("No Base Data")
  }
  else if (p_num<threshold) 
  {
    return ("Significant")
  }
  else{
    return ("Not Significant")
  }
}

# transform the equity value to text
whether_biased_label<-function (value, significance_value,significance_threshold, threshold, neg_break, pos_break){
  if (significance_value>significance_threshold){
    return("Representative(p)")
  }
  
  if (value == -Inf){
    return ("Absent")
  }
  else if(value == -9999999){
    return ("No Info")
  }
  else if(value == -8888888){
    return ("No Base Data")
  }
  else if (value < -threshold) 
  {
    if (value < -neg_break){
      return ("Highly Underrepresented")
    }
    else{
      return ("Underrepresented")
    }
  }
  else if(value > threshold) {
    if (value > pos_break){
      return ("Highly Overrepresented")
    }
    else{
      return ("Overrepresented")
    }
  }
  else{
    return("Representative")
  }
  
}
# assign the colors to different equity values
add_colors<-function(value, significance_value,significance_threshold, threshold, neg_break, pos_break){
  if (significance_value>significance_threshold){
    return("#d4e6e8")
  }
  
  if (value == -Inf){
    return ("#ab2328")
  }
  else if(value == -9999999){
    return ("#9ea2a2")
  }
  else if(value == -8888888){
    return ("#54585a")
  }
  else if (value < -threshold) 
  {
    if (value < -neg_break){
      return ("#d58570")
    }
    else{
      return ("#eabcad")
    }
  }
  else if(value > threshold) {
    if (value > pos_break){
      return ("#667ba2")
    }
    else{
      return ("#a5b0cb")
    }
  }
  else{
    return("#d4e6e8")
  }
}