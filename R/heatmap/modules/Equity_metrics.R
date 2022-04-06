#convert numbers to ratios
Rate_Calculation<-function(subgroup_n,total_n){
  if(total_n == 0){
    return(0)
  }
  return(subgroup_n/total_n)
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

# Significance test (two sided)
compare_population_proportion_2<-function(Xb,Nb,Xa,Na,conf_val=0.95){
  if (Xa == 0 & Xb == 0){
    return (-9999999)
  }
  else if (Xa == 0){
    return (-Inf)
  }
  else if (Xb == 0){
    return (-8888888)
  }
  
  Xa_not<- Na-Xa
  Xb_not<- Nb-Xb
  if (Xa>=5 & Xa_not>=5 & Xb>=5 & Xb_not>=5){
    p_value<-prop.test(x = c(Xa, Xb), n = c(Na, Nb),
                       alternative = c("two.sided"),
                       conf.level = conf_val,
                       correct = FALSE)$p.value
  }
  else{
    p_value<- -7777777
    #p_value<-fisher.test(dat.xtabs,alternative = c("two.sided"),
    #conf.level = conf_val)$p.value
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
  else if (p_num == -7777777) 
  {
    return ("Sample Size Too Small")
  }
  else{
    return ("Not Significant")
  }
}

# transform the equity value to text
whether_biased_label<-function (value, significance_value,significance_threshold, threshold, neg_break, pos_break){
  if (significance_value>significance_threshold){
    return("Equitable(p)")
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
  else if(value == -7777777){
    return ("Insufficient Data")
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
    return("Equitable")
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
    return ("#000000")
  }
  else if(value == -8888888){
    return ("#54585a")
  }
  else if(value == -7777777){
    return ("#000000")
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
      return ("#00205b")
    }
    else{
      return ("#a5b0cb")
    }
  }
  else{
    return("#d4e6e8")
  }
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

