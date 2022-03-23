make_table_combine<-function(background_info,RCT_info,background_info_lab,x, sig_t, cutoff1,cutoff2, selected_metric) {
  user_colnames<-colnames(RCT_info)
  
  if (("TC" %in% user_colnames)|("FPG" %in% user_colnames)){
    nonlab_factors<- user_colnames[!user_colnames%in% c("TC", "FPG","user_n")]
    lab_factors<-c("TC","FPG")
  }
  else{
    nonlab_factors<- user_colnames[!user_colnames%in% c("user_n")]
    lab_factors<-c()
  }
  
  
  num_nonlab<- length(nonlab_factors)
  num_lab<-length(lab_factors)
  
  background_info_df<-background_info %>%
    group_by(.dots = nonlab_factors[1]) %>% 
    summarise(background_n = sum(background_n))
  names(background_info_df)[1] <- "Group_Name"
  background_info_df<-na.omit(background_info_df)
  total_population_background<-sum(as.integer(background_info_df$background_n))
  background_info_df$total_background<-rep(total_population_background, nrow(background_info_df))

  user_info_df<-RCT_info%>%
    group_by(.dots = nonlab_factors[1]) %>% 
    summarise(user_n = sum(user_n))
  names(user_info_df)[1] <- "Group_Name"
  user_info_df<-na.omit(user_info_df)
  total_population_user<-sum(as.integer(user_info_df$user_n))
  user_info_df$total_user<-rep(total_population_user, nrow(user_info_df))

  for (i in 2:num_nonlab){
    background_info_df_var<-background_info %>%
      group_by(.dots = nonlab_factors[i]) %>% 
      summarise(background_n = sum(background_n))
    names(background_info_df_var)[1] <- "Group_Name"
    background_info_df_var<-na.omit(background_info_df_var)
    total_population_background_var<-sum(as.integer(background_info_df_var$background_n))
    background_info_df_var$total_background<-rep(total_population_background_var, nrow(background_info_df_var))
    
    user_info_df_var<-RCT_info%>%
      group_by(.dots = nonlab_factors[i]) %>% 
      summarise(user_n = sum(user_n))
    names(user_info_df_var)[1] <- "Group_Name"
    user_info_df_var<-na.omit(user_info_df_var)
    total_population_user_var<-sum(as.integer(user_info_df_var$user_n))
    user_info_df_var$total_user<-rep(total_population_user, nrow(user_info_df_var))
    background_info_df<-plyr::rbind.fill(background_info_df, background_info_df_var) 
    user_info_df<-plyr::rbind.fill(user_info_df, user_info_df_var) 
  }
  
  for (ii in 1:num_lab){
    background_info_df_var<-background_info_lab %>%
      group_by(.dots = lab_factors[ii]) %>% 
      summarise(background_n = sum(background_n))
    names(background_info_df_var)[1] <- "Group_Name"
    background_info_df_var<-na.omit(background_info_df_var)
    total_population_background_var<-sum(as.integer(background_info_df_var$background_n))
    background_info_df_var$total_background<-rep(total_population_background_var, nrow(background_info_df_var))
    
    user_info_df_var<-RCT_info%>%
      group_by(.dots = lab_factors[ii]) %>% 
      summarise(user_n = sum(user_n))
    names(user_info_df_var)[1] <- "Group_Name"
    user_info_df_var<-na.omit(user_info_df_var)
    total_population_user_var<-sum(as.integer(user_info_df_var$user_n))
    user_info_df_var$total_user<-rep(total_population_user, nrow(user_info_df_var))
    
    background_info_df<-plyr::rbind.fill(background_info_df, background_info_df_var) 
    user_info_df<-plyr::rbind.fill(user_info_df, user_info_df_var) 
  }
  
  merged_df<-merge(background_info_df, user_info_df, by="Group_Name", all=TRUE)
  
  
  merged_df$user_n<-merged_df$user_n %>% replace_na(0)
  merged_df$background_n<-merged_df$background_n %>% replace_na(0)
  
  merged_df$total_user<-merged_df$total_user %>% replace_na(0)
  merged_df$total_background<-merged_df$total_background %>% replace_na(0)
  
  
  merged_df$participant_rate<- mapply(Rate_Calculation,as.integer(merged_df$total_user),as.integer(merged_df$total_background))
  
  #calculate the observed & background rates 
  merged_df$Observed_Rate<- mapply(Rate_Calculation,as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$Background_Rate<- mapply(Rate_Calculation,as.integer(merged_df$background_n),as.integer(merged_df$total_background))

  merged_df$pValue<- mapply(compare_population_proportion,as.numeric(merged_df$Background_Rate),as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$BH_p<- mapply(p.adjust,merged_df$pValue,method = "BH")
  merged_df$whether_significant<- mapply(whether_significant,as.numeric(merged_df$BH_p), sig_t)
  if (selected_metric == "AEO"){
    merged_df$EquityValue<- mapply(Adjusted_Equal_Opportunity,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)
    merged_df$EquityLable<- mapply(whether_biased_label, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,cutoff1,cutoff2,cutoff2)
  }

  else if (selected_metric == "LDI"){
    merged_df$EquityValue<- mapply(Log_Disparate_Impact,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)
    merged_df$EquityLable<-mapply(whether_biased_label, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,cutoff1,cutoff2,cutoff2)
  }
  else if (selected_metric == "QM"){
    merged_df$EquityValue<- mapply(Quality_Metric,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)
    merged_df$EquityLable<-mapply(whether_biased_label, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,cutoff1,cutoff2,cutoff2)
  }
  merged_df<-merged_df %>%mutate(Group_Name =  factor(Group_Name, levels = x)) %>%arrange(Group_Name)
  
  
  new_df_univariable<- data.frame(Group_Name = merged_df$Group_Name,
                                  BH_p =  merged_df$BH_p,
                                  Significant_Level =  merged_df$whether_significant,
                                  Equity_Value= merged_df$EquityValue,
                                  Equity_Level= merged_df$EquityLable,
                                  stringsAsFactors=FALSE)
  
  new_df_univariable<-new_df_univariable%>% mutate_if(is.numeric, round, digits=5)
  
  return(new_df_univariable)
  
}

preprocess_comparison_df<-function(sig_t, cut1,cut2, selected_metrics, disease_type, file_list){
  diabetes_attributes<-c("Female", "Male","18-44", "45-64","64+","NH White","NH Black" ,"Hispanic","Other","<HSG" ,"HSG/GED","Some college/TS",">=College grad","Smoke","No smoke", "Underweight","Normal weight","Overweight","Obese","SBP<120","SBP 120-129","SBP 130-139","SBP>=140","Normal TC","High TC","Glucose<100","Glucose 100-125","Glucose>=126")
  hypertension_attributes<-c("Female", "Male","18-39", "40-59","59+","NH White","NH Black","NH Asian","Hispanic","Other","<HSG" ,"HSG/GED","Some college/TS",">=College grad","Smoke","No smoke", "Underweight","Normal weight","Overweight","Obese","SBP<120","SBP 120-129","SBP 130-139","SBP>=140","Normal TC","High TC","Glucose<100","Glucose 100-125","Glucose>=126")
  
  num_files<- length(file_list)
  
  if (selected_metrics=="AEO"){
    t1<-cut1
    t2<-cut2
  }
  else{
    t1<--log(1-cut1)
    t2<--log(1-cut2)
  }
  
  if (disease_type == "diabetes"){
    df_comparison_studies<-make_table_combine(NHANES_diabetes, file_list[[1]], NHANES_diabetes_lab,diabetes_attributes,sig_t,t1,t2 ,selected_metrics)
    colnames(df_comparison_studies)[2:5] <- paste(colnames(df_comparison_studies)[2:5],"(1)",  sep = "")
    if (num_files>1){
      for (i in 2:num_files){
        file2_univariable_combine<-make_table_combine(NHANES_diabetes, file_list[[i]], NHANES_diabetes_lab,diabetes_attributes,sig_t,t1,t2 ,selected_metrics)
        colnames(file2_univariable_combine)[2:5] <- paste(colnames(file2_univariable_combine)[2:5],paste0("(",i,")"),  sep = "")
        df_comparison_studies<-merge(df_comparison_studies,file2_univariable_combine, by = "Group_Name")
      }
    }
  }
  else if (disease_type == "hypertension"){
    df_comparison_studies<-make_table_combine(NHANES_hypertension, file_list[[1]], NHANES_hypertension_lab,hypertension_attributes,sig_t,t1,t2 ,selected_metrics)
    colnames(df_comparison_studies)[2:5] <- paste(colnames(df_comparison_studies)[2:5],"(1)",  sep = "")
    if (num_files>1){
      for (i in 2:num_files){
        file2_univariable_combine<-make_table_combine(NHANES_hypertension, file_list[[i]], NHANES_hypertension_lab,hypertension_attributes,sig_t,t1,t2 ,selected_metrics)
        colnames(file2_univariable_combine)[2:5] <- paste(colnames(file2_univariable_combine)[2:5],paste0("(",i,")"),  sep = "")
        df_comparison_studies<-merge(df_comparison_studies,file2_univariable_combine, by = "Group_Name")
      }
    }
  }
  
  if (disease_type == "diabetes"){
    df_comparison_studies_new<-df_comparison_studies %>%mutate(Group_Name =  factor(Group_Name, levels = diabetes_attributes)) %>%arrange(Group_Name)
    df_comparison_studies_new$Group_Name<-c("Female","Male","18-44","45-64","64+","Non-Hispanic White","Non-Hispanic Black","Hispanic","Other","Less than high school","High-school graduate","Some college/Technical school","College degree or higher","Current smoker" ,"Not smoke","Underweight","Normal weight","Overweight","Obese","less than 120","120-129","130-139","greater than 139","Normal","High","less than 5.6","5.6-6.9","greater than 6.9")
  }
  else if (disease_type == "hypertension"){
    df_comparison_studies_new<-df_comparison_studies %>%mutate(Group_Name =  factor(Group_Name, levels = hypertension_attributes)) %>%arrange(Group_Name)
    df_comparison_studies_new$Group_Name<-c("Female","Male","18-39","40-59","59+","Non-Hispanic White","Non-Hispanic Black","Non-Hispanic Asian","Hispanic","Other","Less than high school","High-school graduate","Some college/Technical school","College degree or higher","Current smoker" ,"Not smoke","Underweight","Normal weight","Overweight","Obese","less than 120","120-129","130-139","greater than 139","Normal","High","less than 5.6","5.6-6.9","greater than 6.9")
  }
  df_comparison_studies_new<-df_comparison_studies_new[c(1,seq(4,ncol(df_comparison_studies_new),by=4))]
  return(df_comparison_studies_new)
}



