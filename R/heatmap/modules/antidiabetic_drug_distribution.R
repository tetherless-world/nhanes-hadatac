Multum_drug_category_selection<-function(drug_df, level_1_list, level_2_list, level_3_list){
  if (length(level_1_list)>0 ) {
    drug_df<-drug_df %>% subset(drug_df$RXDDCI1A %in% level_1_list  | drug_df$RXDDCI2A %in% level_1_list| drug_df$RXDDCI3A %in% level_1_list | drug_info$RXDDCI4A %in% level_1_list) 
  }
  
  if (length(level_2_list)>0 ) {
    drug_df<-drug_df %>% subset(drug_df$RXDDCI1B %in% level_2_list| drug_df$RXDDCI2B %in% level_2_list| drug_df$RXDDCI3B %in% level_2_list|drug_df$RXDDCI4B %in% level_2_list) 
  }
  
  
  if (length(level_3_list)>0 ) {
    drug_df<-drug_df %>% subset(drug_df$RXDDCI1C %in% level_3_list| drug_df$RXDDCI2C %in% level_3_list|drug_df$RXDDCI3C %in% level_3_list | drug_df$RXDDCI4C %in% level_3_list) 
  }
  
  return(drug_df)
  
}


find_drugs_with_ingredient<-function(drug_df, ingredient_name){
  result_df<-drug_df %>% subset(ingredient_name %in% drug_df$RXDDRUG)
  return(result_df)
}


drug_class_equity<-function(patient_drugs_df, class_method,target_df,sig_t,tol_1,tol_2){
  
  drug_user_df<-patient_drugs_df%>%
    group_by(Classification_1, Race_Ethnicity)%>% 
    summarise(user_n = sum(background_n))%>%
    mutate(total_user = sum(user_n), 
           Observed_Rate = user_n/sum(user_n))
  
  total_population_background<-sum(target_df$background_n)
  target_df$total_background<-rep(total_population_background, nrow(target_df))
  
  
  #combine user input data with the background info
  merged_df<-merge(drug_user_df, target_df,sort = FALSE)
  
  merged_df$pValue<- mapply(compare_population_proportion_2,as.integer(merged_df$background_n),as.integer(merged_df$total_background),as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$BH_p<- p.adjust(merged_df$pValue,method = "BH")
  
  merged_df$EquityValue<- mapply(Log_Disparate_Impact,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)
  merged_df$EquityLable<-mapply(whether_biased_label, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)
  merged_df$EquityColors<-mapply(add_colors, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)
  
  return(merged_df)
}


drug_class_equity2<-function(patient_drugs_df, class_method,target_df,sig_t,tol_1,tol_2){
  
  patient_drugs_df_new<- strsplit(patient_drugs_df$Classification_2, split = ";")
  
  split_drugs<-data.frame(RXDDRGID = rep(patient_drugs_df$RXDDRGID, sapply(patient_drugs_df_new, length)),
                          RXDDRUG = rep(patient_drugs_df$RXDDRUG, sapply(patient_drugs_df_new, length)),
                          Race_Ethnicity = rep(patient_drugs_df$Race_Ethnicity, sapply(patient_drugs_df_new, length)),
                          background_n = rep(patient_drugs_df$background_n, sapply(patient_drugs_df_new, length)),Classification_2 = unlist(patient_drugs_df_new))
  
  drug_user_df<-split_drugs%>%
    group_by(Classification_2, Race_Ethnicity)%>% 
    summarise(user_n = sum(background_n))%>%
    mutate(total_user = sum(user_n), 
           Observed_Rate = user_n/sum(user_n))
  
  total_population_background<-sum(target_df$background_n)
  target_df$total_background<-rep(total_population_background, nrow(target_df))
  
  
  #combine user input data with the background info
  merged_df<-merge(drug_user_df, target_df,sort = FALSE)
  
  merged_df$pValue<- mapply(compare_population_proportion_2,as.integer(merged_df$background_n),as.integer(merged_df$total_background),as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$BH_p<- p.adjust(merged_df$pValue,method = "BH")
  
  merged_df$EquityValue<- mapply(Log_Disparate_Impact,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)
  merged_df$EquityLable<-mapply(whether_biased_label, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)
  merged_df$EquityColors<-mapply(add_colors, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)
  
  return(merged_df)
}


drug_class_equity3<-function(patient_drugs_df, class_method,target_df,sig_t,tol_1,tol_2){
  
  patient_drugs_df_new<- strsplit(patient_drugs_df$Classification_3, split = ";")
  
  split_drugs<-data.frame(RXDDRGID = rep(patient_drugs_df$RXDDRGID, sapply(patient_drugs_df_new, length)),
                          RXDDRUG = rep(patient_drugs_df$RXDDRUG, sapply(patient_drugs_df_new, length)),
                          Race_Ethnicity = rep(patient_drugs_df$Race_Ethnicity, sapply(patient_drugs_df_new, length)),
                          background_n = rep(patient_drugs_df$background_n, sapply(patient_drugs_df_new, length)),Classification_3 = unlist(patient_drugs_df_new))

  
  split_drugs$Classification_3<-trimws(split_drugs$Classification_3)
  drug_user_df<-split_drugs%>%
    group_by(Classification_3, Race_Ethnicity)%>% 
    summarise(user_n = sum(background_n))%>%
    mutate(total_user = sum(user_n), 
           Observed_Rate = user_n/sum(user_n))
  
  total_population_background<-sum(target_df$background_n)
  target_df$total_background<-rep(total_population_background, nrow(target_df))
  
  
  #combine user input data with the background info
  merged_df<-merge(drug_user_df, target_df,sort = FALSE)
  
  merged_df$pValue<- mapply(compare_population_proportion_2,as.integer(merged_df$background_n),as.integer(merged_df$total_background),as.integer(merged_df$user_n),as.integer(merged_df$total_user))
  merged_df$BH_p<- p.adjust(merged_df$pValue,method = "BH")
  
  merged_df$EquityValue<- mapply(Log_Disparate_Impact,as.numeric(merged_df$Background_Rate),as.numeric(merged_df$Observed_Rate), for_plot = FALSE)
  merged_df$EquityLable<-mapply(whether_biased_label, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)
  merged_df$EquityColors<-mapply(add_colors, as.numeric(merged_df$EquityValue),as.numeric(merged_df$BH_p),sig_t,tol_1,tol_2,tol_2)
  
  return(merged_df)
}