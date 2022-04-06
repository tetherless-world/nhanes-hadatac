nhanesSubgroupProcess_colversion_class3 <-function(NHANES_all_mec_unique, HbA1cLevel, insuranceType, NumComorbidity, given_title){
  NHANES_subgroup<- subset(NHANES_all_mec_unique, (HbA1cCondition %in% HbA1cLevel) & (Insurance_Type %in% insuranceType) & (CCI_level %in% NumComorbidity))
  
  jointdataset<-drug_for_race(NHANES_subgroup,antidiabetic_name_code)
  
  jointdataset$y %>% replace_na("unknown")
  
  diabetesTargetRace<-as.data.frame(svytable(~Race_Ethnicity,NHANES_subgroup,addNA = TRUE,na.action=NULL,round=TRUE))%>%rename(background_n=`Freq`)%>% mutate(Background_Rate = background_n/sum(background_n))
  class3_heatmap_cluster_mat<-class3_jointdata(jointdataset,diabetesTargetRace)
  
  heatmap_class3<-Heatmap(class3_heatmap_cluster_mat, col = mycols, column_title = given_title, show_heatmap_legend = FALSE, row_names_gp = gpar(fontsize = 8), show_column_dend = FALSE, show_row_dend = FALSE,row_order = sort(rownames(class3_heatmap_cluster_mat)))
  co_3 <- column_order(heatmap_class3)
  return (co_3)
}


class3_jointdata<-function(jointdataset_low,diabetesTarget_by_race_low){
  result_class3_low<-drug_class_equity3(jointdataset_low,"Classification_3",diabetesTarget_by_race_low,0.05,0.2,0.4)%>% select(Classification_3, Race_Ethnicity, EquityValue, EquityLable)
  
  
  result_class3_low<-result_class3_low[order(result_class3_low$Classification_3, result_class3_low$Race_Ethnicity),]
  
  result_class3_heatmap_low<-result_class3_low %>% select(Classification_3,Race_Ethnicity,EquityValue)%>%
    mutate(EquityValue = ifelse(EquityValue == "-Inf",-4, ifelse(EquityValue == "Inf",4, EquityValue)))
  
  result_class3_heatmap_cleaned_low<-result_class3_heatmap_low[!(result_class3_heatmap_low$Classification_3=="ANTIDIABETIC AGENTS - UNSPECIFIED" | result_class3_heatmap_low$Race_Ethnicity=="Other Race"),]
  
  class3_heatmap_cluster_low<-dcast(result_class3_heatmap_cleaned_low, Classification_3 ~ Race_Ethnicity)
  class3_heatmap_cluster_low <- data.frame(class3_heatmap_cluster_low, row.names = 1)
  
  class3_heatmap_cluster_mat_low <- data.matrix(class3_heatmap_cluster_low)
  return(class3_heatmap_cluster_mat_low)
}




nhanesSubgroupProcess_class3 <-function(NHANES_all_mec_unique, HbA1cLevel, insuranceType, NumComorbidity, given_title,co_2){
  NHANES_subgroup<- subset(NHANES_all_mec_unique, HbA1cCondition %in% HbA1cLevel& Insurance_Type %in% insuranceType & (CCI_level %in% NumComorbidity))
  
  jointdataset<-drug_for_race(NHANES_subgroup,antidiabetic_name_code)
  
  jointdataset$y %>% replace_na("unknown")
  
  diabetesTargetRace<-as.data.frame(svytable(~Race_Ethnicity,NHANES_subgroup,addNA = TRUE,na.action=NULL,round=TRUE))%>%rename(background_n=`Freq`)%>% mutate(Background_Rate = background_n/sum(background_n))
  
  class3_heatmap_cluster_mat<-class3_jointdata(jointdataset,diabetesTargetRace)
  
  heatmap_class3<-Heatmap(class3_heatmap_cluster_mat, col = mycols, column_title = given_title, show_heatmap_legend = FALSE, row_names_gp = gpar(fontsize = 8), show_column_dend = FALSE, show_row_dend = FALSE,row_order = sort(rownames(class3_heatmap_cluster_mat)))#,column_order=co_2)
  
  return (heatmap_class3)
}
