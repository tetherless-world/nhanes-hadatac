#merge the data files
merge_raw_data<-function(folder_path, file_pattern, file_type, merged_ID){
  if (file_type == "xpt"){
    merged_data<- list.files(path = folder_path, pattern=file_pattern,full.names = T) %>%
      lapply(read_xpt) %>%
      reduce(full_join,by = merged_ID )
  }
  else if (file_type == "csv"){
    merged_data<- list.files(path = folder_path, pattern=file_pattern,full.names = T) %>%
      lapply(read.csv) %>%
      reduce(full_join,by = merged_ID )
  }
  
  return(merged_data)
}

# [b1, b2)if right = FALSE
process_continuous_variable<-function(x,breaks_of_range,category_values){
  cut(x, breaks = breaks_of_range, labels = category_values, include.lowest = TRUE, right = FALSE, ordered_result = TRUE)
}


#calculate BMI from height and weight 
bmi_calculation_lbs_inch<-function(weight, height){
  return(703 * weight / (height)^2)
}

bmi_calculation_kg_m<-function(weight, height_cm){
  height_m<-0.01*height_cm
  return(weight / (height_m)^2)
}


#BP avg. calculation : https://www.cdc.gov/nchs/data/nhanes/nhanes_07_08/manual_pe.pdf

#4.2.3.10 Averaging Rules for Determining Mean Blood Pressure 

nhanes_mean_SBP<-function(SBP1,SBP2,SBP3,SBP4){
  #ISIS calculates the blood pressure average using the following protocol:
  # - If only one blood pressure reading was obtained, that reading is the average.
  # - If there is more than one blood pressure reading, the first reading is always excluded from the average.
  # - If only two blood pressure readings were obtained, the second blood pressure reading is the average.
  total_sbp<-0
  avg_sbp<-NA
  sbp_available<- 0
  
  if (!is.na(SBP2)){
    sbp_available<-sbp_available+1
    total_sbp<-total_sbp+SBP2
  }
  if (!is.na(SBP3)){
    sbp_available<-sbp_available+1
    total_sbp<-total_sbp+SBP3
  }
  if (!is.na(SBP4)){
    sbp_available<-sbp_available+1
    total_sbp<-total_sbp+SBP4
  }
  
  if (sbp_available == 0){
    if (!is.na(SBP1)){
      avg_sbp<-SBP1
    }
    else{
      avg_sbp<-NA
    }
  }
  else if (sbp_available == 1){
    avg_sbp<-total_sbp
  }
  else{
    avg_sbp<-total_sbp/sbp_available 
  }
  return(avg_sbp)
  
}

nhanes_mean_DBP<-function(DBP1,DBP2,DBP3,DBP4){
  total_dbp<-0
  avg_dbp<-NA
  dbp_available<- 0
  # - If all diastolic readings were zero, then the average would be zero.
  #  Exception: If there is one diastolic reading of zero and one (or more)
  # with a number above zero, the diastolic reading with zero is not used to
  # calculate the diastolic average.
  # - If two out of three are zero, the one diastolic reading that is not zero is used to calculate the diastolic average.
  if (!is.na(DBP2)){
    if (DBP2 > 0){
      dbp_available<-dbp_available+1
      total_dbp<-total_dbp+DBP2
    }
  }
  
  if (!is.na(DBP3)){
    if (DBP3 > 0){
      dbp_available<-dbp_available+1
      total_dbp<-total_dbp+DBP3
    }
  }
  if (!is.na(DBP4)){
    if (DBP4 > 0){
      dbp_available<-dbp_available+1
      total_dbp<-total_dbp+DBP4
    }
  }
  
  if (dbp_available == 0){
    if (!is.na(DBP1)){
      avg_dbp<-DBP1
    }
    else{
      avg_dbp<-NA
    }
  }
  else if (dbp_available == 1){
    avg_dbp<-total_dbp
  }
  else{
    avg_dbp<-total_dbp/dbp_available 
  }
  
  return(avg_dbp)
  
}

race_ethnicityOrderNew <- c(1,2,3,4,5)
race_ethnicityLabelNew <- c("Hispanic","Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Other/Unknown")


race_ethnicityOrder <- c(1,2,3,4,5)
race_ethnicityLabel <- c("Hispanic","Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Asian", "Other Race")

eductionOrder <- c(1,2,3,4,5)
eductionLabel <- c("Less than 9th grade","9-11th grade (Includes 12th grade with no diploma)","High school graduate/GED or equivalent", "Some college or AA degree", "College graduate or above")

smokerOrder<-c(1,2,3,4)
smokerLabel<-c("Never smoker","Former smoker","Someday smoker","Every day smoker")


bmiOrder<-c(1,2,3,4)
bmiLabel<-c("Underweight","Normal weight","Overweight","Obese" )

sbpOrder<-c(1,2,3,4 )
sbpLabel<-c("SBP < 120 ","SBP 120-129","SBP 130-139","SBP ≥ 140" )
 
ageOrderHypertension<-c(1,2,3,4)
ageLabelHypertension<-c('0-49','50-59','60-74','≥ 75')

ageOrderbowen<-c(1,2,3,4,5,6,7)
ageLabelbowen<-c('0-54','55-59','60-64','65-69','70-74','75-79','≥ 80')

HbA1cOrder<-c(1,2,3,4,5,6,7,8)
HbA1cLabel<-c('<6%','6%-7%','7%-8%','8%-9%','9%-10%','10%-11%','11%-12%',
                          '≥ 12%')

HbA1cConditionOrder<-c(1,2,3)
HbA1cConditionLabel<-c('< 6%','6% - 9%','≥ 9%')

insuranceOrder<-c(1,2,3,4,5)
insuranceLabel<-c("Private insurance",'Medicare','Medicaid','Other insurance','No coverage')

drugCoverageOrder<-c(1,2)
drugCoveragLabel<-c('Cover prescription','Not cover prescription')


# MEN
# Risk score = ( 52.01 * ln(Age)) + (20.01 * ln(Total cholesterol)) + (-0.91 * ln(HDL)) + (1.31 * ln(Systolic BP)) + (0.24 * BP treatment) + (12.1 * Smoker) + (-4.61 * ln(Age) * ln(Total cholesterol)) + ((-2.84) * ln(Age) * Smoker) + ((-2.93) * ln(Age) * ln(Age)) - 172.30
# 
# 
# !For men aged > 70 years use: ((-2.84) * ln(70) * Smoker)
# 
# Death probability = 1 - 0.9402^exp(Risk score)
# 
# WOMEN
# Risk score = (31.76 * ln(Age)) + (22.47 * ln(Total cholesterol)) +( (-1.19) * ln(HDL) )+ (2.55 * ln(Systolic BP)) + (0.42 * BP treatment) + (13.08 * Smoker) + ((-5.06) * ln(Age) * ln(Total cholesterol)) + ((-3) * ln(Age) * Smoker) - 146.59
# 
# For women aged > 78 years use: ((-3) * ln(78) * Smoker)
# 
# Death probability = 1 - 0.98767^exp(Risk score)

risk_score<-function(Gender,Age,TC, HDL, SBP, BP_treatment, Smoker_i){
  if (is.na(Age)|is.na(TC)|is.na(Smoker_i)|is.na(Gender)|is.na(HDL)|is.na(SBP)|is.na(BP_treatment)){
    return(NA)
  }
    
  if (BP_treatment == "Yes"){
    BP_treat<-1
  }
  else{
    BP_treat<-0
  }
  if (Smoker_i == "Yes"){
    Smoker <-1
  }
  else (
    Smoker <-0
  )
  
  if (Gender == "Female"){
    if (Age <= 78){
      Risk_score<-(31.76 * log(Age)) + (22.47 * log(TC)) +( (-1.19) * log(HDL) )+ (2.55 * log(SBP)) + (0.42 * BP_treat) + (13.08 * Smoker) + ((-5.06) * log(Age) * log(TC)) + ((-3) * log(Age) * Smoker) - 146.59
    }
    else{
      Risk_score<-(31.76 * log(Age)) + (22.47 * log(TC)) +( (-1.19) * log(HDL) )+ (2.55 * log(SBP)) + (0.42 * BP_treat) + (13.08 * Smoker) + ((-5.06) * log(Age) * log(TC)) + ((-3) * log(78) * Smoker) - 146.59
      
    }
     
    risk_p<-1 - 0.98767^(exp(Risk_score))
  }
  else{
    if (Age <= 70){
      Risk_score<-( 52.01 * log(Age)) + (20.01 * log(TC)) + (-0.91 * log(HDL)) + (1.31 * log(SBP)) + (0.24 * BP_treat) + (12.1 * Smoker) + (-4.61 * log(Age) * log(TC)) + ((-2.84) * log(70) * Smoker) + ((-2.93) * log(Age) * log(Age)) - 172.30
    }
    else{
      Risk_score<-( 52.01 * log(Age)) + (20.01 * log(TC)) + (-0.91 * log(HDL)) + (1.31 * log(SBP)) + (0.24 * BP_treat) + (12.1 * Smoker) + (-4.61 * log(Age) * log(TC)) + ((-2.84) * log(70) * Smoker) + ((-2.93) * log(Age) * log(Age)) - 172.30
    }
    risk_p<-1 - (0.9402^exp(Risk_score))
  }
  
  return(risk_p)
}



# eGFR formula Scr=serum creatinine in µmol/L
# GFR (mL/min/1.73 m2) = 175 × (Scr)-1.154 × (Age)-0.203 × (0.742 if female) × (1.212 if African American)

eGFR_formula<-function(Scr, Age, Gender, Race_Ethnicity){
  results<- 175 * (Scr)-1.154*(Age)
  
  if (Gender == "Female"){
    if (Race_Ethnicity == "Non-Hispanic Black"){
      results<- results - 0.203 * (0.742) * (1.212)
    }
    else{
      results<- results - 0.203 * (0.742)
    }
  }
  return(results)

}

urine_total<-function(VOL1 ,TIME1,VOL2 , TIME2 ,VOL3 , TIME3 ){
  total_vol<-0
  total_time<-0
  if (!is.na(VOL1) & VOL1>0){
    total_vol<-total_vol+VOL1
  }
  
  if (!is.na(VOL2) & VOL2>0){
    total_vol<-total_vol+VOL2
  }
  
  if (!is.na(VOL3) & VOL3>0){
    total_vol<-total_vol+VOL3
  }
  
  if (!is.na(TIME1) & TIME1>0){
    total_time<-total_time+TIME1
  }
  if (!is.na(TIME2) &TIME2>0){
    total_time<-total_time+TIME2
  }
  if (!is.na(TIME3) &TIME3>0){
    total_time<-total_time+TIME3
  }
  
  return(total_vol/total_time)
  

}

proteinuria_calc<-function(urin_rate,Albumin ){
  results<-(urin_rate*24*60/1000)*Albumin/1000
  return(results)
}
#Albumin, urine (mg/L)
#urin_rate  mL/min

#read_xpt("./nhanes_raw/RXQ_DRUG.xpt")
# drug_vasodilators_count<-0
# drug_thiazide
# drug_potassium
# drug_LD
# drug_central
# drug_calcium
# drug_beta
# drug_ARB
# drug_AAA
# drug_renin
# drug_alpha
# drug_ACE


count_antihyp_class<-function(drug_code){
  all_drugs<-unlist(strsplit(drug_code, ","))
  types_med<- c()
  for (med in all_drugs){
    if (med %in% drug_vasodilators){
      types_med <- c(types_med, "vasodilators")
    }
    if (med %in% drug_thiazide){
      types_med <- c(types_med, "thiazide")
    }
    if (med %in% drug_potassium){
      types_med <- c(types_med, "potassium")
    }
    if (med %in% drug_LD){
      types_med <- c(types_med, "LD")
    }
    if (med %in% drug_central){
      types_med <- c(types_med, "central")
    }
    if (med %in% drug_calcium){
      types_med <- c(types_med, "calcium")
    }
    if (med %in% drug_beta){
      types_med <- c(types_med, "beta")
    }
    if (med %in% drug_ARB){
      types_med <- c(types_med, "ARB")
    }
    if (med %in% drug_AAA){
      types_med <- c(types_med, "AAA")
    }
    if (med %in% drug_renin){
      types_med <- c(types_med, "renin")
    }
    if (med %in% drug_alpha){
      types_med <- c(types_med, "alpha")
    }
    if (med %in% drug_ACE){
      types_med <- c(types_med, "ACE")
    }
  }
  
  count<-length(unique(types_med))
  return(count)
  
}

class1_compare_df<-function(jointdataset_low,diabetesTarget_by_race_low){
  result_class1_low<-drug_class_equity(jointdataset_low,"Classification_1",diabetesTarget_by_race_low,0.05,0.2,0.4)%>% select(Classification_1, Race_Ethnicity, EquityValue, EquityLable)
  
  result_class1_low<-result_class1_low[order(result_class1_low$Classification_1, result_class1_low$Race_Ethnicity),]
  result_class1_heatmap_low<-result_class1_low %>% select(Classification_1,Race_Ethnicity,EquityValue)%>%
    mutate(EquityValue = ifelse(EquityValue == "-Inf",-4, ifelse (EquityValue == "Inf",4,EquityValue)))
  
  result_class1_heatmap_cleaned_low<-result_class1_heatmap_low[!(result_class1_heatmap_low$Classification_1=="UNSPECIFIED" | result_class1_heatmap_low$Race_Ethnicity=="Other/Unknown"),]
  
}
class1_jointdata<-function(jointdataset_low,diabetesTarget_by_race_low){
  result_class1_low<-drug_class_equity(jointdataset_low,"Classification_1",diabetesTarget_by_race_low,0.05,0.2,0.4)%>% select(Classification_1, Race_Ethnicity, EquityValue, EquityLable)
  
  result_class1_low<-result_class1_low[order(result_class1_low$Classification_1, result_class1_low$Race_Ethnicity),]
  result_class1_heatmap_low<-result_class1_low %>% select(Classification_1,Race_Ethnicity,EquityValue)%>%
    mutate(EquityValue = ifelse(EquityValue == "-Inf",-4, ifelse (EquityValue == "Inf",4,EquityValue)))
  
  result_class1_heatmap_cleaned_low<-result_class1_heatmap_low[!(result_class1_heatmap_low$Classification_1=="UNSPECIFIED" | result_class1_heatmap_low$Race_Ethnicity=="Other/Unknown"),]
  
  class1_heatmap_cluster_low<-reshape2::dcast(result_class1_heatmap_cleaned_low, Classification_1 ~ Race_Ethnicity)
  class1_heatmap_cluster_low <- data.frame(class1_heatmap_cluster_low, row.names = 1)
  class1_heatmap_cluster_mat_low <- data.matrix(class1_heatmap_cluster_low)
  return(class1_heatmap_cluster_mat_low)
}


drug_for_race<-function(nhanes_df,antidiabetic_name_code){
  antidiabetic_med_by_race<-as.data.frame(svytable(~RXDDRGID+Race_Ethnicity,nhanes_df,addNA = TRUE,na.action=NULL,round=TRUE))%>%rename(background_n=`Freq`)
  antidiabetic_med_by_race$RXDDRGID <- factor(antidiabetic_med_by_race$RXDDRGID, levels=c(levels(antidiabetic_med_by_race$RXDDRGID), "xxxxxxx"))
  antidiabetic_med_by_race$RXDDRGID <- sub("^$", "xxxxxxx", antidiabetic_med_by_race$RXDDRGID)
  jointdataset <- merge(antidiabetic_med_by_race, antidiabetic_name_code, by = c("RXDDRGID"))
  
  return(jointdataset)
}
