#upload background datasets
NHANES_diabetes<-read.csv("modules/background_data/nhanes_diabetes.csv")
NHANES_diabetes$X<-NULL
NHANES_diabetes_lab<-read.csv("modules/background_data/nhanes_diabetes_lab.csv")
NHANES_diabetes_lab$X<-NULL
NHANES_diabetes_combined<-read.csv("modules/background_data/nhanes_diabetes_combined.csv")
NHANES_diabetes_combined$X<-NULL

NHANES_hypertension<-read.csv("modules/background_data/nhanes_hypertension.csv")
NHANES_hypertension$X<-NULL
NHANES_hypertension_lab<-read.csv("modules/background_data/nhanes_hypertension_lab.csv")
NHANES_hypertension_lab$X<-NULL
NHANES_hypertension_combined<-read.csv("modules/background_data/nhanes_hypertension_combined.csv")
NHANES_hypertension_combined$X<-NULL
