library(readxl)
library(readr)
library(sqldf)
library(dplyr)
library(plyr)

K_responder_YN <- read_csv("../data/finalTbl_withPIVKAandOtherVKMeasures.csv")
subj_data <- K_responder_YN

age <- c("Age (y)", mean(subj_data$age_0[which(subj_data$K_responder_YN=="Yes")]), 
  sd(subj_data$age_0[which(subj_data$K_responder_YN=="Yes")]), 
  mean(subj_data$age_0[which(subj_data$K_responder_YN=="No")]), 
  sd(subj_data$age_0[which(subj_data$K_responder_YN=="No")]), 
  t.test(subj_data$age_0 ~ subj_data$K_responder_YN)$p.value)

sex_Y <- as.data.frame(table(subj_data$M1F0[which(subj_data$K_responder_YN=="Yes")]))
sex_N <- as.data.frame(table(subj_data$M1F0[which(subj_data$K_responder_YN=="No")]))
sex <- c("Sex, N, (%F)", 
         sex_Y[1,2], sex_Y[1,2]/(sex_Y[1,2]+sex_Y[2,2]), sex_N[1,2], sex_N[1,2]/(sex_N[1,2]+sex_N[2,2]),
         chisq.test(table(subj_data$M1F0, subj_data$K_responder_YN))$p.value)

bmi <- c("BMI (kg/m2)", mean(subj_data$bmi_0[which(subj_data$K_responder_YN=="Yes")]), 
         sd(subj_data$bmi_0[which(subj_data$K_responder_YN=="Yes")]), 
         mean(subj_data$bmi_0[which(subj_data$K_responder_YN=="No")]), 
         sd(subj_data$bmi_0[which(subj_data$K_responder_YN=="No")]), 
         t.test(subj_data$bmi_0 ~ subj_data$K_responder_YN)$p.value)

smoker_Y <- as.data.frame(table(subj_data$smoker_0[which(subj_data$K_responder_YN=="Yes")]))
smoker_N <- as.data.frame(table(subj_data$smoker_0[which(subj_data$K_responder_YN=="No")]))
smoker <- c("Smoker, N, (%Y)", 
            smoker_Y[2,2], smoker_Y[2,2]/(as.numeric(smoker_Y[2,2])+as.numeric(smoker_Y[2,1])), 
            ifelse(is.na(smoker_N[2,2])==T, 0,smoker_N[2,2]) , 
            ifelse(is.na(smoker_N[2,2])==T, 0,smoker_N[2,2])/(smoker_N[1,2]+(ifelse(is.na(smoker_N[2,2])==T, 0,smoker_N[2,2]))),
         chisq.test(table(subj_data$smoker_0, subj_data$K_responder_YN))$p.value)


statins_Y <- as.data.frame(table(subj_data$statinsyn[which(subj_data$K_responder_YN=="Yes")]))
statins_N <- as.data.frame(table(subj_data$statinsyn[which(subj_data$K_responder_YN=="No")]))
statins <- c("Cholesterol-lowering medication use, N (%Y)", 
             statins_Y[2,2], statins_Y[2,2]/(as.numeric(statins_Y[2,2])+as.numeric(statins_Y[2,1])), 
            ifelse(is.na(statins_N[2,2])==T, 0,statins_N[2,2]) , 
            ifelse(is.na(statins_N[2,2])==T, 0,statins_N[2,2])/(statins_N[1,2]+(ifelse(is.na(statins_N[2,2])==T, 0,statins_N[2,2]))),
            chisq.test(table(subj_data$statinsyn, subj_data$K_responder_YN))$p.value)

TG <- c("Baseline plasma TG (mg/dL)", mean(subj_data$TRIG[which(subj_data$K_responder_YN=="Yes")]), 
         sd(subj_data$TRIG[which(subj_data$K_responder_YN=="Yes")]), 
         mean(subj_data$TRIG[which(subj_data$K_responder_YN=="No")]), 
         sd(subj_data$TRIG[which(subj_data$K_responder_YN=="No")]), 
         t.test(subj_data$TRIG ~ subj_data$K_responder_YN)$p.value)

K1_b <- c("Baseline K1 (nmol/L)", mean(subj_data$baseline_K[which(subj_data$K_responder_YN=="Yes")]), 
         sd(subj_data$baseline_K[which(subj_data$K_responder_YN=="Yes")]), 
         mean(subj_data$baseline_K[which(subj_data$K_responder_YN=="No")]), 
         sd(subj_data$baseline_K[which(subj_data$K_responder_YN=="No")]), 
         t.test(subj_data$baseline_K ~ subj_data$K_responder_YN)$p.value)

K1_response <- c("3-Year Change in K1 (nmol/L)", mean(subj_data$response_K[which(subj_data$K_responder_YN=="Yes")]), 
          sd(subj_data$response_K[which(subj_data$K_responder_YN=="Yes")]), 
          mean(subj_data$response_K[which(subj_data$K_responder_YN=="No")]), 
          sd(subj_data$response_K[which(subj_data$K_responder_YN=="No")]), 
          t.test(subj_data$response_K ~ subj_data$K_responder_YN)$p.value)


subject_data <- as.data.frame(rbind(age, sex, bmi, smoker, statins, TG, K1_b, K1_response))
subject_data <- plyr::rename(subject_data, c("V1"="Measure", 
                                             "V2"="Responder Mean", "V3"="Responder SD",
                                             "V4"="Non-Responder Mean", "V5"="Non-Responder SD", 
                                             "V6"="p-value"))
subject_data[,2:6] <- sapply(subject_data[,2:6], as.character)
subject_data[,2:6] <- sapply(subject_data[,2:6], as.numeric)
subject_data
