# Generates the numbers and group comparisons used to populate Table 1 
# (responder and non-responder characteristics)

library(readxl)
library(readr)
library(sqldf)

responder_yn <- read_excel("C:/Users/jfium/Dropbox/Jenny-20130612/Nutrition-Tufts/Thesis/Working/1760/Student Scholarship Application/Write Up/responder_yn.xlsx")
X1760_wide <- read_csv("C:/Users/jfium/Dropbox/Jenny-20130612/Nutrition-Tufts/Thesis/Working/1760/Aim 2.1/Data/1760_wide_from_long.csv")
subj_data <- merge(X1760_wide, responder_yn, by="hnrcid", all.x=T)
sqldf('select count(*) from a where responder_yn=="No"')
names(X1760_wide)

mean(subj_data$age_0[which(subj_data$responder_yn=="Yes")])
sd(subj_data$age_0[which(subj_data$responder_yn=="Yes")])
mean(subj_data$age_0[which(subj_data$responder_yn=="No")])
sd(subj_data$age_0[which(subj_data$responder_yn=="No")])
t.test(subj_data$age_0 ~ subj_data$responder_yn)

table(subj_data$M1F0[which(subj_data$responder_yn=="Yes")])
table(subj_data$M1F0[which(subj_data$responder_yn=="No")])
chisq.test(table(subj_data$M1F0, subj_data$responder_yn))

mean(subj_data$bmi_0[which(subj_data$responder_yn=="Yes")])
sd(subj_data$bmi_0[which(subj_data$responder_yn=="Yes")])
mean(subj_data$bmi_0[which(subj_data$responder_yn=="No")])
sd(subj_data$bmi_0[which(subj_data$responder_yn=="No")])
t.test(subj_data$bmi_0 ~ subj_data$responder_yn)

table(subj_data$smoker_mod[which(subj_data$responder_yn=="Yes")])
table(subj_data$smoker_mod[which(subj_data$responder_yn=="No")])
chisq.test(table(subj_data$smoker_mod, subj_data$responder_yn))

table(subj_data$statinsyn[which(subj_data$responder_yn=="Yes")])
table(subj_data$statinsyn[which(subj_data$responder_yn=="No")])
chisq.test(table(subj_data$statinsyn, subj_data$responder_yn))

mean(subj_data$TRIG[which(subj_data$responder_yn=="Yes")])
sd(subj_data$TRIG[which(subj_data$responder_yn=="Yes")])
mean(subj_data$TRIG[which(subj_data$responder_yn=="No")])
sd(subj_data$TRIG[which(subj_data$responder_yn=="No")])
t.test(subj_data$TRIG ~ subj_data$responder_yn)

mean(subj_data$K1[which(subj_data$responder_yn=="Yes")])
sd(subj_data$K1[which(subj_data$responder_yn=="Yes")])
mean(subj_data$K1[which(subj_data$responder_yn=="No")])
sd(subj_data$K1[which(subj_data$responder_yn=="No")])
t.test(subj_data$K1 ~ subj_data$responder_yn)

X1760_long <- read_csv("C:/Users/jfium/Dropbox/Jenny-20130612/Nutrition-Tufts/Thesis/Working/1760/Aim 2.1/Data/1760_long.csv", 
                       col_types = cols(CHOL_D = col_double(), HDL_D = col_double(), K1_D = col_double(), LDL_D = col_double(), 
                                        TRIG_D = col_double(), T_OC_D = col_double(), dp_ucMGP_D = col_double(), 
                                        p_ucOC_D = col_double(), ucOC_D = col_double()))
X1760_long_1246 <- X1760_long[which(X1760_long$VISIT==1 | X1760_long$VISIT==2
                                  |X1760_long$VISIT==4 | X1760_long$VISIT==6),]

long_data <- as.data.frame(aggregate(X1760_long_1246[,18], list(X1760_long_1246$hnrcid), median, na.rm=T))
names(long_data)[1] <- "hnrcid"
names(long_data)[2] <- "K1_median"

mean(subj_data$K1[which(subj_data$responder_yn=="Yes")])
sd(subj_data$K1[which(subj_data$responder_yn=="Yes")])
mean(subj_data$K1[which(subj_data$responder_yn=="No")])
sd(subj_data$K1[which(subj_data$responder_yn=="No")])
t.test(subj_data$K1 ~ subj_data$responder_yn)

subj_data <- merge(subj_data, long_data, by="hnrcid")
mean(subj_data$K1_median[which(subj_data$responder_yn=="Yes")])
sd(subj_data$K1_median[which(subj_data$responder_yn=="Yes")])
mean(subj_data$K1_median[which(subj_data$responder_yn=="No")])
sd(subj_data$K1_median[which(subj_data$responder_yn=="No")])
t.test(subj_data$K1_median ~ subj_data$responder_yn)

subj_data$change <- subj_data$K1_median - subj_data$K1
mean(subj_data$change[which(subj_data$responder_yn=="Yes")])
sd(subj_data$change[which(subj_data$responder_yn=="Yes")])
mean(subj_data$change[which(subj_data$responder_yn=="No")])
sd(subj_data$change[which(subj_data$responder_yn=="No")])
t.test(subj_data$change ~ subj_data$responder_yn)

mean(subj_data$change[which(subj_data$KorP=="1")], na.rm=T)
sd(subj_data$change[which(subj_data$KorP=="1")], na.rm=T)
mean(subj_data$change[which(subj_data$KorP=="0")], na.rm=T)
sd(subj_data$change[which(subj_data$KorP=="0")], na.rm=T)
