## Importing dataset

data3 <- read.csv("Study 3 Materials/raw-data-study-3.csv")
data.table::setDF(data3)

rm(data3_b)

library(lavaan)
library(psych)
library(MBESS)
library(tidyverse)
library(ggplot2)

## Define factor levels for demographic variables

which(colnames(data3_b)=="TIME_RSI" )

data3$DEMO_1 <- factor(data3$DEMO_1, levels=c(1:4), labels=c("never at all", "once",
                                                             "several times", "almost every day"), ordered=T)

data3$DEMO_2 <- factor(data3$DEMO_2, levels=c(1:6), labels=c("Nation", "Race", "Religion",
                                                             "Ethnic and Racial Groups", "Ethnic and Religious Groups",
                                                             "Race and Religion"), ordered=T)

data3$DEMO_4 <- factor(data3$DEMO_4, levels=c(1:2), labels=c("Male", "Female"), ordered=F)

data3$DEMO_5 <- factor(data3$DEMO_5, levels=c(1:6), labels=c("Junior High School", "Senior High School",
                                                             "Diploma (D1/D2/D3)", "Bachelor (S1/SST)",
                                                             "Master (S2)", "Doctoral (S3)"), ordered=T)

data3$DEMO_7 <- factor(data3$DEMO_7, levels=c(1:17), labels=c("Javanese", "Sundanese", "Batak",
                                                              "Madurese", "Betawi", "Minangnese",
                                                              "Bugis", "Malay", "Indonesian Arabs",
                                                              "Banten", "Banjarese", "Balinese", 
                                                              "Sasak", "Dayak", "Tionghoa (Indonesian Chinese)",
                                                              "Mixed", "Others"), ordered=F)

data3$DEMO_8 <- factor(data3$DEMO_8, levels=c(1:6), labels=c("Surabaya", "Jakarta", "Bandung",
                                                             "Yogyakarta", "Malang", "Others"), ordered=F)

data3$DEMO_9 <- factor(data3$DEMO_9, levels=c(1:6), 
                       labels=c("In the public sector (civil servants, etc.)",
                                "In the private sector (private/independent entrepreneurs)",
                                "In the non-profit sector (social workers, environmental cadres, PAUD cadres, NGOs, etc.)",
                                "As a housewife", " As a Student", "As a University Student"), ordered=F)

table(data3$DEMO_9)
describe(data3$DEMO_6)


## Applying exclusions

data3 <- data3[data3$ATT_CHECK == 4, ] ## select participants who correctly answered attention check question
data3 <- data3[data3$TIME_RSI < 2.5, ] ## select participants who have TIME_RSI < 2.5


## Testing reliability

bjc <- data3[, c("BJC_01", "BJC_02", "BJC_03", "BJC_04", "BJC_05",
                 "BJC_06", "BJC_07", "BJC_08", "BJC_09", "BJC_10",
                 "BJC_11", "BJC_12")]

ci.reliability(data=bjc, type="omega", interval.type = "bca", B=1000)

vcb <- data3[, c("VBC_01", "VBC_02", "VBC_03", "VBC_04", "VBC_05",
                 "VBC_06", "VBC_07", "VBC_08")]

ci.reliability(data=vcb, type="omega", interval.type = "bca", B=1000)

sth <-  data3[, c("STH_01", "STH_02", "STH_03", "STH_04", "STH_05",
                 "STH_06", "STH_07", "STH_08")]

ci.reliability(data=sth, type="omega", interval.type = "bca", B=1000)

bl <-  data3[, c("BL_01", "BL_02", "BL_03", "BL_04", "BL_05")]

ci.reliability(data=bl, type="omega", interval.type = "bca", B=1000)


## Manipulation check
