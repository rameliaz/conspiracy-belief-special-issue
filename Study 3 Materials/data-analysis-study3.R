## Importing dataset

data3 <- read.csv("Study 3 Materials/raw-data-study-3.csv")

library(psych)
library(MBESS)
library(tidyverse)
library(ggplot2)
library(forestmodel)


## Define factor levels for demographic variables

which(colnames(data3)=="TIME_RSI" )

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

## Applying exclusions

data3 <- data3[data3$ATT_CHECK == 4, ] ## select participants who correctly answered attention check question
data3 <- data3[data3$TIME_RSI < 2.5, ] ## select participants who have TIME_RSI < 2.5


## Demographic variables

describe(data3$DEMO_3)
describe(data3$MNC_01)
table(data3$DEMO_4)
table(data3$DEMO_1)


## Testing reliability

bjc_3 <- data3[, c("BJC_01", "BJC_02", "BJC_03", "BJC_04", "BJC_05",
                 "BJC_06", "BJC_07", "BJC_08", "BJC_09", "BJC_10",
                 "BJC_11", "BJC_12")]

bjc_reliability_3 <- ci.reliability(data=bjc_3, type="omega", interval.type = "bca", B=1000)

vcb_3 <- data3[, c("VBC_01", "VBC_02", "VBC_03", "VBC_04", "VBC_05",
                 "VBC_06", "VBC_07", "VBC_08")]

vcb_reliability_3 <- ci.reliability(data=vcb_3, type="omega", interval.type = "bca", B=1000)

sth_3 <-  data3[, c("STH_01", "STH_02", "STH_03", "STH_04", "STH_05",
                 "STH_06", "STH_07", "STH_08")]

sth_reliability_3 <- ci.reliability(data=sth_3, type="omega", interval.type = "bca", B=1000)

bl_3 <-  data3[, c("BL_01", "BL_02", "BL_03", "BL_04", "BL_05")]

bl_reliability_3 <- ci.reliability(data=bl_3, type="omega", interval.type = "bca", B=1000)

vc_3 <- data3[, c("VC_01", "VC_02")]

vc_reliability_3 <- ci.reliability(data=vc_3, type="omega", interval.type = "icc", B=1000)


## Computing total score for each scale and mean centering BJC

data3 <- data3 %>% as_tibble() %>% mutate(
  bjc=BJC_01+BJC_02+BJC_03+BJC_04+BJC_05+BJC_06+BJC_07+BJC_08+BJC_09+BJC_10+BJC_11+BJC_12,
  vcb=VBC_01+VBC_02+VBC_03+VBC_04+VBC_05+VBC_06+VBC_07+VBC_08,
  vc=VC_01+VC_02,
  bl=BL_01+BL_02+BL_03+BL_04+BL_05,
  bjc.mc=bjc-mean(bjc),
  rel_identity=religious_endorse*identity_threat
)

## Mean, SD and zero order correlation

bjc_3 <- bjc_3%>%mutate(bjc_mean=rowMeans(bjc_3)) ## calculating row mean of belief in Jewish conspiracy
describe(bjc_3)

vcb_3 <- vcb_3%>%mutate(vcb_mean=rowMeans(vcb_3)) ## calculating row mean of belief in vaccine conspiracy
describe(vcb_3)

bl_3 <- bl_3%>%mutate(bl_mean=rowMeans(bl_3)) ## calculating row mean of blame
describe(bl_3)

vc_3 <- vc_3%>%mutate(vc_mean=rowMeans(vc_3)) ## calculating row mean of vaccine intention
describe(vc_3)

sth_3 <- sth_3%>%mutate(sth_mean=rowMeans(sth_3)) ## calculating row mean of vaccine intention
data3 <- cbind(data3, sth=sth_3$sth_mean) ## binding sth mean to main dataset (data3)

corplot_3 <- bind_cols(vcb_3$vcb_mean,bjc_3$bjc_mean,bl_3$bl_mean,vc_3$vc_mean) # Binding mean score to calculate zero-order correlations

correlation::correlation(corplot_3) ## Corr matrix



## Manipulation check

### Shapiro-Wilk Test

shapiro.test(data3$sth) ## examining the distribution of identity manipulation check
shapiro.test(data3$MNC_01) ## examining the distribution of religious authorities' endorsement manipulation check


### T-Test

t.test(data3$sth~data3$identity_threat) ## identity threat manipulation check
describeBy(data3$sth, group=data3$identity_threat)

ggstatsplot::ggbetweenstats(
  data = data3,
  x = identity_threat,
  y = sth,
  title = "Manipulation Check Identity Threat"
)


t.test(data3$MNC_01~data3$religious_endorse) ## religious endorsement manipulation check
describeBy(data3$MNC_01, group=data3$religious_endorse)

ggstatsplot::ggbetweenstats(
  data = data3,
  x = religious_endorse,
  y = MNC_01,
  title = "Manipulation Check Religous Endorsement"
)



## Model

model_1 <- lm(vcb ~ bjc.mc + identity_threat + religious_endorse + bjc.mc*identity_threat + bjc.mc*religious_endorse +
              identity_threat*religious_endorse, data=data3)
model.summary <- summary(model_1) # model parameter
confint(model_1) # 95% confidence interval of model parameter
plot(model_1) # model diagnostics
report::report(model_1)

print(forest_model(model_1)) ## Printing forest plot for the model

### Bootstrapping model

model.boot <- car::Boot(model_1, R=10000) ## bootstrapping regression model by 1000 iterations
summary(model.boot) ## Boostrapped model parameter
confint(model.boot) ## 95% CI of boostrapped model parameter
plot(model.boot) ## model diagnostics

print(forest_model(model.boot)) ## Printing forest plot for the model


ggstatsplot::ggscatterstats(
  data=data3,
  x=bjc,
  y=vcb,
  xlab="Belief in Jewish Conspiracy",
  ylab="Vaccine Conspiracy Beliefs",
  title="BJC and VCB"
)


## VCB accross four experimental groups

ggstatsplot::ggbetweenstats(
  data = data3,
  x = group,
  y = vcb,
  title = "Kepercayaan Konspiratif Vaksin Antar-Kelompok",
  xlab="Kelompok",
  ylab="Kepercayaan Konspiratif Vaksin"
)


## Blame vs vaccination intention

model_2 <- lm(vc ~ BL_01+BL_02+BL_03+BL_04+BL_05, data=data3)
summary(model_2) # model parameter
confint(model_2) # 95% confidence interval of model parameter
report::report(model_2)

print(forest_model(model_2)) ## Printing forest plot for the model

## Randomisation check

ggstatsplot::ggbetweenstats(
  data = data3,
  x = group,
  y = bjc,
  title = "Kepercayaan Konspiratif Yahudi Ditinjau dari Kelompok",
  xlab="Kelompok",
  ylab="Kepercayaan Konspiratif Yahudi"
)


## Plots (Exploratory)

ggstatsplot::ggbetweenstats(
  data = data3,
  x = DEMO_5,
  y = bjc,
  title = "Kepercayaan Konspiratif Yahudi Ditinjau dari Tingkat Pendidikan",
  xlab="Tingkat Pendidikan",
  ylab="Kepercayaan Konspiratif Yahudi"
)

ggstatsplot::ggbetweenstats(
  data = data3,
  x = DEMO_5,
  y = vcb,
  title = "Kepercayaan Konspiratif Terhadap Vaksin Ditinjau dari Tingkat Pendidikan",
  xlab="Tingkat Pendidikan",
  ylab="Kepercayaan Konspiratif Terhadap Vaksin"
)




## Session info

sessionInfo()