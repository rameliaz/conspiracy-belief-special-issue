
## Importing dataset

data <- read.csv("Study 1 Materials/raw-data-study-1.csv")
library(lavaan)
library(psych)
library(MBESS)
library(tidyverse)

## Reverse coding CN_07

data$CN_07R <- car::recode(data$CN_07, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')

## Testing reliability of each scale

str(data)

rzl <- data[, c("RZL_01", "RZL_02", "RZL_03", "RZL_04", "RZL_05",
                "RZL_06", "RZL_07", "RZL_08", "RZL_09", "RZL_10", 
                "RZL_11", "RZL_12", "RZL_13", "RZL_14", "RZL_15",
                "RZL_16")] ## creating a dataframe containing RZL items

rzl_reliability <- ci.reliability(data=rzl, type="omega", interval.type = "bca", B=1000) ## Estimating RZL reliability

sth <-  data[, c("STH_01", "STH_02", "STH_03", "STH_04", "STH_05",
                 "STH_06", "STH_07", "STH_08")] ## creating a dataframe containing STH items

sth_reliability <- ci.reliability(data=sth, type="omega", interval.type = "bca", B=1000) ## Estimating STH reliability

bjc <- data[, c("BJC_01", "BJC_02", "BJC_03", "BJC_04", "BJC_05",
                "BJC_06", "BJC_07", "BJC_08", "BJC_09", "BJC_10",
                "BJC_11", "BJC_12")] ## creating a dataframe containing BJC items

bjc_reliability <- ci.reliability(data=bjc, type="omega", interval.type = "bca", B=1000) ## Estimating BJC reliability

cn <-  data[, c("CN_01", "CN_02", "CN_03", "CN_04", "CN_05",
                "CN_06", "CN_07R", "CN_08", "CN_09")] ## creating a dataframe containing CN items

cn_reliability <- ci.reliability(data=cn, type="omega", interval.type = "bca", B=1000) ## Estimating CN reliability


## Demographics and descriptive data

data$DEMO_4 <- factor(data$DEMO_4, levels=c(1:2), labels=c("Male", "Female"), ordered=F)

describe(data$DEMO_3) # age means and SD
table(data$DEMO_4) # gender proportion

rzl <- rzl%>%mutate(rzl_mean=rowMeans(rzl)) ## calculating row mean of religious zeal
describe(rzl)

sth <- sth%>%mutate(sth_mean=rowMeans(sth)) ## calculating row mean of symbolic threat
describe(sth)

bjc <- bjc%>%mutate(bjc_mean=rowMeans(bjc)) ## calculating row mean of belief in Jewish conspiracy
describe(bjc)

cn <- cn%>%mutate(cn_mean=rowMeans(cn)) ## calculating row mean of collective narcissism
describe(cn)

corplot_1 = bind_cols(bjc$bjc_mean,sth$sth_mean,cn$cn_mean,rzl$rzl_mean) # Binding mean score to calculate zero-order correlations

data <- subset(data, select=-c(rzl,bjc,cn,sth,rzl_mean,bjc_mean,cn_mean,sth_mean)) ## remove total and mean score from dataset

PerformanceAnalytics::chart.Correlation(corplot_1, histogram=T,pch=19) ## corr matrix (zero-order correlation)
corr.test(corplot_1) 



## Model

model <- '

# Measurement Model

sth_1 =~ STH_01 + STH_02 + STH_03 + STH_04 + STH_05 + STH_06 + STH_07 + STH_08
rzl_1 =~ RZL_01 + RZL_02 + RZL_03 + RZL_04 + RZL_05 + RZL_06 + RZL_07 + RZL_08 + RZL_09 + RZL_10 + RZL_11 + RZL_12 + RZL_13 + RZL_14 + RZL_15 + RZL_16
cn_1 =~ CN_01 + CN_02 + CN_03 + CN_04 + CN_05 + CN_06 + CN_07R + CN_08 + CN_09
bjc_1 =~ BJC_01 + BJC_02 + BJC_03 + BJC_04 + BJC_05 + BJC_06 + BJC_07 + BJC_08 + BJC_09 + BJC_10 + BJC_11 + BJC_12

# Structural Model

bjc_1 ~ d*sth_1 + f*cn_1 + c*rzl_1
sth_1 ~ a*rzl_1 + g*cn_1
cn_1 ~ b*rzl_1

# Indirect effects

## Through ST
ad := a*d

## Through CN
bf := b*f

'

fit <- sem(model, data=data,
             ordered=c("RZL_01", "RZL_02", "RZL_03", "RZL_04", "RZL_05", 
                       "RZL_06", "RZL_07", "RZL_08", "RZL_09", "RZL_10", 
                       "RZL_11", "RZL_12", "RZL_13", "RZL_14", "RZL_15",
                       "RZL_16", "STH_01", "STH_02", "STH_03", "STH_04", 
                       "STH_05", "STH_06", "STH_07", "STH_08","BJC_01", 
                       "BJC_02", "BJC_03", "BJC_04", "BJC_05", "BJC_06", 
                       "BJC_07", "BJC_08", "BJC_09", "BJC_10", "BJC_11", 
                       "BJC_12","CN_01", "CN_02", "CN_03", "CN_04", 
                       "CN_05", "CN_06", "CN_07R", "CN_08", "CN_09"))

parameters::model_parameters(fit, standardize=T) ## obtaining standardised loadings with 95% CI

summary(fit)
fitMeasures(fit)
lavInspect(fit, "rsquare")
lavInspect(fit, "cor.lv")
lavInspect(fit, "coef")$theta
lavInspect(fit, what="list")
vcov(fit)
