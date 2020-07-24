
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

rzl_1 <- data[, c("RZL_01", "RZL_02", "RZL_03", "RZL_04", "RZL_05",
                "RZL_06", "RZL_07", "RZL_08", "RZL_09", "RZL_10", 
                "RZL_11", "RZL_12", "RZL_13", "RZL_14", "RZL_15",
                "RZL_16")] ## creating a dataframe containing RZL items

rzl_reliability_1 <- ci.reliability(data=rzl_1, type="omega", interval.type = "bca", B=1000) ## Estimating RZL reliability

sth_1 <-  data[, c("STH_01", "STH_02", "STH_03", "STH_04", "STH_05",
                 "STH_06", "STH_07", "STH_08")] ## creating a dataframe containing STH items

sth_reliability_1 <- ci.reliability(data=sth_1, type="omega", interval.type = "bca", B=1000) ## Estimating STH reliability

bjc_1 <- data[, c("BJC_01", "BJC_02", "BJC_03", "BJC_04", "BJC_05",
                "BJC_06", "BJC_07", "BJC_08", "BJC_09", "BJC_10",
                "BJC_11", "BJC_12")] ## creating a dataframe containing BJC items

bjc_reliability_1 <- ci.reliability(data=bjc_1, type="omega", interval.type = "bca", B=1000) ## Estimating BJC reliability

cn_1 <-  data[, c("CN_01", "CN_02", "CN_03", "CN_04", "CN_05",
                "CN_06", "CN_07R", "CN_08", "CN_09")] ## creating a dataframe containing CN items

cn_reliability_1 <- ci.reliability(data=cn_1, type="omega", interval.type = "bca", B=1000) ## Estimating CN reliability


## Demographics and descriptive data

data$DEMO_4 <- factor(data$DEMO_4, levels=c(1:2), labels=c("Male", "Female"), ordered=F) # defining label for gender

describe(data$DEMO_3) # age means and SD
table(data$DEMO_4) # gender proportion

rzl_1 <- rzl_1%>%mutate(rzl_mean=rowMeans(rzl_1)) ## calculating row mean of religious zeal
describe(rzl_1)

sth_1 <- sth_1%>%mutate(sth_mean=rowMeans(sth_1)) ## calculating row mean of symbolic threat
describe(sth_1)

bjc_1 <- bjc_1%>%mutate(bjc_mean=rowMeans(bjc_1)) ## calculating row mean of belief in Jewish conspiracy
describe(bjc_1)

cn_1 <- cn_1%>%mutate(cn_mean=rowMeans(cn_1)) ## calculating row mean of collective narcissism
describe(cn_1)

corplot_1 = bind_cols(bjc_1$bjc_mean,sth_1$sth_mean,cn_1$cn_mean,rzl_1$rzl_mean) # Binding mean score to calculate zero-order correlations

data <- subset(data, select=-c(rzl,bjc,cn,sth,rzl_mean,bjc_mean,cn_mean,sth_mean)) ## remove total and mean score from dataset

correlation::correlation(corplot_1) ## corr matrix (zero-order correlation)
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
sth_1 ~ a*rzl_1 
cn_1 ~ b*rzl_1 + g*sth_1

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
                       "CN_05", "CN_06", "CN_07R", "CN_08", "CN_09"), 
           se="bootstrap", estimator="DWLS", bootstrap=1000)

standardizedSolution(fit, type="std.all", se=T, 
                     pvalue=T, ci=T, level=0.95) ## obtaining standardised loadings with 95% CI

summary(fit, standardized=T) ## extracting model parameters
fitMeasures(fit) ## extracting fit indices
lavInspect(fit, "rsquare") ## extracting rsquare 
lavInspect(fit, "cor.lv") ## extracting correlations between latent variables

options(max.print = 10000)

parameterEstimates(fit,
                   se=T, zstat=T, pvalue=T, ci=T, 
                   standardized=T) ## extracting unstandardised loadings with 95% CI



## Session info

sessionInfo()

set.seed(1989)
sample(2:368, 10)