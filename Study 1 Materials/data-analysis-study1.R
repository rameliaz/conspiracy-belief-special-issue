
## Importing dataset

data <- read.csv("Study 1 Materials/raw-data-study-1.csv")
library(lavaan)
library(psych)
library(MBESS)
library(tidyverse)
library(MVN)

## Reverse coding CN_07

data$CN_07R <- car::recode(data$CN_07, '1=7; 2=6; 3=5; 4=4; 5=3; 6=2; 7=1')

## Testing reliability of each scale

str(data)

rzl <- data[, c("RZL_01", "RZL_02", "RZL_03", "RZL_04", "RZL_05",
                "RZL_06", "RZL_07", "RZL_08", "RZL_09", "RZL_10", 
                "RZL_11", "RZL_12", "RZL_13", "RZL_14", "RZL_15",
                "RZL_16")]
ci.reliability(data=rzl, type="omega", interval.type = "bca", B=1000)

sth <-  data[, c("STH_01", "STH_02", "STH_03", "STH_04", "STH_05",
                 "STH_06", "STH_07", "STH_08")]

ci.reliability(data=sth, type="omega", interval.type = "bca", B=1000)

bjc <- data[, c("BJC_01", "BJC_02", "BJC_03", "BJC_04", "BJC_05",
                "BJC_06", "BJC_07", "BJC_08", "BJC_09", "BJC_10",
                "BJC_11", "BJC_12")]

ci.reliability(data=bjc, type="omega", interval.type = "bca", B=1000)

cn <-  data[, c("CN_01", "CN_02", "CN_03", "CN_04", "CN_05",
                "CN_06", "CN_07R", "CN_08", "CN_09")]

ci.reliability(data=cn, type="omega", interval.type = "bca", B=1000)


## Demographics and descriptive data

describe(data$DEMO_3) # demographics
table(data$DEMO_4)

rzl <- rzl%>%mutate(rzl_mean=rowMeans(rzl)) ## calculating row mean of religious zeal
describe(rzl)

sth <- sth%>%mutate(sth_mean=rowMeans(sth)) ## calculating row mean 
describe(sth)

bjc <- bjc%>%mutate(bjc_mean=rowMeans(bjc)) ## calculating row mean 
describe(bjc)

cn <- cn%>%mutate(cn_mean=rowMeans(cn)) ## calculating row mean 
describe(cn)

## Model

model <- '

# Measurement Model

sth =~ STH_01 + STH_02 + STH_03 + STH_04 + STH_05 + STH_06 + STH_07 + STH_08
rzl =~ RZL_01 + RZL_02 + RZL_03 + RZL_04 + RZL_05 + RZL_06 + RZL_07 + RZL_08 + RZL_09 + RZL_10 + RZL_11 + RZL_12 + RZL_13 + RZL_14 + RZL_15 + RZL_16
cn =~ CN_01 + CN_02 + CN_03 + CN_04 + CN_05 + CN_06 + CN_07R + CN_08 + CN_09
bjc =~ BJC_01 + BJC_02 + BJC_03 + BJC_04 + BJC_05 + BJC_06 + BJC_07 + BJC_08 + BJC_09 + BJC_10 + BJC_11 + BJC_12

# Structural Model

bjc ~ d*sth + f*cn + c*rzl
sth ~ a*rzl + g*cn
cn ~ b*rzl

# Indirect effects

## Through ST
ad := a*d

## Through CN
bf := b*f

'

fit <- sem(model, data=data, se="bootstrap", bootstrap = 10000,
           ordered=c("RZL_01", "RZL_02", "RZL_03", "RZL_04", "RZL_05", 
                     "RZL_06", "RZL_07", "RZL_08", "RZL_09", "RZL_10", 
                     "RZL_11", "RZL_12", "RZL_13", "RZL_14", "RZL_15",
                     "RZL_16", "STH_01", "STH_02", "STH_03", "STH_04", 
                     "STH_05", "STH_06", "STH_07", "STH_08","BJC_01", 
                     "BJC_02", "BJC_03", "BJC_04", "BJC_05", "BJC_06", 
                     "BJC_07", "BJC_08", "BJC_09", "BJC_10", "BJC_11", 
                     "BJC_12","CN_01", "CN_02", "CN_03", "CN_04", 
                     "CN_05", "CN_06", "CN_07R", "CN_08", "CN_09"))

summary(fit)
fitMeasures(fit)
lavInspect(fit, "rsquare")
lavInspect(fit, "cor.lv")
lavInspect(fit, "coef")$theta
lavInspect(fit, what="list")
vcov(fit)


