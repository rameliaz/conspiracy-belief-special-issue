
## Importing dataset

data2 <- read.csv("raw-data-study-2.csv")
library(lavaan)
library(psych)
library(MBESS)
library(tidyverse)
library(ggplot2)
library(semTools)


## Testing reliability

str(data2)

bjc <- data2[, c("BJC_01", "BJC_02", "BJC_03", "BJC_04", "BJC_05",
                 "BJC_06", "BJC_07", "BJC_08", "BJC_09", "BJC_10",
                 "BJC_11", "BJC_12")]

ci.reliability(data=bjc, type="omega", interval.type = "bca", B=1000)

cor <- data2[, c("COR_1", "COR_2", "COR_3", "COR_4", "COR_5")]

ci.reliability(data=cor, type="omega", interval.type = "bca", B=1000)
omega(cor, nfactors = 1, fm="ml")

vcb <- data2[, c("VBC_01", "VBC_02", "VBC_03", "VBC_04", "VBC_05",
                "VBC_06", "VBC_07", "VBC_08")]

ci.reliability(data=vcb, type="omega", interval.type = "bca", B=1000)

vh <- data2[, c("VBE_01", "VBE_02", "VBE_03R", "VBE_04", "VBE_05", "VBE_06")]
vh_r <- data2[, c("VBE_01", "VBE_02", "VBE_03R", "VBE_05", "VBE_06")]

ci.reliability(data=vh, type="omega", interval.type = "bca", B=1000)
omega(vh, nfactors = 1, fm="ml")
omega(vh_r, fm="minres")

## Mean-centering all predictors




## Demographics

describe(data2$DEMO_3)
describe(data2$DEMO_7)
data2$DEMO_5 <- factor(data2$DEMO_5, levels=c(1,2,3,4,5,6,7), 
                       labels=c("SMP", "SMA", "Diploma (D1/D2/D3)", 
                                "Sarjana (S1/SST)", "Magister (S2)",
                                "Profesi/Spesialis", "Doktoral (S3)"), ordered=T)


## Creating raw and interaction terms scores

data2 <- data2 %>% as.tibble() %>% mutate(
  bjc=BJC_01 + BJC_02 + BJC_03 + BJC_04 + BJC_05 + BJC_06 + BJC_07 + BJC_08 + BJC_09 + BJC_10 + BJC_11 + BJC_12,
  cor=COR_1 + COR_2 + COR_3 + COR_4 + COR_5,
  vcb=VBC_01 + VBC_02 + VBC_03 + VBC_04 + VBC_05 + VBC_06 + VBC_07 + VBC_08,
  vh=VBE_01 + VBE_02 + VBE_03R + VBE_05 + VBE_06,
  bjc.mc=bjc-mean(bjc),
  cor.mc=cor-mean(cor),
  bjc.cor=bjc.mc*cor.mc
)


## Model
## NOTE: Variables are treated as observational variables (not latent)

model2 <- '

## Model

vh ~ d*vcb + c*bjc
vcb ~ a*bjc + b*cor + ab*bjc.cor

## Index of moderated mediation

imm := d*ab

## Conditional indirect effects

morels := a * ab * 0.5 
lessrels := a * ab * -0.5
morel := a * ab * imm * 0.5
lessrel := a * ab * imm * -0.5

'

fit2 <- sem(model2, data = data2, se = "bootstrap", bootstrap = 10000, likelihood = "wishart")
summary(fit2)
fitMeasures(fit2)
lavInspect(fit2, "rsquare")

corr.test(data2$bjc, data2$vh, alpha=0.05, ci=T)
corr.test(data2$vcb, data2$vh, alpha=0.05, ci=T)


## Plots

ggplot(data2) +
  aes(x = DEMO_5, y = vcb) +
  geom_boxplot(fill = "#bdbdbd") +
  labs(x = "Tingkat Pendidikan", y = "Kepercayaan Konspiratif Vaksin", title = "Kepercayaan Konspiratif Vaksin Berdasarkan Tingkat Pendidikan", subtitle = "N=370") +
  ggthemes::theme_wsj()

ggplot(data2) +
  aes(x = DEMO_5, y = bjc) +
  geom_boxplot(fill = "#bdbdbd") +
  labs(x = "Tingkat Pendidikan", y = "Kepercayaan Konspiratif Yahudi", title = "Kepercayaan Konspiratif Yahudi Berdasarkan Tingkat Pendidikan", subtitle = "N=370") +
  ggthemes::theme_wsj()