
## Importing dataset

data2 <- read.csv("Study 2 Materials/raw-data-study-2.csv")
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

bjc_reliability_2 <- ci.reliability(data=bjc, type="omega", interval.type = "bca", B=1000)

cor <- data2[, c("COR_1", "COR_2", "COR_3", "COR_4", "COR_5")]

cor_reliability_2 <- ci.reliability(data=cor, type="omega", interval.type = "bca", B=1000)

vcb <- data2[, c("VBC_01", "VBC_02", "VBC_03", "VBC_04", "VBC_05",
                "VBC_06", "VBC_07", "VBC_08")]

vcb_reliability_2 <- ci.reliability(data=vcb, type="omega", interval.type = "bca", B=1000)

vh <- data2[, c("VBE_01", "VBE_02", "VBE_03R", "VBE_04", "VBE_05", "VBE_06")]
vh_r <- data2[, c("VBE_01", "VBE_02", "VBE_03R", "VBE_05", "VBE_06")]

vh_reliability_2 <- ci.reliability(data=vh_r, type="omega", interval.type = "bca", B=1000)
omega(vh, nfactors = 1, fm="ml")
omega(vh_r, fm="minres")


## Demographics and zero-order correlations

data2$DEMO_5 <- factor(data2$DEMO_5, levels=c(1,2,3,4,5,6,7), 
                       labels=c("SMP", "SMA", "Diploma (D1/D2/D3)", 
                                "Sarjana (S1/SST)", "Magister (S2)",
                                "Profesi/Spesialis", "Doktoral (S3)"), ordered=T)

data3$DEMO_4 <- factor(data3$DEMO_4, levels=c(1:2), labels=c("Male", "Female"), ordered=F)

describe(data2$DEMO_3) ## Participants mean/SD age 
describe(data2$DEMO_7) ## Participants' children mean/SD age
table(data2$DEMO_4) ## Participants' gender

cor <- cor%>%mutate(cor_mean=rowMeans(cor)) ## calculating row mean of centrality of religiosity
describe(cor)

vcb <- vcb%>%mutate(vcb_mean=rowMeans(vcb)) ## calculating row mean of vaccine conspiracy belief
describe(vcb)

bjc <- bjc%>%mutate(bjc_mean=rowMeans(bjc)) ## calculating row mean of belief in Jewish conspiracy
describe(bjc)

vh_r <- vh_r%>%mutate(vh_mean=rowMeans(vh_r)) ## calculating row mean of vaccine delay and refusal
describe(vh)

corplot_2 = bind_cols(vh_r$vh_mean,bjc$bjc_mean,vcb$vcb_mean,cor$cor_mean) # Binding mean score to calculate zero-order correlations

PerformanceAnalytics::chart.Correlation(corplot_2, histogram=T,pch=19) ## zero order correlation



## Creating total scores and interaction terms

data2 <- data2 %>% as_tibble() %>% mutate(
  bjc=BJC_01 + BJC_02 + BJC_03 + BJC_04 + BJC_05 + BJC_06 + BJC_07 + BJC_08 + BJC_09 + BJC_10 + BJC_11 + BJC_12,
  cor=COR_1 + COR_2 + COR_3 + COR_4 + COR_5,
  vcb=VBC_01 + VBC_02 + VBC_03 + VBC_04 + VBC_05 + VBC_06 + VBC_07 + VBC_08,
  vh=VBE_01 + VBE_02 + VBE_03R + VBE_05 + VBE_06,
  bjc.mc=bjc-mean(bjc),
  cor.mc=cor-mean(cor),
  bjc.cor=bjc.mc*cor.mc
)


## Moderated-Mediation Model
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

fit2 <- sem(model2, data = data2)
summary(fit2)
fitMeasures(fit2)
lavInspect(fit2, "rsquare")

corr.test(data2$bjc, data2$vh, alpha=0.05, ci=T)
corr.test(data2$vcb, data2$vh, alpha=0.05, ci=T)

parameters::model_parameters(fit2, standardize=T) ## obtaining standardised loadings with 95% CI


## Plots

ggstatsplot::ggbetweenstats(
  data = data2,
  x = DEMO_5,
  y = vcb,
  title = "Kepercayaan Konspiratif Terhadap Vaksin Ditinjau dari Tingkat Pendidikan",
  xlab="Tingkat Pendidikan",
  ylab="Kepercayaan Konspiratif Terhadap Vaksin"
)

ggstatsplot::ggbetweenstats(
  data = data2,
  x = DEMO_5,
  y = bjc,
  title = "Kepercayaan Konspiratif Yahudi Ditinjau dari Tingkat Pendidikan",
  xlab="Tingkat Pendidikan",
  ylab="Kepercayaan Konspiratif Yahudi"
)