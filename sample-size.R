
#################################
#                               #
#     Sample Size Planning      #
#                               #
#################################

# Here are the codes to plan sample size for our Study 1 and Study 2
# Project Repository: [Insert OSF link here]
# Before running the codes, we made correlation matrices (for both Study 1 and 2) as the basis of simulation
# Please refer to the file: cor-matrix-sample-size-planning.xlsx in our repository 

library(lavaan)
library(tidyverse)
library(simsem)
library(lme4)
library(simr)
set.seed(172020)


#################################
#                               #
#         Study 1               #
#                               #
#################################

# This codes are used to estimate sample size for study 1, where X=Religious Zeal, M1=Symbolic Threat, M2=Collective Narcissism, and Y=Belief in Jewish Conspiracy. We expect to find small-to-moderate correlations (0.15-0.2)

# Correlation between predictor, mediators, and outcome variable

cor_study1_out <- c(0.1, 0.2, 0.2)

# Correlations matrix between predictor and mediators

cormat_study1_pred <- matrix(c(1, 0.2, 0.2,
                               0.2, 1, 0.2,
                               0.2, 0.2, 1), ncol=3)

# R2 of the model
R2_study1 <- t(cor_study1_out) %*% solve(cormat_study1_pred) %*% cor_study1_out; R2_study1

# standardized reg coefficient
bmod_study1 <- solve(cormat_study1_pred) %*% cor_study1_out; bmod_study1

# Building the model

mod_study1 <- '

Y ~ 0.357*X + 0.160*M1 + 0.160*M2
M1 ~ 0.2*X
M2 ~ 0.2*X

## Correlation M1 & M2

M1 ~~ 0.2*M2

## Residuals

Y ~~ 0.933

'

# model parameter
fit_study1 <- sem(mod_study1, fixed.x=FALSE)
summary(fit_study1, standardized = TRUE, rsquare =TRUE)

# model implied covariances
cov_study1 <- fitted(fit_study1)$cov

# model implied correlation
cov2cor(cov_study1)

## Now we are ready to simulate the model

# creating analysis model
mod_study1_an <- 'Y ~ X + M1 + M2'
mod_study1_an_M1 <- 'M1 ~ X'


#simulate data with unknown sample size. Sample size from 200-700, increasing by 10 (m=50)
sim_study1 <- sim(nRep=NULL, model=mod_study1_an, n=rep(seq(200,400,10), 50), 
            generate=mod_study1, lavaanfun="sem",
            seed=565, multicore=FALSE)

# return averaged results from simulated data
summaryParam(sim_study1, detail = TRUE, alpha= 0.05)

# power curve of the model
plotPower(sim_study1, powerParam = "Y~X", alpha= 0.05)

### Accuracy in parameter estimation (AIPE)

power.study1 <- getPower(sim_study1, alpha=0.05, powerParam = "Y~M2", nVal=50:400)

power.study1 <- getPower(sim_study1)



#################################
#                               #
#         Study 2               #
#                               #
#################################

# This codes are used to estimate sample size for study 2, where X=Belief in Jewish Conspiracy, M=Belief in Vaccination Conspiracy, W=Centrality of Religion, and Y=Vaccination Intention. We expect to find moderate correlations (0.25-0.4)

# Correlation between predictor, mediator, moderator, and outcome variable

cor_study2_out <- c(-0.25, -0.4, -0.35, -0.3)

# Correlations matrix between predictor and mediators

cormat_study2_pred <- matrix(c(1, 0.2, 0.15, 0.12,
                               0.2, 1, 0.15, 0.15,
                               0.15, 0.15, 1, 0.15,
                               0.12, 0.15, 0.15, 1), ncol=4)

# R2 of the model
R2_study2 <- t(cor_study2_out) %*% solve(cormat_study2_pred) %*% cor_study2_out; R2_study2

# standardized reg coefficient
bmod_study2 <- solve(cormat_study2_pred) %*% cor_study2_out; bmod_study2

# Building the model

mod_study2 <- '

Y ~ 0.126*X + 0.306*M + 0.254*W + 0.2*XW
M ~ 0.2*X

## Residuals

Y ~~ 0.697

'

# model parameter
fit_study2 <- sem(mod_study2, fixed.x=FALSE)
summary(fit_study2, standardized = TRUE, rsquare =TRUE)

# model implied covariances
cov_study2 <- fitted(fit_study2)$cov

# model implied correlation
cov2cor(cov_study2)

## Now we are ready to simulate the model

# creating analysis model
mod_study2_an <- 'Y ~ X + M + W + XW'

#simulate data with unknown sample size. Sample size from 200-700, increasing by 10 (m=50)
sim_study2 <- sim(nRep=NULL, model=mod_study2_an, n=rep(seq(200,400,10), 50), 
                  generate=mod_study2, lavaanfun="sem",
                  seed=565, multicore=FALSE)

# return averaged results from simulated data
summaryParam(sim_study2, detail = TRUE, alpha= 0.05)

# power curve of the model
plotPower(sim_study2, powerParam = "Y~XW", alpha= 0.05)

### Accuracy in parameter estimation (AIPE)

power.study2 <- getPower(sim_study2, alpha=0.05, powerParam = "Y~XW", nVal=500:1000)

power.study2 <- getPower(sim_study2)



#################################
#                               #
#         Study 3               #
#                               #
#################################

# This codes are used to estimate sample size for study 3 (experimental between-subject design). Variables including .

# Correlation between predictor, mediator, moderator, and outcome variable

cor_study3_out <- c(0.4,
                    0.32,
                    -0.25,
                    0.25,
                    0.25,
                    0.25)

# Correlations matrix between predictor and mediators

cormat_study3_pred <- matrix(c(1,0.3,-0.2,0.12,0.12,0.12,
                               0.3,1,-0.2,0.2,0.2,0.2,
                               -0.2,0.2,1,0.15,0.1,0.1,
                               0.12,0.2,0.15,1,0.1,0.1,
                               0.12,0.2,0.1,0.1,1,0.1,
                               0.12,0.2,0.1,0.1,0.1,1), ncol=6)

# R2 of the model
R2_study3 <- t(cor_study3_out) %*% solve(cormat_study3_pred) %*% cor_study3_out; R2_study3

# standardized reg coefficient
bmod_study3 <- solve(cormat_study3_pred) %*% cor_study3_out; bmod_study3

# Building the model

mod_study3 <- '

Y ~ 0.234*X1 + 0.189*X2 + -0.226*X3 + 0.183*X1X2 + 0.171*X1X3 + 0.171*X2X3

## Residuals

Y ~~ 0.657

'

# model parameter
fit_study3 <- sem(mod_study3, fixed.x=FALSE)
summary(fit_study3, standardized = TRUE, rsquare =TRUE)

# model implied covariances
cov_study3 <- fitted(fit_study3)$cov

# model implied correlation
cov2cor(cov_study3)

## Now we are ready to simulate the model

# creating analysis model
mod_study3_an <- 'Y ~ X1 + X2 + X3 + X1X2 + X1X3 + X2X3'

#simulate data with unknown sample size. Sample size from 200-700, increasing by 10 (m=50)
sim_study3 <- sim(nRep=NULL, model=mod_study3_an, n=rep(seq(200,400,10), 50), 
                  generate=mod_study3, lavaanfun="sem",
                  seed=565, multicore=FALSE)

# return averaged results from simulated data
summaryParam(sim_study3, detail = TRUE, alpha= 0.05)

# power curve of the model
plotPower(sim_study3, powerParam = "Y~X2", alpha= 0.05)

### Accuracy in parameter estimation (AIPE)

power.study3 <- getPower(sim_study3, alpha=0.05, powerParam = "Y~X2", nVal=300:500)

power.study3 <- getPower(sim_study3, nVal=300:500)

