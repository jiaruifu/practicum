library(MatchIt)
library(dplyr)
library(ggplot2)

setwd('/Users/JiaruiFu/Desktop/practicum/practicum')
load("analysis_dat.rdata")

colnames(analysis_dat)[1] <- 'Y'
colnames(analysis_dat)[18] <- 'Z'

reg <- subset(analysis_dat, select = -c(Y))
cov <- subset(analysis_dat, select = -c(Y, Z))
cov$Y <- analysis_dat$Y
cov$Z <- analysis_dat$Z

cov$prop.scores <-  glm(Z ~ ., family = binomial,data = reg)$fitted.values
