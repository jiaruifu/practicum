library(MatchIt)
library(dplyr)
library(ggplot2)
library(survey)

setwd('/Users/JiaruiFu/Desktop/practicum/practicum')
load("analysis_dat.rdata")

colnames(analysis_dat)[1] <- 'Y'
colnames(analysis_dat)[18] <- 'Z'

treat <- subset(analysis_dat, Z == 1)
con <- subset(analysis_dat, Z==0)

pscore.mod <- glm(Y ~ ., data = treat)


analysis_dat$prop.scores <-  glm(Z ~ . -Y, family = binomial,data = reg)$fitted.values

library(tableone)
xvars <- colnames(analysis_dat)[-c(1, 18) ]
table1 <- CreateTableOne(vars = xvars,strata = "Z",data = analysis_dat, test = FALSE)
print(table1, smd=TRUE)

psmatch <- matchit(Z ~.-Y, distance = "logit", method = "optimal", data = analysis_dat)
summary(psmatch, standardize=TRUE)
plot(psmatch)
plot(psmatch, type="jitter", interactive = FALSE)
plot(psmatch, type="hist")

psmatch.data <- match.data(psmatch)

psmatch.data$Y <- as.numeric(psmatch.data$Y)
psmatch.mod <- svyglm(Y ~ ., design = svydesign(ids = ~1,data = psmatch.data,weights = ~ weights), rescale=TRUE)
summary(psmatch.mod)
confint(psmatch.mod)



