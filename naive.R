library(MatchIt)
library(dplyr)
library(ggplot2)
library(survey)

setwd('/Users/JiaruiFu/Desktop/practicum/practicum')
load("analysis_dat.rdata")

# original dataset, mean sd compare
treated <- analysis_dat[analysis_dat[18] == TRUE, ]
control <- analysis_dat[analysis_dat[18] == FALSE, ]
mean_treat <- colMeans(treated)
sd_treat <- apply(treated, 2, sd)

mean_con <- colMeans(control)
sd_con <- apply(control, 2, sd)

summary <- cbind(mean_treat, sd_treat, mean_con, sd_con)
tab <- data.frame(summary)
colnames(tab) <- c('Mean (Treated)', 'St Dev (Treated)', 'Mean (Control)', 'St Dev (Control)' )


colnames(analysis_dat)[1] <- 'Y'
colnames(analysis_dat)[6] <- 'U'
colnames(analysis_dat)[18] <- 'Z'

treat <- subset(analysis_dat, Z == 1)
con <- subset(analysis_dat, Z==0)

pscore.mod <- glm(Y ~ .-U, data = treat)

#analysis_dat$prop.scores <-  glm(Z ~ . -Y, family = binomial,data = reg)$fitted.values
library(tableone)
xvars <- colnames(analysis_dat)[-c(1, 18) ]
table1 <- CreateTableOne(vars = xvars,strata = "Z",data = analysis_dat, test = FALSE)
print(table1, smd=TRUE)

psmatch <- matchit(Z ~.-Y, distance = "logit", method = "optimal", data = analysis_dat)
summary(psmatch, standardize=TRUE)
plot(psmatch)
plot(psmatch, type="jitter", interactive = FALSE)
#plot(psmatch, type="hist")]

psmatch.data <- match.data(psmatch)

matched_treat <- subset(psmatch.data, Z==1)
matched_treat  <- subset(matched_treat , select = -c(Z, subclass))
mean_treat <- colMeans(matched_treat)
sd_treat <- apply(matched_treat, 2, sd, na.rm=TRUE)
comp <- data.frame(mean_treat, sd_treat)

# matching shows error! add more covariates
psmatch.data$Y <- as.numeric(psmatch.data$Y)
psmatch.mod <- svyglm(Y ~ Z + pctCapacity_byHI + Phase2 + Fac.Latitude + U + Fac.Longitude  + mean4MaxTemp + 
                        PctUrban + PctWhite + PctBlack + PctHisp + PctHighSchool + MedianHHInc + PctPoor + PctOccupied + PctMovedIn5
                      + MedianHValue + logHeatInput + logPopPerSQM + mostlyGas + small_nunits + med_nunits + distance + weights
                      , design = svydesign(~1, data = psmatch.data, weights = ~ weights))
summary(psmatch.mod)
confint(psmatch.mod)



