library(devtools)
library(MatchIt)
library(survey)
library(survival)

devtools::install_github("gpapadog/DAPSm")
library(DAPSm)

setwd('/Users/JiaruiFu/Desktop/practicum/practicum')
load("analysis_dat.rdata")

colnames(analysis_dat)[1] <- 'Y'
colnames(analysis_dat)[18] <- 'Z'

reg <- subset(analysis_dat, select = -c(Y))
cov <- subset(analysis_dat, select = -c(Y, Z))
cov$Y <- analysis_dat$Y
cov$Z <- analysis_dat$Z
#cov$U <- analysis_dat$U

cov$prop.scores <-  glm(Z ~ ., family = binomial,data = reg)$fitted.values

# different weight method, confidence interval for ozone

# fixed weight optimal (matching failed)
daps_fixed_optimal <- DAPSest(cov, out.col = 22, trt.col = 23, caliper = 0.3,
                weight = 0.513, coords.columns = c(4, 3),
                pairsRet = TRUE, cov.cols = 1:21, cutoff = 0.15,
                coord_dist = TRUE, caliper_type = 'DAPS',w_tol = 0.001,
                matching_algorithm = 'optimal')

# fast search of optimal weight (searched weight = 0.907)
daps_search_optimal <- DAPSest(cov, out.col = 22, trt.col = 23, caliper = 0.3,
                weight = 'optimal', coords.columns = c(4, 3),
                pairsRet = TRUE, cov.cols = 1:21, cutoff = 0.15,
                w_tol = 0.001, coord_dist = TRUE, caliper_type = 'DAPS',
                matching_algorithm = 'greedy')

est_opt_search <- daps_search_optimal$est
se_opt_search <- daps_search_optimal$se
est_opt_search - 1.96*se_opt_search
est_opt_search + 1.96*se_opt_search

# extensive search of optimal weight (searched weight = 0.923)
bal <- CalcDAPSWeightBalance(cov, weights = seq(0, 1, length.out = 40),
                             cov.cols = 1:21, trt.col = 23,
                             coords.columns = c(4, 3), caliper = 0.3,
                             matching_algorithm = 'greedy')

daps_extensive_optimal <- DAPSchoiceModel(cov, trt.col = 23, balance = bal$balance,
                        cutoff = 0.15, pairs = bal$pairs,
                        weights = seq(0, 1, length.out = 40))

est_ext_search <- daps_extensive_optimal$est
se_ext_search <- daps_extensive_optimal$se
est_ext_search - 1.96*se_ext_search
est_ext_search + 1.96*se_ext_search


mod <- glm(Y ~ .,data = cov)
summary(mod)

#psmatch1 <- matchit(Z ~ .-Y,
#                    distance="logit", method = "nearest", discard = "control", data = cov)
#summary(psmatch1, standardize=TRUE)

#psmatch1.data <- match.data(psmatch1)
#psmatch1.data$Y <- as.numeric(psmatch1.data$Y)
#psmatch1.mod <- svyglm(Y ~ .,
#                       design = svydesign(~ 1, weights = ~ weights, data=psmatch1.data))
#summary(psmatch1.mod)

daps <- DAPSest(cov, out.col = 22, trt.col = 23, caliper = 0.3,
                weight = 0.513, coords.columns = c(4, 3),
                pairsRet = TRUE, cov.cols = 1:21, cutoff = 0.15,
                w_tol = 0.001, coord_dist = TRUE, caliper_type = 'DAPS',
                matching_algorithm = 'greedy')
paired <- as.data.frame(daps[["pairs"]])
id <- paired$IDtrt
treat <- cov[id, ]
con <- cov[paired$IDcon,]
#dif <- treat$Y - con$Y
#summary(dif)
#m <- mean(dif)
#sd <- sd(dif)

#comp <- lm(treat$Y ~ con$Y)
dif <- t.test(treat$Y, con$Y, paired=TRUE)
mean(treat$Y) - mean(con$Y)

confint_tabel <- rbind(est_opt_search, est_ext_search, dif$estimate)
confint_tabel <- data.frame(confint_tabel, row.names = c('Fast Search','Extensive Search' , 'Fixed Weight (Greedy)'))
confint_tabel$LB <- c(est_opt_search - 1.96*se_opt_search, est_ext_search - 1.96*se_ext_search, dif$conf.int[1])
confint_tabel$UB <- c(est_opt_search + 1.96*se_opt_search, est_ext_search + 1.96*se_ext_search, dif$conf.int[2])
colnames(confint_tabel) <- c('Estimate', 'LB', 'UB')

mean_treat <- colMeans(treat)
mean_con <- colMeans(con)
sd_treat <- apply(treat, 2, sd, na.rm =TRUE)
sd_con <- apply(con, 2, sd, na.rm =TRUE)

comp <- data.frame(mean_treat, sd_treat)

paired$distance <- ((paired$Trt.Fac.Longitude - paired$Con.Fac.Longitude)^2 + 
  (paired$Trt.Fac.Latitude - paired$Con.Fac.Latitude) ^2)^0.5

weight <- 0.513
paired$prop_score_diff <- paired$Trt.prop.scores- paired$Con.prop.scores
paired$daps_propscore <- weight * abs(paired$prop_score_diff) +(1-weight)*paired$distance


# estimate (fit another regression) vs true values of Y???
fittreat <- glm(Y ~ .-prop.scores, data = treat)
summary(fittreat$fitted.values)
summary(fittreat)

#MatchedDataMap(x = daps$pairs, trt_coords = c(3, 4), con_coords = c(7, 8))
