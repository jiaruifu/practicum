library(devtools)
library(MatchIt)
library(survey)

devtools::install_github("gpapadog/DAPSm")
library(DAPSm)

setwd('/Users/JiaruiFu/Desktop/practicum/practicum')
load("analysis_dat.rdata")
colnames(analysis_dat)[1] <- 'U'
colnames(analysis_dat)[6] <- 'Y'
colnames(analysis_dat)[18] <- 'Z'

reg <- subset(analysis_dat, select = -c(Y, U))
cov <- subset(analysis_dat, select = -c(Y, U, Z))
cov$Y <- analysis_dat$Y
cov$Z <- analysis_dat$Z
cov$U <- analysis_dat$U

cov$prop.scores <-  glm(Z ~ ., family = binomial,data = reg)$fitted.values

mod <- glm(Y ~ .-U, data = cov)
summary(mod)

#psmatch1 <- matchit(Z ~ .-Y,
#                    distance="logit", method = "nearest", discard = "control", data = cov)
#summary(psmatch1, standardize=TRUE)

#psmatch1.data <- match.data(psmatch1)
#psmatch1.data$Y <- as.numeric(psmatch1.data$Y)
#psmatch1.mod <- svyglm(Y ~ .,
#                       design = svydesign(~ 1, weights = ~ weights, data=psmatch1.data))
#summary(psmatch1.mod)

# hardcode weight=0.513
daps <- DAPSest(cov, out.col = 21, trt.col = 22, caliper = 0.3,
                 weight = 0.513, coords.columns = c(4, 3),
                 pairsRet = TRUE, cov.cols = 1:20, cutoff = 0.15,
                 w_tol = 0.001, coord_dist = TRUE, caliper_type = 'DAPS',
                 matching_algorithm = 'greedy')
paired <- as.data.frame(daps[["pairs"]])
# matching by DAPS
id <- paired$IDtrt
treat <- cov[id, ]
con <- cov[paired$IDcon,]
# paired t-test? ozone non-significant
t.test(treat$Y, con$Y, paired = TRUE)
# estimate = diff of means
mean(treat$Y) - mean(con$Y)


#fittreat <- glm(Y ~ Z, data = treat)
#summary(fittreat$fitted.values)

#MatchedDataMap(x = daps$pairs, trt_coords = c(3, 4), con_coords = c(7, 8))
