library(devtools)

devtools::install_github("gpapadog/DAPSm")
library(DAPSm)

load("analysis_dat.rdata")
colnames(analysis_dat)[1] <- 'U'
colnames(analysis_dat)[6] <- 'Y'
colnames(analysis_dat)[18] <- 'Z'

#reg <- subset(analysis_dat, select = - c(Y) )

#power plant charatersitics asdm graph
ppprop <- subset(analysis_dat, select = c(pctCapacity_byHI, Phase2, 
                                          logHeatInput, mostlyGas, small_nunits, med_nunits, Fac.Longitude, Fac.Latitude, Z) )

ppchar <- subset(analysis_dat, select = c(pctCapacity_byHI, Phase2, 
                                          logHeatInput, mostlyGas, small_nunits, med_nunits, Fac.Longitude, Fac.Latitude, Y, Z, U) )

ppchar$prop.scores <- glm(Z ~ ., family = binomial,
                          data = ppprop)$fitted.values


#analysis_dat$prop.scores <- glm(Z ~ ., family = binomial,
#                            data = reg)$fitted.values

#ppchar$prop.scores <- glm(Z ~ ., family = binomial,data = ppchar)$fitted.values


#cov <- subset(reg, select = - c(Z) )
#cov['Y'] <- analysis_dat[6]
#cov['Z'] <- analysis_dat[18]
#cov['prop_score'] <- analysis_dat[24]


#daps <- DAPSest(ppchar, out.col = 9, trt.col = 10, caliper = 0.3,
#                weight = 0.7, coords.columns = c(7, 8),
#                pairsRet = TRUE, cov.cols = 1:6, cutoff = 0.1,
#                coord_dist = TRUE, caliper_type = 'DAPS',
#                matching_algorithm = 'greedy')

# DAPSm with extensive search for the optimal weight, power plant char
bal <- CalcDAPSWeightBalance(ppchar, weights = seq(0, 1, length.out = 40),
                             cov.cols = 1:6, trt.col = 10,
                             coords.columns = c(7, 8), caliper = 0.3,
                             matching_algorithm = 'greedy')

#bal <- CalcDAPSWeightBalance(analysis_dat, weights = seq(0, 1, length.out = 40),
#                             cov.cols = 1:21, trt.col = 23,
#                             coords.columns = c(4, 5), caliper = 0.3,
#                             matching_algorithm = 'greedy')

#graph
PlotWeightBalance(bal$balance,  weights = seq(0, 1, length.out = 40), cutoff = 0.15,
                              axis_cex = 1, mar = c(2, 2, 2, 2), inset = -0.1,
                              ylimit = NULL, leg_cex = 0.6, plot_title = '',
                              title_cex = 1, cols = NULL, xlab = 'weight (w)',
                              ylab = 'ASDM')

DAPS <- DAPSchoiceModel(ppchar, trt.col = 10, balance = bal$balance,
                        cutoff = 0.15, pairs = bal$pairs,
                        weights = seq(0, 1, length.out = 40))

daps1 <- DAPSest(ppchar, out.col = 9, trt.col = 10, caliper = 0.3,
                 weight = 0.513, coords.columns = c(7, 8),
                 pairsRet = TRUE, cov.cols = 1:6, cutoff = 0.15,
                 w_tol = 0.001, coord_dist = TRUE, caliper_type = 'DAPS',
                 matching_algorithm = 'greedy')


MatchedDataMap(x = daps1$pairs, trt_coords = c(3, 4), con_coords = c(7, 8))

MatchedDataMap(x = bal$full_pairs[[10]], trt_coords = c(3, 4),
               con_coords = c(7, 8))

# area level charateritics asdm graph

alprop <- subset(analysis_dat, select = c(mean4MaxTemp, PctUrban,PctWhite, PctBlack, PctHisp, PctHighSchool, 
                                          MedianHHInc, PctPoor, PctOccupied, PctMovedIn5, MedianHValue, 
                                          logPopPerSQM, Fac.Longitude, Fac.Latitude, Z) )

alchar <- subset(analysis_dat, select = c(mean4MaxTemp, PctUrban,PctWhite, PctBlack, PctHisp, PctHighSchool, 
                                          MedianHHInc, PctPoor, PctOccupied, PctMovedIn5, MedianHValue, 
                                          logPopPerSQM, Fac.Longitude, Fac.Latitude, Y, Z, U) )

alchar$prop.scores <- glm(Z ~ ., family = binomial,data = alprop)$fitted.values

#daps2 <- DAPSest(alchar, out.col = 15, trt.col = 16, caliper = 0.3,
#                weight = 0.7, coords.columns = c(13, 14),
#                pairsRet = TRUE, cov.cols = 1:12, cutoff = 0.1,
#                coord_dist = TRUE, caliper_type = 'DAPS',
#                matching_algorithm = 'greedy')

# DAPSm with extensive search for the optimal weight,  char
bal2 <- CalcDAPSWeightBalance(alchar, weights = seq(0, 1, length.out = 40),
                             cov.cols = 1:12, trt.col = 16,
                             coords.columns = c(13, 14), caliper = 0.3,
                             matching_algorithm = 'greedy')


PlotWeightBalance(bal2$balance,  weights = seq(0, 1, length.out = 40), cutoff = 0.15,
                  axis_cex = 1, mar = c(2, 2, 2, 2), inset = -0.1,
                  ylimit = NULL, leg_cex = 0.4, plot_title = '',
                  title_cex = 1, cols = NULL, xlab = 'weight (w)',
                  ylab = 'ASDM')

DAPS2 <- DAPSchoiceModel(alchar, trt.col = 16, balance = bal2$balance,
                        cutoff = 0.15, pairs = bal2$pairs,
                        weights = seq(0, 1, length.out = 40))



