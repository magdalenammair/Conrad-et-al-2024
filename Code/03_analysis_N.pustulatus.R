
# Load packages
library(dplyr)
library(DHARMa)
library(car)
library(effects)

# Import data
dat = read.csv("Data/raw_data_offspring.csv", stringsAsFactors = TRUE)
eggs = read.csv("Data/raw_data_eggs.csv", stringsAsFactors = TRUE)
# str(dat)
# str(eggs)

# Data preparation------
# Only N. pustulatus
pus = dat %>% filter(species == "Pus")
pus$experiment = as.factor(pus$experiment)
pus.eggs = eggs %>% filter(species == "Pus")
pus.eggs$experiment = as.factor(pus.eggs$experiment)

# calculate weight in mg (instead of g) for measurements on larvae
pus$average_weight_larvae_0 = pus$average_weight_larvae_0*1000
pus$average_weight_larvae_48 = pus$average_weight_larvae_48*1000
pus$average_weight_at_dispersal = pus$average_weight_at_dispersal*1000

# Center continuous predictors
pus$size_female_parent = scale(pus$size_female_parent, scale = FALSE)
pus$size_male_parent = scale(pus$size_male_parent, scale = FALSE)
pus$carcass_weight = scale(pus$carcass_weight, scale = FALSE)

pus.eggs$size_female_parent = scale(pus.eggs$size_female_parent, scale = FALSE)
pus.eggs$size_male_parent = scale(pus.eggs$size_male_parent, scale = FALSE)
pus.eggs$carcass_weight = scale(pus.eggs$carcass_weight, scale = FALSE)

# Analysis ------

## Egg numbers -----------------
hist(pus.eggs$n_eggs)

pus_n_eggs = glm(n_eggs ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                 family = poisson,
                 data = pus.eggs)

# check residuals with DHARMa
res = simulateResiduals(pus_n_eggs)
plot(res)
plotResiduals(res, pus.eggs$treatment)
plotResiduals(res, pus.eggs$size_male_parent)
plotResiduals(res, pus.eggs$size_female_parent)
plotResiduals(res, pus.eggs$carcass_weight)
plotResiduals(res, pus.eggs$experiment)
testDispersion(res) # overdispersed

# add dispersion parameter

pus_n_eggs = glm(n_eggs ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                 family = quasipoisson,
                 data = pus.eggs)

# get results and save
(regtab_pus_n_eggs = summary(pus_n_eggs))
(r_pus_n_eggs = Anova(pus_n_eggs))

# get expected means
(pus_n_eggs_emm = emmeans(pus_n_eggs, ~ treatment, 
                          weights = "proportional", type = "response")) 
# get ratio of means with confidence intervals
(effect_pus_n_eggs = confint(pairs(pus_n_eggs_emm, reverse = TRUE)))

# plot effects
# plot(allEffects(pus_n_eggs))
# boxplot(pus.eggs$n_eggs ~ pus.eggs$treatment)


## Egg length ----------------
hist(pus.eggs$average_egg_length)

pus_egg_length = lm(average_egg_length ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                    data = pus.eggs)

# check residuals
op = par(mfrow = c(2,2))
plot(pus_egg_length)
par(op)
# two outliers, but ok

# residuals versus predictors
plot(residuals(pus_egg_length) ~ pus.eggs$treatment)
plot(residuals(pus_egg_length) ~ pus.eggs$size_male_parent)
plot(residuals(pus_egg_length) ~ pus.eggs$size_female_parent)
plot(residuals(pus_egg_length) ~ pus.eggs$carcass_weight)
plot(residuals(pus_egg_length) ~ pus.eggs$experiment)

# check residuals with DHARMa
res = simulateResiduals(pus_egg_length)
plot(res)
# good

# get results and save
(regtab_pus_egg_length = summary(pus_egg_length))
(r_pus_egg_length = Anova(pus_egg_length))

# get expected means
(pus_egg_length_emm = emmeans(pus_egg_length, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_pus_egg_length = confint(pairs(pus_egg_length_emm)))

# plot effects
# plot(allEffects(pus_egg_length))
# boxplot(pus.eggs$average_egg_length ~ pus.eggs$treatment)

## Egg width ------------------
hist(pus.eggs$average_egg_width)

pus_egg_width = lm(average_egg_width ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                   data = pus.eggs)

# check residuals
op = par(mfrow = c(2,2))
plot(pus_egg_width)
par(op)
# ok

# residuals versus predictors
plot(residuals(pus_egg_width) ~ pus.eggs$treatment)
plot(residuals(pus_egg_width) ~ pus.eggs$size_male_parent)
plot(residuals(pus_egg_width) ~ pus.eggs$size_female_parent)
plot(residuals(pus_egg_width) ~ pus.eggs$carcass_weight)
plot(residuals(pus_egg_width) ~ pus.eggs$experiment)

# check residuals with DHARMa
res = simulateResiduals(pus_egg_width)
plot(res)
# not perfect, but ok

# get results and save
(regtab_pus_egg_width = summary(pus_egg_width))
(r_pus_egg_width = Anova(pus_egg_width))

# get expected means
(pus_egg_width_emm = emmeans(pus_egg_width, ~ treatment, 
                             weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_pus_egg_width = confint(pairs(pus_egg_width_emm)))

# plot effects
# boxplot(pus.eggs$average_egg_width ~ pus.eggs$treatment)


## Hatching time ----------------------
hist(pus$average_hatching_time) 

# fit model with binomial error distribution
pus_hatch_time = lm(average_hatching_time ~ treatment + size_male_parent*size_female_parent + 
                           carcass_weight + experiment,
                         data = pus)

# check residuals
op = par(mfrow = c(2,2))
plot(pus_hatch_time)
par(op)
# residuals versus predictors
plot(residuals(pus_hatch_time) ~ pus$treatment[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plot(residuals(pus_hatch_time) ~ pus$size_male_parent[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plot(residuals(pus_hatch_time) ~ pus$size_female_parent[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plot(residuals(pus_hatch_time) ~ pus$carcass_weight[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plot(residuals(pus_hatch_time) ~ pus$experiment[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
# three samples with particularly high residual values: samples 5,14 and 23
# check samples
pus$average_hatching_time[c(5,14,23)]
summary(pus$average_hatching_time)
# the three samples have particularly long hatching times that are not explained by the predictors in the model

# check residuals with DHARMa
res = simulateResiduals(pus_hatch_time)
plot(res)
plotResiduals(res, pus$treatment[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plotResiduals(res, pus$size_male_parent[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plotResiduals(res, pus$size_female_parent[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plotResiduals(res, pus$carcass_weight[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plotResiduals(res, pus$experiment[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
# same as above: three samples create issues
# consider ok

# get results and save
(regtab_pus_hatch_time = summary(pus_hatch_time))
(r_pus_hatch_time = Anova(pus_hatch_time))

# get expected means
(pus_hatch_time_emm = emmeans(pus_hatch_time, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_pus_hatch_time = confint(pairs(pus_hatch_time_emm)))

# plot effects
# plot(allEffects(pus_hatch_time))
# boxplot(pus$average_hatching_time ~ pus$treatment)


## Larval survival 48 hours ---------------
hist(pus$n_survival_larvae_48)
table(pus$n_survival_larvae_48, pus$treatment) # values are either zero or 7 to 10

# fit model with binomial error distribution
pus_surv_48 = glm(cbind(n_survival_larvae_48, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                        carcass_weight + experiment,
                      family = binomial, 
                      data = pus)
# check residuals
res = simulateResiduals(pus_surv_48)
plot(res)
# issues
testZeroInflation(res)
# as expected

# split modelling task into two steps
# 1. binomial model to predict no survival (absence) vs. some survival (presence)
# 2. for samples with surviving larvae, fit binomial model on k out of n

# 1. presence-absence
pus$binary_48 = ifelse(pus$n_survival_larvae_48 == 0, 
                       pus$binary_48 <- 0,
                       pus$binary_48 <- 1)
# add dummy 0 to both groups, because model produces unrealistically high standard errors with varaince of zero in control group
dummy = data.frame(
  binary_48 = c(0,0),
  treatment = c("control", "silenced"),
  size_male_parent = rep(mean(pus$size_male_parent, na.rm = T),2),
  size_female_parent = rep(mean(pus$size_female_parent, na.rm = T),2),
  carcass_weight = rep(mean(pus$carcass_weight, na.rm = T),2), 
  experiment = c(1,1)
)
pus.dummy = rbind(pus[,c("binary_48", "treatment", "size_male_parent", "size_female_parent", "carcass_weight", "experiment")], dummy)

pus_surv_48_binary =  glm(binary_48 ~ treatment + size_male_parent*size_female_parent + 
                                carcass_weight + experiment,
                              family = binomial,
                              data = pus.dummy)
# check residuals with DHARMa
res = simulateResiduals(pus_surv_48_binary)
plot(res)
# good

# get and save results
(regtab_pus_surv_48_binary = summary(pus_surv_48_binary))
(r_pus_surv_48_binary = Anova(pus_surv_48_binary))

# get expected means
(pus_surv_48_binary_emm = emmeans(pus_surv_48_binary, ~ treatment, 
                           weights = "proportional", type = "response")) 
# get odds ratios with confidence intervals
(effect_pus_surv_48_binary = confint(pairs(pus_surv_48_binary_emm, reverse = TRUE)))

# 2. k of n model
pus.surv48 = pus[pus$n_survival_larvae_48 > 0,]

pus_surv_48 = glm(cbind(n_survival_larvae_48, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                    carcass_weight + experiment,
                  family = binomial, 
                  data = pus.surv48)

# check residuals
res = simulateResiduals(pus_surv_48)
plot(res)
# residuals versus predictors
plot(res$fittedResiduals~pus.surv48$treatment)
plot(res$fittedResiduals~pus.surv48$size_male_parent)
plot(res$fittedResiduals~pus.surv48$size_female_parent)
plot(res$fittedResiduals~pus.surv48$carcass_weight)
boxplot(res$fittedResiduals~pus.surv48$experiment)
testDispersion(res) # underdispersion

#add dispersion parameter
pus_surv_48 = glm(cbind(n_survival_larvae_48, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                    carcass_weight + experiment,
                  family = quasibinomial, 
                  data = pus.surv48)

# get results and save
(regtab_pus_surv_48 = summary(pus_surv_48))
(r_pus_surv_48 = Anova(pus_surv_48))

# get expected means
(pus_surv_48_emm = emmeans(pus_surv_48, ~ treatment, 
                           weights = "proportional", type = "response")) 
# get odds ratios with confidence intervals
(effect_pus_surv_48 = confint(pairs(pus_surv_48_emm, reverse = TRUE)))

# plot effects
# plot(allEffects(pus_surv_48))
# boxplot(pus.surv48$n_survival_larvae_48 ~ pus.surv48$treatment)


## Larval survival at dispersal ----------------------
hist(pus$n_survival_at_dispersal)
table(pus$n_survival_at_dispersal)
# same as for 48 hour survival: values are either 0 or >=5

# fit model with binomial error distribution
pus_surv_dis = glm(cbind(n_survival_at_dispersal, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                         carcass_weight + experiment,
                       family = binomial, 
                       data = pus)
# check residuals
res = simulateResiduals(pus_surv_dis)
plot(res)
# issues
testZeroInflation(res)
# as expected

# split modelling task into two steps
# 1. binomial model to predict no survival (absence) vs. some survival (presence)
# 2. for samples with surviving larvae, fit binomial model on k out of n

# 1. presence-absence
pus$binary_disp = ifelse(pus$n_survival_at_dispersal == 0, 
                       pus$binary_disp <- 0,
                       pus$binary_disp <- 1)
# add dummy 0 to both groups, because model produces unrealistically high standard errors with varaince of zero in control group
dummy.disp = data.frame(
  binary_disp = c(0,0),
  treatment = c("control", "silenced"),
  size_male_parent = rep(mean(pus$size_male_parent, na.rm = T),2),
  size_female_parent = rep(mean(pus$size_female_parent, na.rm = T),2),
  carcass_weight = rep(mean(pus$carcass_weight, na.rm = T),2), 
  experiment = c(1,1)
)
pus.dummy.disp = rbind(pus[,c("binary_disp", "treatment", "size_male_parent", "size_female_parent", "carcass_weight", "experiment")], dummy.disp)

pus_surv_dis_binary =  glm(binary_disp ~ treatment + size_male_parent*size_female_parent + 
                            carcass_weight + experiment,
                          family = binomial,
                          data = pus.dummy.disp)
# check residuals with DHARMa
res = simulateResiduals(pus_surv_dis_binary)
plot(res)
# good

# get results and save
summary(pus_surv_dis_binary)
(r_pus_surv_dis_binary = Anova(pus_surv_dis_binary))
# identical to survival at 48 hours

# get expected means
(pus_surv_dis_binary_emm = emmeans(pus_surv_dis_binary, ~ treatment, 
                            weights = "proportional", type = "response")) 
# get odds ratio with confidence intervals
(effect_pus_surv_dis_binary = confint(pairs(pus_surv_dis_binary_emm, reverse = TRUE)))

# 2. k of n model
pus.survdisp = pus[pus$n_survival_at_dispersal > 0,]

pus_surv_disp = glm(cbind(n_survival_at_dispersal, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                    carcass_weight + experiment,
                  family = binomial, 
                  data = pus.survdisp)

# check residuals
res = simulateResiduals(pus_surv_disp)
plot(res)
# residuals versus predictors
plot(res$fittedResiduals~pus.survdisp$treatment)
plot(res$fittedResiduals~pus.survdisp$size_male_parent)
plot(res$fittedResiduals~pus.survdisp$size_female_parent)
plot(res$fittedResiduals~pus.survdisp$carcass_weight)
boxplot(res$fittedResiduals~pus.survdisp$experiment)
testDispersion(res) # underdispersion

#add dispersion parameter
pus_surv_dis = glm(cbind(n_survival_at_dispersal, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                    carcass_weight + experiment,
                  family = quasibinomial, 
                  data = pus.survdisp)

# get results and save
(regtab_pus_surv_dis = summary(pus_surv_dis))
(r_pus_surv_dis = Anova(pus_surv_dis))

# get expected means
(pus_surv_dis_emm = emmeans(pus_surv_dis, ~ treatment, 
                            weights = "proportional", type = "response")) 
# get odds ratio with confidence intervals
(effect_pus_surv_dis = confint(pairs(pus_surv_dis_emm, reverse = TRUE)))

# plot effects
# plot(allEffects(pus_surv_dis))
# plot(pus$n_survival_at_dispersal ~ pus$treatment)


## Larval survival to adult ----------------------------------
hist(pus$n_survival_to_adult) 
table(pus$n_survival_to_adult)

# for samples that are NA in survival to adult and had 0 survivals at dispersal, set survival to adult to 0
pus$n_survival_to_adult[is.na(pus$n_survival_to_adult) & pus$n_survival_at_dispersal == 0] = 0

# fit model with binomial error distribution
pus_surv_adult = glm(cbind(n_survival_to_adult, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                           carcass_weight + experiment ,
                         family = binomial, 
                         data = pus)

# check residuals
res = simulateResiduals(pus_surv_adult)
plot(res)
# same pattern as for survival to 48 hours and dispersal
testZeroInflation(res) # as expected

# split modelling task into two steps
# 1. binomial model to predict no survival (absence) vs. some survival (presence)
# 2. for samples with surviving larvae, fit binomial model on k out of n

# 1. presence-absence
pus$binary_adult = ifelse(pus$n_survival_to_adult == 0, 
                         pus$binary_adult <- 0,
                         pus$binary_adult <- 1)
# add dummy 0 to both groups, because model produces unrealistically high standard errors with varaince of zero in control group
dummy.adult = data.frame(
  binary_adult = c(0,0),
  treatment = c("control", "silenced"),
  size_male_parent = rep(mean(pus$size_male_parent, na.rm = T),2),
  size_female_parent = rep(mean(pus$size_female_parent, na.rm = T),2),
  carcass_weight = rep(mean(pus$carcass_weight, na.rm = T),2), 
  experiment = c(1,1)
)
pus.dummy.adult = rbind(pus[,c("binary_adult", "treatment", "size_male_parent", "size_female_parent", "carcass_weight", "experiment")], dummy.adult)

pus_surv_adult_binary =  glm(binary_adult ~ treatment + size_male_parent*size_female_parent + 
                              carcass_weight + experiment,
                            family = binomial,
                            data = pus.dummy.adult)
# check residuals with DHARMa
res = simulateResiduals(pus_surv_adult_binary)
plot(res)
# not perfect, but ok

# get results and save
summary(pus_surv_adult_binary)
(r_pus_surv_adult_binary = Anova(pus_surv_adult_binary))

# get expected means
(pus_surv_adult_binary_emm = emmeans(pus_surv_adult_binary, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get odds ratio with confidence intervals
(effect_pus_surv_adult_binary = confint(pairs(pus_surv_adult_binary_emm, reverse = TRUE)))


# 2. k of n model
pus.survadult = pus[pus$n_survival_to_adult > 0,]

pus_surv_adult = glm(cbind(n_survival_to_adult, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                      carcass_weight + experiment,
                    family = binomial, 
                    data = pus.survadult)

# check residuals
res = simulateResiduals(pus_surv_adult)
plot(res)
# residuals versus predictors
plot(res$fittedResiduals~pus.survadult$treatment[!is.na(pus.survadult$size_female_parent) & !is.na(pus.survadult$size_male_parent) & !is.na(pus.survadult$n_survival_to_adult)])
plot(res$fittedResiduals~pus.survadult$size_male_parent[!is.na(pus.survadult$size_female_parent) & !is.na(pus.survadult$size_male_parent) & !is.na(pus.survadult$n_survival_to_adult)])
plot(res$fittedResiduals~pus.survadult$size_female_parent[!is.na(pus.survadult$size_female_parent) & !is.na(pus.survadult$size_male_parent) & !is.na(pus.survadult$n_survival_to_adult)])
plot(res$fittedResiduals~pus.survadult$carcass_weight[!is.na(pus.survadult$size_female_parent) & !is.na(pus.survadult$size_male_parent) & !is.na(pus.survadult$n_survival_to_adult)])
boxplot(res$fittedResiduals~pus.survadult$experiment[!is.na(pus.survadult$size_female_parent) & !is.na(pus.survadult$size_male_parent) & !is.na(pus.survadult$n_survival_to_adult)])
testDispersion(res) # underdispersion

#add dispersion parameter
pus_surv_adult = glm(cbind(n_survival_to_adult, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                      carcass_weight + experiment,
                    family = quasibinomial, 
                    data = pus.survadult)

# get results and save
(regtab_pus_surv_adult = summary(pus_surv_adult))
(r_pus_surv_adult = Anova(pus_surv_adult))

# get expected means
(pus_surv_adult_emm = emmeans(pus_surv_adult, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get odds ratio with confidence intervals
(effect_pus_surv_adult = confint(pairs(pus_surv_adult_emm, reverse = TRUE)))

# plot effects
# plot(allEffects(pus_surv_adult))
# plot(pus$n_survival_to_adult ~ pus$treatment)

## Larval weight at hatching---------------
hist(pus$average_weight_larvae_0)

# fit model with binomial error distribution
pus_weight_0 = lm(average_weight_larvae_0 ~ treatment + size_male_parent*size_female_parent +
                         carcass_weight + experiment,
                       data = pus)

# check residuals
op = par(mfrow = c(2,2))
plot(pus_weight_0)
par(op)
# not perfect, but ok

# check residuals with DHARMa
res = simulateResiduals(pus_weight_0)
plot(res)
plotResiduals(res, pus$treatment[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plotResiduals(res, pus$size_male_parent[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plotResiduals(res, pus$size_female_parent[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plotResiduals(res, pus$carcass_weight[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
plotResiduals(res, pus$experiment[!is.na(pus$size_male_parent)&!is.na(pus$size_female_parent)])
# good

# get results and save
(regtab_pus_weight_0 = summary(pus_weight_0))
(r_pus_weight_0 = Anova(pus_weight_0))

# get expected means
(pus_weight_0_emm = emmeans(pus_weight_0, ~ treatment, 
                            weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_pus_weight_0 = confint(pairs(pus_weight_0_emm)))

# # plot effects
# plot(allEffects(pus_weight_0))
# boxplot(pus$average_weight_larvae_0 ~ pus$treatment)

## Larval weight after 48 hours-----------------------
hist(pus$average_weight_larvae_48) 

# fit model with binomial error distribution
pus_weight_48 = lm(average_weight_larvae_48 ~ treatment + size_male_parent*size_female_parent + 
                          carcass_weight + experiment,
                        data = pus)
# check residuals
op = par(mfrow = c(2,2))
plot(pus_weight_48)
par(op)
# residuals versus predictors
plot(residuals(pus_weight_48) ~ pus$treatment[!is.na(pus$average_weight_larvae_48)])
plot(residuals(pus_weight_48) ~ pus$size_male_parent[!is.na(pus$average_weight_larvae_48)])
plot(residuals(pus_weight_48) ~ pus$size_female_parent[!is.na(pus$average_weight_larvae_48)])
plot(residuals(pus_weight_48) ~ pus$carcass_weight[!is.na(pus$average_weight_larvae_48)])
plot(residuals(pus_weight_48) ~ pus$experiment[!is.na(pus$average_weight_larvae_48)])

# check residuals with DHARMa
res = simulateResiduals(pus_weight_48)
plot(res)
plotResiduals(res, pus$treatment[!is.na(pus$average_weight_larvae_48)])
plotResiduals(res, pus$size_male_parent[!is.na(pus$average_weight_larvae_48)])
plotResiduals(res, pus$size_female_parent[!is.na(pus$average_weight_larvae_48)])
#plotResiduals(res, pus$carcass_weight[!is.na(pus$average_weight_larvae_48)])
plotResiduals(res, pus$experiment[!is.na(pus$average_weight_larvae_48)])
# variance larger in experiment 1 than in experiment 2; quantile in carcass_weight plot do not converge
# not perfect, but ok

# get results and save
(regtab_pus_weight_48 = summary(pus_weight_48))
(r_pus_weight_48 = Anova(pus_weight_48))

# get expected means
(pus_weight_48_emm = emmeans(pus_weight_48, ~ treatment, 
                             weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_pus_weight_48 = confint(pairs(pus_weight_48_emm)))

# # plot effects
# plot(allEffects(pus_weight_48))
# boxplot(pus$average_weight_larvae_48 ~ pus$treatment)


## Larval weight at dispersal -----------------
hist(pus$average_weight_at_dispersal) 

# fit model with binomial error distribution
pus_weight_disp = lm(average_weight_at_dispersal ~ treatment + size_male_parent*size_female_parent + 
                            carcass_weight + experiment,
                          data = pus)

# check residuals
op = par(mfrow = c(2,2))
plot(pus_weight_disp)
par(op)
# residuals versus predictors
plot(residuals(pus_weight_disp) ~ pus$treatment[!is.na(pus$average_weight_at_dispersal)])
plot(residuals(pus_weight_disp) ~ pus$size_male_parent[!is.na(pus$average_weight_at_dispersal)])
plot(residuals(pus_weight_disp) ~ pus$size_female_parent[!is.na(pus$average_weight_at_dispersal)])
plot(residuals(pus_weight_disp) ~ pus$carcass_weight[!is.na(pus$average_weight_at_dispersal)])
plot(residuals(pus_weight_disp) ~ pus$experiment[!is.na(pus$average_weight_at_dispersal)])

# check residuals with DHARMa
res = simulateResiduals(pus_weight_disp)
plot(res)
plotResiduals(res, pus$treatment[!is.na(pus$average_weight_at_dispersal)])
plotResiduals(res, pus$size_male_parent[!is.na(pus$average_weight_at_dispersal)])
plotResiduals(res, pus$size_female_parent[!is.na(pus$average_weight_at_dispersal)])
plotResiduals(res, pus$carcass_weight[!is.na(pus$average_weight_at_dispersal)])
plotResiduals(res, pus$experiment[!is.na(pus$average_weight_at_dispersal)])
# larger variance in experiment 1, not perfect but ok

# get results and save
(regtab_pus_weight_disp = summary(pus_weight_disp))
(r_pus_weight_disp = Anova(pus_weight_disp))

# get expected means
(pus_weight_disp_emm = emmeans(pus_weight_disp, ~ treatment, 
                               weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_pus_weight_disp = confint(pairs(pus_weight_disp_emm)))


# plot effects
# plot(allEffects(pus_weight_disp))
# boxplot(pus$average_weight_at_disp ~ pus$treatment)

## Pronotum size adult offspring -----------------
hist(pus$average_size_adult_offspring) 

# fit model with binomial error distribution
pus_size_adult = lm(average_size_adult_offspring ~ treatment + size_male_parent*size_female_parent + 
                           carcass_weight + experiment,
                         data = pus)

# check residuals
op = par(mfrow = c(2,2))
plot(pus_size_adult)
par(op)
# residuals versus predictors
plot(residuals(pus_size_adult) ~ pus$treatment[!is.na(pus$average_weight_at_dispersal)])
plot(residuals(pus_size_adult) ~ pus$size_male_parent[!is.na(pus$average_weight_at_dispersal)])
plot(residuals(pus_size_adult) ~ pus$size_female_parent[!is.na(pus$average_weight_at_dispersal)])
plot(residuals(pus_size_adult) ~ pus$carcass_weight[!is.na(pus$average_weight_at_dispersal)])
plot(residuals(pus_size_adult) ~ pus$experiment[!is.na(pus$average_weight_at_dispersal)])
# variance in experiment 2 smaller than in experiment 1
# ok

# check residuals with DHARMa
res = simulateResiduals(pus_size_adult)
plot(res)
plotResiduals(res, pus$treatment[!is.na(pus$average_size_adult_offspring)])
plotResiduals(res, pus$size_male_parent[!is.na(pus$average_size_adult_offspring)])
plotResiduals(res, pus$size_female_parent[!is.na(pus$average_size_adult_offspring)])
plotResiduals(res, pus$carcass_weight[!is.na(pus$average_size_adult_offspring)])
plotResiduals(res, pus$experiment[!is.na(pus$average_size_adult_offspring)])
# not perfect, but ok

# get results and save
(regtab_pus_size_adult = summary(pus_size_adult))
(r_pus_size_adult = Anova(pus_size_adult))

# get expected means
(pus_size_adult_emm = emmeans(pus_size_adult, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_pus_size_adult = confint(pairs(pus_size_adult_emm)))

# plot effects
# plot(allEffects(pus_size_adult))
# boxplot(pus$average_size_adult_offspring ~ pus$treatment)


# Save results ------
saveRDS(list(regtab_pus_n_eggs = regtab_pus_n_eggs, pus_n_eggs = r_pus_n_eggs,effect_pus_n_eggs = effect_pus_n_eggs,
             regtab_pus_egg_length = regtab_pus_egg_length, pus_egg_length = r_pus_egg_length,effect_pus_egg_length = effect_pus_egg_length,
             regtab_pus_egg_width = regtab_pus_egg_width, pus_egg_width = r_pus_egg_width,effect_pus_egg_width = effect_pus_egg_width,
             regtab_pus_hatch_time = regtab_pus_hatch_time, pus_hatch_time = r_pus_hatch_time, effect_pus_hatch_time = effect_pus_hatch_time,
             regtab_pus_surv_48_binary = regtab_pus_surv_48_binary, r_pus_surv_48_binary = r_pus_surv_48_binary, effect_pus_surv_48_binary = effect_pus_surv_48_binary,
             regtab_pus_surv_48 = regtab_pus_surv_48, pus_surv_48 = r_pus_surv_48, effect_pus_surv_48 = effect_pus_surv_48,
             regtab_pus_surv_dis = regtab_pus_surv_dis, pus_surv_dis = r_pus_surv_dis, effect_pus_surv_dis = effect_pus_surv_dis,
             regtab_pus_surv_adult = regtab_pus_surv_adult, pus_surv_adult = r_pus_surv_adult, effect_pus_surv_adult = effect_pus_surv_adult,
             regtab_pus_weight_0 = regtab_pus_weight_0, pus_weight_0 = r_pus_weight_0, effect_pus_weight_0 = effect_pus_weight_0,
             regtab_pus_weight_48 = regtab_pus_weight_48, pus_weight_48 = r_pus_weight_48, effect_pus_weight_48 = effect_pus_weight_48,
             regtab_pus_weight_disp = regtab_pus_weight_disp, pus_weight_disp = r_pus_weight_disp, effect_pus_weight_disp = effect_pus_weight_disp,
             regtab_pus_size_adult= regtab_pus_size_adult, pus_size_adult = r_pus_size_adult, effect_pus_size_adult = effect_pus_size_adult),  
        file = "Results/results_pustulatus.rds")

# END ########
