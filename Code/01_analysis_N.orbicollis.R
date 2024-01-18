
# Load packages
library(dplyr)
library(DHARMa)
library(car)
library(effects)
library(emmeans)

# Import data
dat = read.csv("Data/raw_data_offspring.csv", stringsAsFactors = TRUE)
eggs = read.csv("Data/raw_data_eggs.csv", stringsAsFactors = TRUE)
# str(dat)
# str(eggs)

# Data preparation------
# Only N. orbicollis
orb = dat %>% filter(species == "Orb")
orb$experiment = as.factor(orb$experiment)
orb.eggs = eggs %>% filter(species == "Orb")
orb.eggs$experiment = as.factor(orb.eggs$experiment)

# calculate weight in mg (instead of g) 
orb$average_weight_larvae_0 = orb$average_weight_larvae_0*1000
orb$average_weight_larvae_48 = orb$average_weight_larvae_48*1000
orb$average_weight_at_dispersal = orb$average_weight_at_dispersal*1000

# Center continuous predictors
orb$size_female_parent = scale(orb$size_female_parent, scale = FALSE)
orb$size_male_parent = scale(orb$size_male_parent, scale = FALSE)
orb$carcass_weight = scale(orb$carcass_weight, scale = FALSE)

orb.eggs$size_female_parent = scale(orb.eggs$size_female_parent, scale = FALSE)
orb.eggs$size_male_parent = scale(orb.eggs$size_male_parent, scale = FALSE)
orb.eggs$carcass_weight = scale(orb.eggs$carcass_weight, scale = FALSE)


# Analysis ------

## Egg numbers -----------------
hist(orb.eggs$n_eggs)

orb_n_eggs = glm(n_eggs ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                 family = poisson,
                 data = orb.eggs)

# check residuals with DHARMa
res = simulateResiduals(orb_n_eggs)
plot(res)
plotResiduals(res, orb.eggs$treatment)
plotResiduals(res, orb.eggs$size_male_parent)
plotResiduals(res, orb.eggs$size_female_parent)
plotResiduals(res, orb.eggs$carcass_weight)
plotResiduals(res, orb.eggs$experiment)
testDispersion(res) # overdispersed

# add dispersion parameter

orb_n_eggs = glm(n_eggs ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                 family = quasipoisson,
                 data = orb.eggs)

# get results and save
(regtab_orb_n_eggs = summary(orb_n_eggs))
(r_orb_n_eggs = Anova(orb_n_eggs))

# get expected means
(orb_n_eggs_emm = emmeans(orb_n_eggs, ~ treatment, 
                          weights = "proportional", type = "response")) # put weights according to the number of samples per level when averaging over levels of experiment 
# get ratio of means with confidence intervals
(effect_orb_n_eggs = confint(pairs(orb_n_eggs_emm, reverse = TRUE)) )

# plot effects
# plot(allEffects(orb_n_eggs))
# boxplot(orb.eggs$n_eggs ~ orb.eggs$treatment)


## Egg length ----------------
hist(orb.eggs$average_egg_length)

orb_egg_length = lm(average_egg_length ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                 data = orb.eggs)

# check residuals
op = par(mfrow = c(2,2))
plot(orb_egg_length)
par(op)
# variance increases with fitted values, but ok

# residuals versus predictors
plot(residuals(orb_egg_length) ~ orb.eggs$treatment)
plot(residuals(orb_egg_length) ~ orb.eggs$size_male_parent)
plot(residuals(orb_egg_length) ~ orb.eggs$size_female_parent)
plot(residuals(orb_egg_length) ~ orb.eggs$carcass_weight)
plot(residuals(orb_egg_length) ~ orb.eggs$experiment)

# check residuals with DHARMa
res = simulateResiduals(orb_egg_length)
plot(res)
# good

# get results and save
(regtab_orb_egg_length = summary(orb_egg_length))
(r_orb_egg_length = Anova(orb_egg_length))

# get expected means
(orb_egg_length_emm = emmeans(orb_egg_length, ~ treatment, 
                          weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_orb_egg_length = confint(pairs(orb_egg_length_emm)))

# plot effects
# plot(allEffects(orb_egg_length))
# boxplot(orb.eggs$average_egg_length ~ orb.eggs$treatment)

## Egg width ------------------
hist(orb.eggs$average_egg_width)

orb_egg_width = lm(average_egg_width ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                    data = orb.eggs)

# check residuals
op = par(mfrow = c(2,2))
plot(orb_egg_width)
par(op)
# ok

# residuals versus predictors
plot(residuals(orb_egg_width) ~ orb.eggs$treatment)
plot(residuals(orb_egg_width) ~ orb.eggs$size_male_parent)
plot(residuals(orb_egg_width) ~ orb.eggs$size_female_parent)
plot(residuals(orb_egg_width) ~ orb.eggs$carcass_weight)
plot(residuals(orb_egg_width) ~ orb.eggs$experiment)

# check residuals with DHARMa
res = simulateResiduals(orb_egg_width)
plot(res)
# not perfect, but ok

# get results and save
(regtab_orb_egg_width = summary(orb_egg_width))
(r_orb_egg_width = Anova(orb_egg_width))

# get expected means
(orb_egg_width_emm = emmeans(orb_egg_width, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_orb_egg_width = confint(pairs(orb_egg_width_emm)))

# plot effects
# boxplot(orb.eggs$average_egg_width ~ orb.eggs$treatment)


## Hatching time -----------------------------------
hist(orb$average_hatching_time) 

# fit model with binomial error distribution
orb_hatch_time = lm(average_hatching_time ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                         data = orb)
# check residuals
op = par(mfrow = c(2,2))
plot(orb_hatch_time)
par(op)
# residuals versus predictors
plot(residuals(orb_hatch_time) ~ orb$treatment[!is.na(orb$average_hatching_time)])
plot(residuals(orb_hatch_time) ~ orb$size_male_parent[!is.na(orb$average_hatching_time)])
plot(residuals(orb_hatch_time) ~ orb$size_female_parent[!is.na(orb$average_hatching_time)])
plot(residuals(orb_hatch_time) ~ orb$carcass_weight[!is.na(orb$average_hatching_time)])
plot(residuals(orb_hatch_time) ~ orb$experiment[!is.na(orb$average_hatching_time)])
# higher variance in experiment2 than in experiment 1, higher variance in silenced than in control, but ok
# check residuals with DHARMa
res = simulateResiduals(orb_hatch_time)
plot(res)
plotResiduals(res, orb$treatment[!is.na(orb$average_hatching_time)])
plotResiduals(res, orb$size_male_parent[!is.na(orb$average_hatching_time)])
plotResiduals(res, orb$size_female_parent[!is.na(orb$average_hatching_time)])
plotResiduals(res, orb$carcass_weight[!is.na(orb$average_hatching_time)])
plotResiduals(res, orb$experiment[!is.na(orb$average_hatching_time)])
# higher variance in experiment2, but ok

# get results and save
(regtab_orb_hatch_time = summary(orb_hatch_time))
(r_orb_hatch_time = Anova(orb_hatch_time))

# get expected means
(orb_hatch_time_emm = emmeans(orb_hatch_time, ~ treatment, 
                             weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_orb_hatch_time = confint(pairs(orb_hatch_time_emm)))

# plot effects
# plot(allEffects(orb_hatch_time))
# boxplot(orb$average_hatching_time ~ orb$treatment)


## Larval survival 48 hours -----------------------------------
hist(orb$n_survival_larvae_48, breaks = 10)

# fit model with binomial error distribution
orb_surv_48 = glm(cbind(n_survival_larvae_48, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                        carcass_weight + experiment,
                      family = binomial, 
                      data = orb)
# check residuals
res = simulateResiduals(orb_surv_48)
plot(res)
# residuals versus predictors
plot(res$fittedResiduals~orb$treatment)
plot(res$fittedResiduals~orb$size_male_parent)
plot(res$fittedResiduals~orb$size_female_parent)
plot(res$fittedResiduals~orb$carcass_weight)
boxplot(res$fittedResiduals~orb$experiment)
# one outlier (sample with none of the larvae surviving) 
# consider ok

# get results and save
(regtab_orb_surv_48 = summary(orb_surv_48))
(r_orb_surv_48 = Anova(orb_surv_48))

# get expected means
(orb_surv_48_emm = emmeans(orb_surv_48, ~ treatment, 
                          weights = "proportional", type = "response")) 
# get odds ratios with confidence intervals
(effect_orb_surv_48 = confint(pairs(orb_surv_48_emm, reverse = TRUE)))

# plot effects
# plot(allEffects(orb_surv_48))
# boxplot(orb$n_survival_larvae_48/10 ~ orb$treatment)


## Larval survival at dispersal -----------------
hist(orb$n_survival_at_dispersal, breaks = 10)

# fit model with binomial error distribution
orb_surv_dis = glm(cbind(n_survival_at_dispersal, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                        carcass_weight + experiment,
                      family = binomial, 
                      data = orb)
# check residuals
res = simulateResiduals(orb_surv_dis)
plot(res)
plot(res$fittedResiduals~orb$treatment)
plot(res$fittedResiduals~orb$size_male_parent)
plot(res$fittedResiduals~orb$size_female_parent)
plot(res$fittedResiduals~orb$carcass_weight)
plot(res$fittedResiduals~orb$experiment)
# two outliers (two samples with no surviving larvae)
# consider ok

# get results and save
(regtab_orb_surv_dis = summary(orb_surv_dis))
(r_orb_surv_dis = Anova(orb_surv_dis))

# get expected means
(orb_surv_dis_emm = emmeans(orb_surv_dis, ~ treatment, 
                           weights = "proportional", type = "response")) 
# get odds ratio with confidence intervals
(effect_orb_surv_dis = confint(pairs(orb_surv_dis_emm, reverse = TRUE)))

# plot effects
# plot(allEffects(orb_surv_dis))


## Larval survival to adult ----------------------------------
hist(orb$n_survival_to_adult, breaks = 10)

# fit model with binomial error distribution
orb_surv_adult = glm(cbind(n_survival_to_adult, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                         carcass_weight + experiment ,
                        family = binomial, 
                        data = orb)
# check residuals
res = simulateResiduals(orb_surv_adult)
plot(res)
plotResiduals(res, orb$treatment)
plotResiduals(res, orb$size_male_parent)
plotResiduals(res, orb$size_female_parent)
plotResiduals(res, orb$carcass_weight)
plotResiduals(res, orb$experiment)
# not perfect - some degree of underdispersion.

# switch to quasibinomial
orb_surv_adult = glm(cbind(n_survival_to_adult, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                       carcass_weight + experiment ,
                     family = quasibinomial, 
                     data = orb)

# get results and save
(regtab_orb_surv_adult = summary(orb_surv_adult))
(r_orb_surv_adult = Anova(orb_surv_adult))

# get expected means
(orb_surv_adult_emm = emmeans(orb_surv_adult, ~ treatment, 
                            weights = "proportional", type = "response")) 
# get odds ratio with confidence intervals
(effect_orb_surv_adult = confint(pairs(orb_surv_adult_emm, reverse = TRUE)) )

# plot effects
# plot(allEffects(orb_surv_adult))
# plot(orb$n_survival_to_adult/10 ~ orb$treatment)


## Larval weight at hatching-----------------
hist(orb$average_weight_larvae_0)

# fit linear
orb_weight_0 = lm(average_weight_larvae_0 ~ treatment + size_male_parent*size_female_parent +
                           carcass_weight + experiment,
                         data = orb)

# check residuals
op = par(mfrow = c(2,2))
plot(orb_weight_0)
par(op)
# residuals versus predictors
plot(residuals(orb_weight_0) ~ orb$treatment)
plot(residuals(orb_weight_0) ~ orb$size_male_parent)
plot(residuals(orb_weight_0) ~ orb$size_female_parent)
plot(residuals(orb_weight_0) ~ orb$carcass_weight)
plot(residuals(orb_weight_0) ~ orb$experiment)
# ok

# check residuals with DHARMa
res = simulateResiduals(orb_weight_0)
plot(res)
plotResiduals(res, orb$treatment)
plotResiduals(res, orb$size_male_parent)
plotResiduals(res, orb$size_female_parent)
plotResiduals(res, orb$carcass_weight)
plotResiduals(res, orb$experiment)
# ok

# get results and save
(regtab_orb_weight_0 = summary(orb_weight_0))
(r_orb_weight_0 = Anova(orb_weight_0))

# get expected means
(orb_weight_0_emm = emmeans(orb_weight_0, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_orb_weight_0 = confint(pairs(orb_weight_0_emm)))

# plot effects
# plot(allEffects(orb_weight_0))
# boxplot(orb$average_weight_larvae_0 ~ orb$treatment)


## Larval weight after 48 hours-----------------------
hist(orb$average_weight_larvae_48) 

# fit model with binomial error distribution
orb_weight_48 = lm(average_weight_larvae_48 ~ treatment + size_male_parent*size_female_parent + 
                         carcass_weight + experiment,
                        data = orb)
# check residuals
op = par(mfrow = c(2,2))
plot(orb_weight_48)
par(op)
# residuals versus predictors
plot(residuals(orb_weight_48) ~ orb$treatment[!is.na(orb$average_weight_larvae_48)])
plot(residuals(orb_weight_48) ~ orb$size_male_parent[!is.na(orb$average_weight_larvae_48)])
plot(residuals(orb_weight_48) ~ orb$size_female_parent[!is.na(orb$average_weight_larvae_48)])
plot(residuals(orb_weight_48) ~ orb$carcass_weight[!is.na(orb$average_weight_larvae_48)])
plot(residuals(orb_weight_48) ~ orb$experiment[!is.na(orb$average_weight_larvae_48)])
# ok

# check residuals with DHARMa
res = simulateResiduals(orb_weight_48)
plot(res)
plotResiduals(res, orb$treatment[!is.na(orb$average_weight_larvae_48)])
plotResiduals(res, orb$size_male_parent[!is.na(orb$average_weight_larvae_48)])
plotResiduals(res, orb$size_female_parent[!is.na(orb$average_weight_larvae_48)])
plotResiduals(res, orb$carcass_weight[!is.na(orb$average_weight_larvae_48)])
plotResiduals(res, orb$experiment[!is.na(orb$average_weight_larvae_48)])
# good

# get results and save
(regtab_orb_weight_48 = summary(orb_weight_48))
(r_orb_weight_48 = Anova(orb_weight_48))

# get expected means
(orb_weight_48_emm = emmeans(orb_weight_48, ~ treatment, 
                            weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_orb_weight_48 = confint(pairs(orb_weight_48_emm)))

# # plot effects
# plot(allEffects(orb_weight_48))
# boxplot(orb$average_weight_larvae_48 ~ orb$treatment)


## Weight at dispersal --------------------
hist(orb$average_weight_at_dispersal, breaks = 10) 

# fit model with binomial error distribution
orb_weight_disp = lm(average_weight_at_dispersal ~ treatment + size_male_parent*size_female_parent + 
                          carcass_weight + experiment,
                        data = orb)
# check residuals
op = par(mfrow = c(2,2))
plot(orb_weight_disp)
par(op)
# residuals versus predictors
plot(residuals(orb_weight_disp) ~ orb$treatment[!is.na(orb$average_weight_at_disp)])
plot(residuals(orb_weight_disp) ~ orb$size_male_parent[!is.na(orb$average_weight_at_disp)])
plot(residuals(orb_weight_disp) ~ orb$size_female_parent[!is.na(orb$average_weight_at_disp)])
plot(residuals(orb_weight_disp) ~ orb$carcass_weight[!is.na(orb$average_weight_at_disp)])
plot(residuals(orb_weight_disp) ~ orb$experiment[!is.na(orb$average_weight_at_disp)])
# one outlier, but ok

# check residuals
res = simulateResiduals(orb_weight_disp)
plot(res)
plotResiduals(res, orb$treatment[!is.na(orb$average_weight_at_dispersal)])
plotResiduals(res, orb$size_male_parent[!is.na(orb$average_weight_at_dispersal)])
plotResiduals(res, orb$size_female_parent[!is.na(orb$average_weight_at_dispersal)])
plotResiduals(res, orb$carcass_weight[!is.na(orb$average_weight_at_dispersal)])
plotResiduals(res, orb$experiment[!is.na(orb$average_weight_at_dispersal)])
# good

# get results and save
(regtab_orb_weight_disp = summary(orb_weight_disp))
(r_orb_weight_disp = Anova(orb_weight_disp))

# get expected means
(orb_weight_disp_emm = emmeans(orb_weight_disp, ~ treatment, 
                             weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_orb_weight_disp = confint(pairs(orb_weight_disp_emm)))

# plot effects
# plot(allEffects(orb_weight_disp))
# boxplot(orb$average_weight_at_disp ~ orb$treatment)


## Pronotum size adult offspring ---------------------------
hist(orb$average_size_adult_offspring) 

# fit model with binomial error distribution
orb_size_adult = lm(average_size_adult_offspring ~ treatment + size_male_parent*size_female_parent + 
                           carcass_weight + experiment,
                         data = orb)

# check residuals
op = par(mfrow = c(2,2))
plot(orb_size_adult)
par(op)
# residuals versus predictors
plot(residuals(orb_size_adult) ~ orb$treatment[!is.na(orb$average_size_adult_offspring)])
plot(residuals(orb_size_adult) ~ orb$size_male_parent[!is.na(orb$average_size_adult_offspring)])
plot(residuals(orb_size_adult) ~ orb$size_female_parent[!is.na(orb$average_size_adult_offspring)])
plot(residuals(orb_size_adult) ~ orb$carcass_weight[!is.na(orb$average_size_adult_offspring)])
plot(residuals(orb_size_adult) ~ orb$experiment[!is.na(orb$average_size_adult_offspring)])
# check residuals with DHARMa
res = simulateResiduals(orb_size_adult)
plot(res)
plotResiduals(res, orb$treatment[!is.na(orb$average_size_adult_offspring)])
plotResiduals(res, orb$size_male_parent[!is.na(orb$average_size_adult_offspring)])
plotResiduals(res, orb$size_female_parent[!is.na(orb$average_size_adult_offspring)])
plotResiduals(res, orb$carcass_weight[!is.na(orb$average_size_adult_offspring)])
plotResiduals(res, orb$experiment[!is.na(orb$average_size_adult_offspring)])
# ok

# get results and save
(regtab_orb_size_adult = summary(orb_size_adult))
(r_orb_size_adult = Anova(orb_size_adult))

# get expected means
(orb_size_adult_emm = emmeans(orb_size_adult, ~ treatment, 
                               weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_orb_size_adult = confint(pairs(orb_size_adult_emm)))

# plot effects
# plot(allEffects(orb_size_adult))
# boxplot(orb$average_size_adult_offspring ~ orb$treatment)

# Save results ------
saveRDS(list(regtab_orb_n_eggs = regtab_orb_n_eggs, orb_n_eggs = r_orb_n_eggs, effect_orb_n_eggs = effect_orb_n_eggs,
               regtab_orb_egg_length = regtab_orb_egg_length, orb_egg_length = r_orb_egg_length, effect_orb_egg_length = effect_orb_egg_length,
               regtab_orb_egg_width = regtab_orb_egg_width, orb_egg_width = r_orb_egg_width, effect_orb_egg_width = effect_orb_egg_width,
               regtab_orb_hatch_time = regtab_orb_hatch_time, orb_hatch_time = r_orb_hatch_time, effect_orb_hatch_time = effect_orb_hatch_time,
               regtab_orb_surv_48 = regtab_orb_surv_48, orb_surv_48 = r_orb_surv_48, effect_orb_surv_48 = effect_orb_surv_48,
               regtab_orb_surv_dis = regtab_orb_surv_dis, orb_surv_dis = r_orb_surv_dis, effect_orb_surv_dis = effect_orb_surv_dis,
               regtab_orb_surv_adult = regtab_orb_surv_adult, orb_surv_adult = r_orb_surv_adult, effect_orb_surv_adult = effect_orb_surv_adult,
               regtab_orb_weight_0 = regtab_orb_weight_0, orb_weight_0 = r_orb_weight_0, effect_orb_weight_0 = effect_orb_weight_0,
               regtab_orb_weight_48 = regtab_orb_weight_48, orb_weight_48 = r_orb_weight_48, effect_orb_weight_48 = effect_orb_weight_48,
               regtab_orb_weight_disp = regtab_orb_weight_disp, orb_weight_disp = r_orb_weight_disp, effect_orb_weight_disp = effect_orb_weight_disp,
               regtab_orb_size_adult= regtab_orb_size_adult, orb_size_adult = r_orb_size_adult, effect_orb_size_adult = effect_orb_size_adult), 
        file = "Results/results_orbicollis.rds")


# END ########
