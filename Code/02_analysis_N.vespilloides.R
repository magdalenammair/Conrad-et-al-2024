
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
# Only N. vespilloides
ves = dat %>% filter(species == "Ves")
ves$experiment = as.factor(ves$experiment)
ves.eggs = eggs %>% filter(species == "Ves")
ves.eggs$experiment = as.factor(ves.eggs$experiment)

# calculate weight in mg (instead of g) for measurements on larvae
ves$average_weight_larvae_0 = ves$average_weight_larvae_0*1000
ves$average_weight_larvae_48 = ves$average_weight_larvae_48*1000
ves$average_weight_at_dispersal = ves$average_weight_at_dispersal*1000

# Center continuous predictors
ves$size_female_parent = scale(ves$size_female_parent, scale = FALSE)
ves$size_male_parent = scale(ves$size_male_parent, scale = FALSE)
ves$carcass_weight = scale(ves$carcass_weight, scale = FALSE)

ves.eggs$size_female_parent = scale(ves.eggs$size_female_parent, scale = FALSE)
ves.eggs$size_male_parent = scale(ves.eggs$size_male_parent, scale = FALSE)
ves.eggs$carcass_weight = scale(ves.eggs$carcass_weight, scale = FALSE)

# Analysis ------

## Egg numbers -----------------
hist(ves.eggs$n_eggs)

ves_n_eggs = glm(n_eggs ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                 family = poisson,
                 data = ves.eggs)

# check residuals with DHARMa
res = simulateResiduals(ves_n_eggs)
plot(res)
plotResiduals(res, ves.eggs$treatment)
plotResiduals(res, ves.eggs$size_male_parent)
plotResiduals(res, ves.eggs$size_female_parent)
plotResiduals(res, ves.eggs$carcass_weight)
plotResiduals(res, ves.eggs$experiment)
testDispersion(res) # overdispersed

# add dispersion parameter

ves_n_eggs = glm(n_eggs ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                 family = quasipoisson,
                 data = ves.eggs)

# get results and save
(regtab_ves_n_eggs = summary(ves_n_eggs))
(r_ves_n_eggs = Anova(ves_n_eggs))

# get expected means
(ves_n_eggs_emm = emmeans(ves_n_eggs, ~ treatment, 
                          weights = "proportional", type = "response")) 
# get ratio of means with confidence intervals
(effect_ves_n_eggs = confint(pairs(ves_n_eggs_emm, reverse = TRUE)))

# plot effects
# boxplot(ves.eggs$n_eggs ~ ves.eggs$treatment)


## Egg length ----------------
hist(ves.eggs$average_egg_length) #bimodal?

ves_egg_length = lm(average_egg_length ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                    data = ves.eggs)

# check residuals
op = par(mfrow = c(2,2))
plot(ves_egg_length)
par(op)
# scale-location plot not perfect, but ok
# 2 outliers

# residuals versus predictors
plot(residuals(ves_egg_length) ~ ves.eggs$treatment)
plot(residuals(ves_egg_length) ~ ves.eggs$size_male_parent)
plot(residuals(ves_egg_length) ~ ves.eggs$size_female_parent)
plot(residuals(ves_egg_length) ~ ves.eggs$carcass_weight)
plot(residuals(ves_egg_length) ~ ves.eggs$experiment)

# check residuals with DHARMa
res = simulateResiduals(ves_egg_length)
plot(res)
# good

# get results and save
(regtab_ves_egg_length = summary(ves_egg_length))
(r_ves_egg_length = Anova(ves_egg_length))

# get expected means
(ves_egg_length_emm = emmeans(ves_egg_length, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_ves_egg_length = confint(pairs(ves_egg_length_emm)))

# plot effects
# plot(allEffects(ves_egg_length))
# boxplot(ves.eggs$average_egg_length ~ ves.eggs$treatment)

## Egg width ------------------
hist(ves.eggs$average_egg_width)

ves_egg_width = lm(average_egg_width ~ treatment + size_male_parent*size_female_parent + carcass_weight + experiment,
                   data = ves.eggs)

# check residuals
op = par(mfrow = c(2,2))
plot(ves_egg_width)
par(op)
# ok

# residuals versus predictors
plot(residuals(ves_egg_width) ~ ves.eggs$treatment)
plot(residuals(ves_egg_width) ~ ves.eggs$size_male_parent)
plot(residuals(ves_egg_width) ~ ves.eggs$size_female_parent)
plot(residuals(ves_egg_width) ~ ves.eggs$carcass_weight)
plot(residuals(ves_egg_width) ~ ves.eggs$experiment)

# check residuals with DHARMa
res = simulateResiduals(ves_egg_width)
plot(res)
# residuals higher and less variable at extremer values, but ok

# get results and save
(regtab_ves_egg_width = summary(ves_egg_width))
(r_ves_egg_width = Anova(ves_egg_width))

# get expected means
(ves_egg_width_emm = emmeans(ves_egg_width, ~ treatment, 
                             weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_ves_egg_width = confint(pairs(ves_egg_width_emm)))

# plot effects
# plot(allEffects(ves_egg_width))
# boxplot(ves.eggs$average_egg_width ~ ves.eggs$treatment)


## Hatching time -------------------------
hist(ves$average_hatching_time) 

# fit model with binomial error distribution
ves_hatch_time = lm(average_hatching_time ~ treatment + size_male_parent*size_female_parent + 
                           carcass_weight + experiment,
                         data = ves)
# check residuals
op = par(mfrow = c(2,2))
plot(ves_hatch_time)
par(op)
# residuals versus predictors
plot(residuals(ves_hatch_time) ~ ves$treatment[!is.na(ves$average_hatching_time)])
plot(residuals(ves_hatch_time) ~ ves$size_male_parent[!is.na(ves$average_hatching_time)])
plot(residuals(ves_hatch_time) ~ ves$size_female_parent[!is.na(ves$average_hatching_time)])
plot(residuals(ves_hatch_time) ~ ves$carcass_weight[!is.na(ves$average_hatching_time)])
plot(residuals(ves_hatch_time) ~ ves$experiment[!is.na(ves$average_hatching_time)])
# good

# check residuals with DHARMa
res = simulateResiduals(ves_hatch_time)
plot(res)
plotResiduals(res, ves$treatment[!is.na(ves$average_hatching_time)])
plotResiduals(res, ves$size_male_parent[!is.na(ves$average_hatching_time)])
plotResiduals(res, ves$size_female_parent[!is.na(ves$average_hatching_time)])
plotResiduals(res, ves$carcass_weight[!is.na(ves$average_hatching_time)])
plotResiduals(res, ves$experiment[!is.na(ves$average_hatching_time)])
# good

# get results and save
(regtab_ves_hatch_time = summary(ves_hatch_time))
(r_ves_hatch_time = Anova(ves_hatch_time))

# get expected means
(ves_hatch_time_emm = emmeans(ves_hatch_time, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_ves_hatch_time = confint(pairs(ves_hatch_time_emm)))

# plot effects
# plot(allEffects(ves_hatch_time))
# boxplot(ves$average_hatching_time ~ ves$treatment)


## Larval survival 48 hours ----------------------
hist(ves$n_survival_larvae_48)

# fit model with binomial error distribution
ves_surv_48 = glm(cbind(n_survival_larvae_48, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                        carcass_weight + experiment,
                      family = binomial, 
                      data = ves)
# check residuals
res = simulateResiduals(ves_surv_48)
plot(res)
plot(res$fittedResiduals~ves$treatment)
plot(res$fittedResiduals~ves$size_male_parent)
plot(res$fittedResiduals~ves$size_female_parent)
plot(res$fittedResiduals~ves$carcass_weight)
boxplot(res$fittedResiduals~ves$experiment)
# several issues
testDispersion(res) #underdispersion 


# add dispersion parameter
ves_surv_48 = glm(cbind(n_survival_larvae_48, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                    carcass_weight + experiment,
                  family = quasibinomial, 
                  data = ves)

# get results and save
(regtab_ves_surv_48 = summary(ves_surv_48))
(r_ves_surv_48 = Anova(ves_surv_48))

# get expected means
(ves_surv_48_emm = emmeans(ves_surv_48, ~ treatment, 
                           weights = "proportional", type = "response")) 
# get odds ratios with confidence intervals
(effect_ves_surv_48 = confint(pairs(ves_surv_48_emm, reverse = TRUE)))

# plot effects
# plot(allEffects(ves_surv_48))
# boxplot(ves$n_survival_larvae_48/10 ~ treatment, data = ves)


## Larval survival at dispersal ---------------------
hist(ves$n_survival_at_dispersal)

# fit model with binomial error distribution
ves_surv_dis = glm(cbind(n_survival_at_dispersal, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                         carcass_weight + experiment,
                       family = binomial, 
                       data = ves)

# check residuals
res = simulateResiduals(ves_surv_dis)
plot(res)
plot(res$fittedResiduals~ves$treatment)
plot(res$fittedResiduals~ves$size_male_parent)
plot(res$fittedResiduals~ves$size_female_parent)
plot(res$fittedResiduals~ves$carcass_weight)
plot(res$fittedResiduals~ves$experiment)
testDispersion(res)
# underdispersion issues

# add dispersion parameter
ves_surv_dis = glm(cbind(n_survival_at_dispersal, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                     carcass_weight + experiment,
                   family = quasibinomial, 
                   data = ves)

# get results and save
(regtab_ves_surv_dis = summary(ves_surv_dis))
(r_ves_surv_dis = Anova(ves_surv_dis))

# get expected means
(ves_surv_dis_emm = emmeans(ves_surv_dis, ~ treatment, 
                            weights = "proportional", type = "response")) 
# get odds ratio with confidence intervals
(effect_ves_surv_dis = confint(pairs(ves_surv_dis_emm, reverse = TRUE)))

# plot effects
# plot(allEffects(ves_surv_dis_beta))
# boxplot(ves$propdis ~ treatment, data = ves)


## Larval survival to adult ----------------------------------
hist(ves$n_survival_to_adult, breaks = 10) 

# fit model with binomial error distribution
ves_surv_adult = glm(cbind(n_survival_to_adult, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                           carcass_weight + experiment ,
                         family = binomial, 
                         data = ves)
# check residuals
res = simulateResiduals(ves_surv_adult)
plot(res)
plotResiduals(res, ves$treatment)
plotResiduals(res, ves$size_male_parent)
plotResiduals(res, ves$size_female_parent)
plotResiduals(res, ves$carcass_weight)
plotResiduals(res, ves$experiment)
# dispersion looks smaller than expected

# add dispersion parameter
ves_surv_adult = glm(cbind(n_survival_to_adult, total_n_larvae) ~ treatment + size_male_parent*size_female_parent + 
                       carcass_weight + experiment ,
                     family = quasibinomial, 
                     data = ves)

# get results and save
(regtab_ves_surv_adult = summary(ves_surv_adult))
(r_ves_surv_adult = Anova(ves_surv_adult))

# get expected means
(ves_surv_adult_emm = emmeans(ves_surv_adult, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get odds ratio with confidence intervals
(effect_ves_surv_adult = confint(pairs(ves_surv_adult_emm, reverse = TRUE)))

# plot effects
# plot(allEffects(ves_surv_adult))
# boxplot(ves$propadult ~ treatment, data = ves)

## Larval weight at hatching-----------------
hist(ves$average_weight_larvae_0)

# fit model with binomial error distribution
ves_weight_0 = lm(average_weight_larvae_0 ~ treatment + size_male_parent*size_female_parent +
                         carcass_weight + experiment,
                       data = ves)

# check residuals
op = par(mfrow = c(2,2))
plot(ves_weight_0)
par(op)
# not perfect, but ok

# check residuals with DHARMa
res = simulateResiduals(ves_weight_0)
plot(res)
plotResiduals(res, ves$treatment)
plotResiduals(res, ves$size_male_parent)
plotResiduals(res, ves$size_female_parent)
plotResiduals(res, ves$carcass_weight)
plotResiduals(res, ves$experiment)
# not perfect, but ok

# get results and save
(regtab_ves_weight_0 = summary(ves_weight_0))
(r_ves_weight_0 = Anova(ves_weight_0))

# get expected means
(ves_weight_0_emm = emmeans(ves_weight_0, ~ treatment, 
                            weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_ves_weight_0 = confint(pairs(ves_weight_0_emm)))

# # plot effects
# plot(allEffects(ves_weight_0))
# boxplot(ves$average_weight_larvae_0 ~ ves$treatment)

## Larval weight after 48 hours---------------
hist(ves$average_weight_larvae_48) 

# fit linear model
ves_weight_48 = lm(average_weight_larvae_48 ~ treatment + size_male_parent*size_female_parent + 
                          carcass_weight + experiment, 
                        data = ves)
# check residuals
op = par(mfrow = c(2,2))
plot(ves_weight_48)
par(op)
# residuals versus predictors
plot(residuals(ves_weight_48) ~ ves$treatment[!is.na(ves$average_weight_larvae_48)])
plot(residuals(ves_weight_48) ~ ves$size_male_parent[!is.na(ves$average_weight_larvae_48)])
plot(residuals(ves_weight_48) ~ ves$size_female_parent[!is.na(ves$average_weight_larvae_48)])
plot(residuals(ves_weight_48) ~ ves$carcass_weight[!is.na(ves$average_weight_larvae_48)])
plot(residuals(ves_weight_48) ~ ves$experiment[!is.na(ves$average_weight_larvae_48)])
# variance is a bit larger for medium fitted values, but ok

# check residuals with DHARMa
res = simulateResiduals(ves_weight_48)
plot(res)
plotResiduals(res, ves$treatment)
plotResiduals(res, ves$size_male_parent)
plotResiduals(res, ves$size_female_parent)
plotResiduals(res, ves$carcass_weight)
plotResiduals(res, ves$experiment)
# good

# get results and save
(regtab_ves_weight_48 = summary(ves_weight_48))
(r_ves_weight_48 = Anova(ves_weight_48))

# get expected means
(ves_weight_48_emm = emmeans(ves_weight_48, ~ treatment, 
                             weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_ves_weight_48 = confint(pairs(ves_weight_48_emm)))

# plot effects
# plot(allEffects(ves_weight_48))
# boxplot(ves$average_weight_larvae_48 ~ ves$treatment)


## Larval weight at dispersal ------------------
hist(ves$average_weight_at_dispersal) 

# fit linear model 
ves_weight_disp = lm(average_weight_at_dispersal ~ treatment + size_male_parent*size_female_parent + 
                            carcass_weight + experiment,
                          data = ves)

# check residuals
op = par(mfrow = c(2,2))
plot(ves_weight_disp)
par(op)
# residuals versus predictors
plot(residuals(ves_weight_disp) ~ ves$treatment[!is.na(ves$average_weight_at_disp)])
plot(residuals(ves_weight_disp) ~ ves$size_male_parent[!is.na(ves$average_weight_at_disp)])
plot(residuals(ves_weight_disp) ~ ves$size_female_parent[!is.na(ves$average_weight_at_disp)])
plot(residuals(ves_weight_disp) ~ ves$carcass_weight[!is.na(ves$average_weight_at_disp)])
plot(residuals(ves_weight_disp) ~ ves$experiment[!is.na(ves$average_weight_at_disp)])
# qq plot not perfect, but ok

# check residuals with DHARMa
res = simulateResiduals(ves_weight_disp)
plot(res)
plotResiduals(res, ves$treatment[!is.na(ves$average_weight_at_dispersal)])
plotResiduals(res, ves$size_male_parent[!is.na(ves$average_weight_at_dispersal)])
plotResiduals(res, ves$size_female_parent[!is.na(ves$average_weight_at_dispersal)])
plotResiduals(res, ves$carcass_weight[!is.na(ves$average_weight_at_dispersal)])
plotResiduals(res, ves$experiment[!is.na(ves$average_weight_at_dispersal)])
# not perfect, but ok

# get results and save
(regtab_ves_weight_disp = summary(ves_weight_disp))
(r_ves_weight_disp = Anova(ves_weight_disp))

# get expected means
(ves_weight_disp_emm = emmeans(ves_weight_disp, ~ treatment, 
                               weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_ves_weight_disp = confint(pairs(ves_weight_disp_emm)))


# plot effects
# plot(allEffects(ves_weight_disp))
# boxplot(ves$average_weight_at_disp ~ ves$treatment)

## Pronotum size adult offspring ------------------------------
hist(ves$average_size_adult_offspring) 

# fit model with binomial error distribution
ves_size_adult = lm(average_size_adult_offspring ~ treatment + size_male_parent*size_female_parent + 
                           carcass_weight + experiment,
                         data = ves)

# check residuals
op = par(mfrow = c(2,2))
plot(ves_size_adult)
par(op)
# residuals versus predictors
plot(residuals(ves_size_adult) ~ ves$treatment[!is.na(ves$average_size_adult_offspring)])
plot(residuals(ves_size_adult) ~ ves$size_male_parent[!is.na(ves$average_size_adult_offspring)])
plot(residuals(ves_size_adult) ~ ves$size_female_parent[!is.na(ves$average_size_adult_offspring)])
plot(residuals(ves_size_adult) ~ ves$carcass_weight[!is.na(ves$average_size_adult_offspring)])
plot(residuals(ves_size_adult) ~ ves$experiment[!is.na(ves$average_size_adult_offspring)])
#ok

# check residuals with DHARMa
res = simulateResiduals(ves_size_adult)
plot(res)
plotResiduals(res, ves$treatment[!is.na(ves$average_size_adult_offspring)])
plotResiduals(res, ves$size_male_parent[!is.na(ves$average_size_adult_offspring)])
plotResiduals(res, ves$size_female_parent[!is.na(ves$average_size_adult_offspring)])
plotResiduals(res, ves$carcass_weight[!is.na(ves$average_size_adult_offspring)])
plotResiduals(res, ves$experiment[!is.na(ves$average_size_adult_offspring)])
# not perfect, but ok

# get results and save
(regtab_ves_size_adult = summary(ves_size_adult))
(r_ves_size_adult = Anova(ves_size_adult))

# get expected means
(ves_size_adult_emm = emmeans(ves_size_adult, ~ treatment, 
                              weights = "proportional", type = "response")) 
# get difference of means with confidence intervals
(effect_ves_size_adult = confint(pairs(ves_size_adult_emm)))

# plot effects
# plot(allEffects(ves_size_adult))
# boxplot(ves$average_size_adult_offspring ~ ves$treatment)

# Save results ------
saveRDS(list(regtab_ves_n_eggs = regtab_ves_n_eggs, ves_n_eggs = r_ves_n_eggs,effect_ves_n_eggs = effect_ves_n_eggs,
             regtab_ves_egg_length = regtab_ves_egg_length, ves_egg_length = r_ves_egg_length,effect_ves_egg_length = effect_ves_egg_length,
             regtab_ves_egg_width = regtab_ves_egg_width, ves_egg_width = r_ves_egg_width,effect_ves_egg_width = effect_ves_egg_width,
             regtab_ves_hatch_time = regtab_ves_hatch_time, ves_hatch_time = r_ves_hatch_time, effect_ves_hatch_time = effect_ves_hatch_time,
             regtab_ves_surv_48 = regtab_ves_surv_48, ves_surv_48 = r_ves_surv_48, effect_ves_surv_48 = effect_ves_surv_48,
             regtab_ves_surv_dis = regtab_ves_surv_dis, ves_surv_dis = r_ves_surv_dis, effect_ves_surv_dis = effect_ves_surv_dis,
             regtab_ves_surv_adult = regtab_ves_surv_adult, ves_surv_adult = r_ves_surv_adult, effect_ves_surv_adult = effect_ves_surv_adult,
             regtab_ves_weight_0 = regtab_ves_weight_0, ves_weight_0 = r_ves_weight_0, effect_ves_weight_0 = effect_ves_weight_0,
             regtab_ves_weight_48 = regtab_ves_weight_48, ves_weight_48 = r_ves_weight_48, effect_ves_weight_48 = effect_ves_weight_48,
             regtab_ves_weight_disp = regtab_ves_weight_disp, ves_weight_disp = r_ves_weight_disp, effect_ves_weight_disp = effect_ves_weight_disp,
             regtab_ves_size_adult= regtab_ves_size_adult, ves_size_adult = r_ves_size_adult, effect_ves_size_adult = effect_ves_size_adult), 
        file = "Results/results_vespilloides.rds")

# END ########