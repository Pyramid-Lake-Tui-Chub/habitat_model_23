#### INSTALL PACKAGES ----
library(DHARMa)
library(tidyverse)
library(glmmTMB)
library(compositions)
library(robustbase)
library(car)
library(effects)
library(MuMIn)
library(performance)
library(lmtest)
library(effectsize)

theme_set(theme_cowplot(font_size = 12))

################################################################################
####                            EXP 23                                      ####
################################################################################

## THE COMPLETE MODEL(S) ##
glmm_exp23 <- glmmTMB(count ~ cmecs_reduce + scale(log(veg_cover_avg + 0.9)) + poly(temp_avg, 2) + poly(mesh_num, 2) + scale(log(slope + 0.9)) + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)
summary(m2)

# put the offset term in as a fixed effect, needed to work with ggpredict
# should use this model over the first one unless you have good reason
m2 <- glmmTMB(count ~ cmecs_reduce + scale(log(veg_cover_avg + 0.9)) + poly(temp_avg, 2) + poly(mesh_num, 2) + scale(log(slope + 0.9)) + offset(log(effort)) + (1|site) + (1|site:net),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)
summary(m2)

## SUBSET MODELS ##

# not even effort!
nullnull <- glmmTMB(count ~ 1 + (1|site) + (1|site:net),
                    ziformula = ~.,
                    family = truncated_poisson,
                    data = exp23)
# just effort
null <- glmmTMB(count ~ 1 + (1|site) + (1|site:net),
                offset = log(effort),
                ziformula = ~.,
                family = truncated_poisson,
                data = exp23)

# substrate
sub <- glmmTMB(count ~ cmecs_reduce + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)

# vegetation cover
cover <- glmmTMB(count ~ cmecs_reduce + scale(log(veg_cover_avg + 0.9)) + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)

# temperature
temp <- glmmTMB(count ~ cmecs_reduce + scale(log(veg_cover_avg + 0.9)) + poly(temp_avg, 2) + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)

# mesh integer
mesh <- glmmTMB(count ~ cmecs_reduce + scale(log(veg_cover_avg + 0.9)) + poly(temp_avg, 2) + poly(mesh_num, 2) + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)


## MODEL CHECKS ##

# likelihood ratio test
lr <- lrtest(nullnull, null, cover, sub, temp, mesh, m2)
lr
# performance pacakge metrics
# includes fake R squared
# Susan does not care about this
model_performance(m2)

#QQ plot residuals
m2_resids <- simulateResiduals(m2)
plot(m2_resids)

# ANOVA results
car::Anova(m2, test.statistic = "Chisq")

# check collinearity
check_collinearity(m2)

