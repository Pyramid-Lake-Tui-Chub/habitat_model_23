#### INSTALL PACKAGES ----
library(DHARMa)
library(glmmTMB)
library(compositions)
library(robustbase)
library(car)
library(effects)
library(MuMIn)
library(emmeans)
library(performance)
library(ggplot2)
library(lubridate)
library(lmtest)
library(cowplot)
theme_set(theme_cowplot())

################################################################################
####                            CORE 22                                     ####
################################################################################
#### COMPLETE DATASET MODEL ----

## COUNT BASED MODEL ##
# using effort as a covariate rather than an offset
# offset implies a multiplicative effect of effort on count
# ie, 2x more effort = 2x more count
# not necessarily true
glmm_fullnets_22 <- glmmTMB(count ~ cmecs_reduce + scale(temp_avg) + scale(log(slope + 0.9)) + (1|site),
                            offset = log(effort),
                            ziformula = ~.,
                            family = truncated_poisson,
                            data = fullnets_22)
summary(glmm_fullnets_22)

## NULL MODEL ##
# still includes effort
# not even effort!
nullnull <- glmmTMB(count ~ 1 + (1|site),
                    ziformula = ~.,
                    family = truncated_poisson,
                    data = fullnets_22)

null <- glmmTMB(count ~ 1 + (1|site),
                offset = log(effort),
                            ziformula = ~.,
                            family = truncated_poisson,
                            data = fullnets_22)

sub <- glmmTMB(count ~ cmecs_reduce + (1|site),
               offset = log(effort),
                            ziformula = ~.,
                            family = truncated_poisson,
                            data = fullnets_22)

temp <- glmmTMB(count ~ cmecs_reduce + scale(temp_avg) + (1|site),
                offset = log(effort),
                            ziformula = ~.,
                            family = truncated_poisson,
                            data = fullnets_22)

#### MODEL CHECKS ----

# likelihood ratio test
lr <- lrtest(nullnull, null, sub, temp, glmm_fullnets_22)

#### PRINT TABLES ----
library(gt)
library(furrr)
library(broom.mixed)

setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

# conditional effects
conditional <- broom::tidy(glmm_fullnets_22, component = "cond", effects = "fixed", conf.int = TRUE) %>% gt() %>% opt_stylize(style = 1, color = "gray") %>% fmt_number(decimals = 2)
gtsave(conditional, "conditional_core221.docx")

hurdle <- broom::tidy(glmm_fullnets_22, component = "zi", effects = "fixed", conf.int = TRUE) %>% gt() %>% opt_stylize(style = 1, color = "gray")%>% fmt_number(decimals = 2)
gtsave(hurdle, "hurdle_core221.docx")

lrtest <- gt(lr) %>% fmt_number(decimals = 2)
gtsave(lrtest, "lrtest_core221.docx")

################################################################################
####                         Model Performance                              ####
################################################################################

# performance pacakge metrics
# includes fake R squared
# Susan does not care about this
model_performance(glmm_fullnets_22)
              
#QQ plot residuals
glmm_fullnets_22_resids <- simulateResiduals(glmm_fullnets_22)
plot(glmm_fullnets_22_resids)

# check collinearity
check_collinearity(glmm_fullnets_22)

# ANOVA results
# use Wald chi square statistics for comparison 
# of sample distribution to null distribution
# larger less likely to be true under null
car::Anova(glmm_fullnets_22)

#### EFFECTS ----
# use just predictorEffect() to get them seperately
# and specify your focal predictor
# predictions are based only on conditional means
# cannot isolate zero portion in Effects
plot(predictorEffects(glmm_fullnets_22, type = "response"))

#### PREDICT ----

## EMMEANS: conditional marginal means and zero probability ##
# cmecs_reduce
emmeans(glmm_fullnets_22, "cmecs_reduce", component = "cmean")
emmeans(glmm_fullnets_22, "cmecs_reduce", type= "response", component = "zi")

# temp_avg
# mean at intervals of 5C
emmeans(glmm_fullnets_22, "temp_avg", component = "cmean", 
        at = list(temp_avg = c(0, 5, 10, 15, 20, 25)))
emmeans(glmm_fullnets_22, "temp_avg", type = "response", component = "zi",
        at = list(temp_avg = c(0, 5, 10, 15, 20, 25)))

# slope
# marginal mean at intervals of 0.2 increases of slope
emmeans(glmm_fullnets_22, "slope", component = "cmean", 
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))
emmeans(glmm_fullnets_22, "slope", type = "response", component = "zi", 
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))

# pairwise comparison of cmecs_reduce
emmeans(glmm_fullnets_22, pairwise ~ cmecs_reduce)

## EMMIPS ##
emmip(glmm_fullnets_22, cmecs_reduce ~ poly(temp_avg, 2), cov.reduce = range)
emmip(glmm_fullnets_22, cmecs_reduce ~ scale(log(slope + 0.9)), cov.reduce = range)
emmip(glmm_fullnets_22, cmecs_reduce ~ scale(log(effort)), cov.reduce = range)

