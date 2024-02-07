#### INSTALL PACKAGES ----
library(DHARMa)
library(glmmTMB)
library(compositions)
library(robustbase)
library(car)
library(MuMIn)
library(emmeans)
library(performance)
library(ggplot2)
library(lubridate)
library(effects)
library(lmtest)
library(cowplot)
theme_set(theme_cowplot())

################################################################################
####                            CORE 23                                     ####
################################################################################
#### COMPLETE DATASET MODEL ----

## COUNT BASED MODEL ##
# using offset for effort (instead of including as covariate)
# based on residual and AICc check
glmm_core23 <- glmmTMB(count ~ cmecs_reduce + scale(log10(veg_cover_avg + 0.9)) + poly(temp_avg, 2) + scale(log10(slope + 0.9)) + (1|site),
                       offset = log(effort),
                       ziformula = ~1,
                       family = poisson,
                       data = core23)
summary(glmm_core23)

## NULL MODEL ##
# not even effort!
nullnull <- glmmTMB(count ~ 1 + (1|site),
                    ziformula = ~1,
                    family = poisson,
                    data = core23)

# still includes effort
null <- glmmTMB(count ~ 1 + (1|site),
                offset = log(effort),
                ziformula = ~1,
                family = poisson,
                data = core23)

sub <- glmmTMB(count ~ cmecs_reduce + (1|site),
                       offset = log(effort),
                       ziformula = ~1,
                       family = poisson,
                       data = core23)

veg <- glmmTMB(count ~ cmecs_reduce + scale(log10(veg_cover_avg + 0.9)) + (1|site),
                       offset = log(effort),
                       ziformula = ~1,
                       family = poisson,
                       data = core23)

temp <- glmmTMB(count ~ cmecs_reduce + scale(log10(veg_cover_avg + 0.9)) + poly(temp_avg, 2) + (1|site),
                       offset = log(effort),
                       ziformula = ~1,
                       family = poisson,
                       data = core23)
#### MODEL CHECKS ----

# likelihood ratio test
lrtest(nullnull, null, sub, veg, temp, glmm_core23)

# performance package metrics
# includes fake R squared
# Susan does not care about this
model_performance(glmm_core23)

#QQ plot residuals
glmm_core23_resids <- simulateResiduals(glmm_core23)
plot(glmm_core23_resids)

# check collinearity
check_collinearity(glmm_core23)

# ANOVA results
# use Wald chi square statistics for comparison 
# of sample distribution to null distribution
# larger less likely to be true under null
car::Anova(glmm_core23)

#### EFFECTS ----
# use just predictorEffect() to get them seperately
# and specify your focal predictor
# predictions are based only on conditional means
# cannot isolate zero portion in Effects
plot(predictorEffects(glmm_core23, type = "response"))

#### PREDICT ----

## EMMEANS: conditional marginal means and zero probability ##
# cmecs_reduce
# just conditional
emmeans(glmm_core23, "cmecs_reduce", component = "cmean")
# conditional accounting for zero inflation
emmeans(glmm_core23, "cmecs_reduce", type = "response")

# temp_avg
# mean at intervals of 5C
emmeans(glmm_core23, "temp_avg", component = "cmean", 
        at = list(temp_avg = c(5, 10, 15, 20, 25)))
emmeans(glmm_core23, "temp_avg", type = "response", 
        at = list(temp_avg = c(5, 10, 15, 20, 25)))

# slope
# marginal mean at intervals of 0.2 increases of slope
emmeans(glmm_core23, "slope", component = "cmean", 
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))
emmeans(glmm_core23, "slope", type = "response",
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))

# veg_cover
emmeans(glmm_core23, "veg_cover_avg", component = "cmean", 
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))
emmeans(glmm_core23, "veg_cover_avg", type = "response",
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))

# pairwise comparison of cmecs_reduce
emmeans(glmm_exp23, pairwise ~ cmecs_reduce)

## EMMIPS ##


## FULL MODEL PREDICT ##
# Data manipulation for prediction 
predict_data_sub <- as.data.frame(Effect("cmecs_reduce", glmm_core23, xlevels=2, interval="prediction"))
predict_data_sub <- predict_data_sub[,-c(3)]
predict_data_sub <- predict_data_sub %>% pivot_longer(cols = c("fit","lower","upper"),
                                                      names_to = "dummy",
                                                      values_to = "confint")

predict_data_temp <- as.data.frame(Effect("temp_avg", glmm_core23, xlevels=25, interval="prediction"))
predict_data_slope <- as.data.frame(Effect("slope", 
                                           glmm_core23, xlevels=20, interval="prediction"))
predict_data_veg <- as.data.frame(Effect("veg_cover_avg", 
                                           glmm_core23, xlevels=20, interval="prediction"))
# substrate prediction plot
predict_counteffort_sub <- ggplot() +
  geom_jitter(data=subset(core23, count != 0), aes(x=cmecs_reduce, y=count),size=2, alpha=0.6, width=0.2)+
  geom_boxplot(data= predict_data_sub, aes(x=cmecs_reduce, y = confint), alpha = 0.1) +
  ylab("Number Captured") +
  scale_y_continuous(trans="log10")
predict_counteffort_sub

# temperature prediction plot
predict_counteffort_temp <- ggplot()+
  geom_jitter(data=subset(core23, count != 0), aes(x=temp_avg, y=count),size=2, alpha=0.6, width=0.2)+
  geom_ribbon(data=predict_data_temp, aes(x=temp_avg, ymin=lower, ymax=upper), alpha=0.6)+
  geom_line(data=predict_data_temp, aes(x=temp_avg, y=fit), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab("Number Captured")+
  scale_y_continuous(trans="log10")
predict_counteffort_temp

# slope prediction plot
predict_counteffort_slope <- ggplot()+
  geom_jitter(data=subset(core23, count != 0), aes(x=slope, y=count),size=2, alpha=0.6, width=0.02)+
  geom_ribbon(data=predict_data_slope, aes(x=slope, ymin=lower, ymax=upper), alpha=0.6)+
  geom_line(data=predict_data_slope, aes(x=slope, y=fit), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab("Number captured")+
  scale_y_continuous(trans="log10")
predict_counteffort_slope

# vegetation prediction plot
predict_counteffort_veg <- ggplot()+
  geom_jitter(data=subset(core23, count != 0), aes(x=veg_cover_avg, y=count),size=2, alpha=0.6, width=0.02)+
  geom_ribbon(data=predict_data_veg, aes(x=veg_cover_avg, ymin=lower, ymax=upper), alpha=0.6)+
  geom_line(data=predict_data_veg, aes(x=veg_cover_avg, y=fit), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab("Number captured")+
  scale_y_continuous(trans="log10")
predict_counteffort_veg

#### PRINT OUTPUT ?? ----
write.csv(ss, "C:\\Documents\\Pyramid_Lake\\RCreations\\csv_files\\sstest.csv", row.names=FALSE)

