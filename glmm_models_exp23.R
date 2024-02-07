#### INSTALL PACKAGES ----
library(DHARMa)
library(tidyverse)
library(glmmTMB)
library(compositions)
library(robustbase)
library(car)
library(effects)
library(MuMIn)
library(emmeans)
library(performance)
library(ggplot2)
library(ggeffects)
library(lubridate)
library(lmtest)
library(cowplot)
library(jtools)
library(rempsyc)
library(flextable)
library(broom)
library(report)
library(effectsize)
library(gridExtra)
library(grid)
library(ggplotify)

theme_set(theme_cowplot(font_size = 8))

################################################################################
####                            EXP 23                                      ####
################################################################################
#### COMPLETE DATASET MODEL ----

## THE MODEL ##
glmm_exp23 <- glmmTMB(count ~ cmecs_reduce + scale(log(veg_cover_avg + 0.9)) + poly(temp_avg, 2) + poly(mesh_num, 2) + scale(log(slope + 0.9)) + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)
summary(glmm_exp23)

## NULL MODEL ##

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


# build subset mdoels
sub <- glmmTMB(count ~ cmecs_reduce + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)

cover <- glmmTMB(count ~ cmecs_reduce + scale(log(veg_cover_avg + 0.9)) + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)

temp <- glmmTMB(count ~ cmecs_reduce + scale(log(veg_cover_avg + 0.9)) + poly(temp_avg, 2) + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)

mesh <- glmmTMB(count ~ cmecs_reduce + scale(log(veg_cover_avg + 0.9)) + poly(temp_avg, 2) + poly(mesh_num, 2) + (1|site) + (1|site:net),
                      offset = log(effort),
                      ziformula =~.,
                      family = truncated_poisson(),
                      data = exp23)


## MODEL CHECKS ##

# likelihood ratio test
lr <- lrtest(nullnull, null, cover, sub, temp, mesh, glmm_exp23)
lr
# performance pacakge metrics
# includes fake R squared
# Susan does not care about this
model_performance(glmm_exp23)

#QQ plot residuals
glmm_exp23_resids <- simulateResiduals(glmm_exp23)
plot(glmm_exp23_resids)

# ANOVA results
car::Anova(glmm_exp23, test.statistic = "Chisq")

# check collinearity
check_collinearity(glmm_exp23)

#### EFFECTS ----
# use just predictorEffect() to get them seperately
# and specify your focal predictor
# predictions are based only on conditional means
# cannot isolate zero portion in Effects
plot(predictorEffects(glmm_exp23, type = "response"))

#### PREDICT ----

## EMMEANS: conditional marginal means and zero probability ##
# cmecs_reduce
emmeans(glmm_exp23, "cmecs_reduce", type = "response", component = "cmean")
emmeans(glmm_exp23, "cmecs_reduce", type= "response", component = "zi")

# temp_avg
# mean at intervals of 5C
emmeans(glmm_exp23, "temp_avg", component = "cmean", 
        at = list(temp_avg = c(0, 5, 10, 15, 20, 25)))
emmeans(glmm_exp23, "temp_avg", type = "response", component = "zi",
        at = list(temp_avg = c(0, 5, 10, 15, 20, 25)))
ref_grid(glmm_exp23)

# slope
# marginal mean at intervals of 0.2 increases of slope
emmeans(glmm_exp23, "slope", component = "cmean", 
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))
emmeans(glmm_exp23, "slope", type = "response", component = "zi", 
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))

# veg cover
emmeans(glmm_exp23, "veg_cover_avg", component = "cmean")
emmeans(glmm_exp23, "veg_cover_avg", type = "response", component = "zi")

# pairwise comparison of cmecs_reduce
emmeans(glmm_exp23, pairwise ~ cmecs_reduce)

# partial regression cmecs_reduce
emmip(glmm_exp23, cmecs_reduce ~ poly(temp_avg, 2), cov.reduce = range)
emmip(glmm_exp23, cmecs_reduce ~ scale(log(veg_cover_avg + 0.9)), cov.reduce = range)
emmip(glmm_exp23, cmecs_reduce ~ poly(mesh_num, 2), cov.reduce = range)
emmip(glmm_exp23, cmecs_reduce ~ scale(log(slope + 0.9)), cov.reduce = range)

## FULL MODEL PREDICT ##
#temp
temp_pred <- ggpredict(glmm_exp23, terms = c("temp_avg[n=25]"), condition = c(effort = 2))

predict_cpue_temp <- ggplot()+
  geom_ribbon(data=temp_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=temp_avg, y=cpue),size=1, alpha=0.6, width=0.1)+
  geom_line(data=temp_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab(" ") +
  xlab("Average Temperature (C)") +
  scale_x_continuous(n.breaks = 8)
predict_cpue_temp

# substrate
sub_pred <- ggpredict(glmm_exp23, terms = "cmecs_reduce", condition = c(effort = 2))
sub_pred

sub_pred$predicted <- as.numeric(sub_pred$predicted)
sub_pred$std.error <- as.numeric(sub_pred$std.error)
sub_pred <- sub_pred %>% mutate(low.2SE = predicted-2*std.error)
sub_pred <- sub_pred %>% mutate(high.2SE = predicted+2*std.error)
sub_pred <- sub_pred[,-c(3:6)]
sub_pred <- na.omit(sub_pred)

sub_pred <- as.data.frame(sub_pred) %>% pivot_longer(cols = c("predicted","low.2SE","high.2SE"),
                                                      names_to = "dummy",
                                                      values_to = "confint")
sub_pred$x <- factor(sub_pred$x, levels=c("mud", "sand_mud", "sand", "gravelly", "gravel_mixes"))


predict_cpue_sub <- ggplot() +
  geom_boxplot(data= sub_pred, aes(x=x, y=confint), fill = "grey25", alpha = 0.6) +
  geom_jitter(data=subset(exp23, count != 0), aes(x=reorder(cmecs_reduce, cmecs_reduce_ord), y=cpue),size=1, alpha=0.6, width=0.1)+
  ylab(" ") +
  scale_x_discrete(name = " ", labels = c("Mud", "Sand/Mud", "Sand", "Gravelly", "Gravel Mixes"))
predict_cpue_sub

# vegetation cover
veg_pred <- ggpredict(glmm_exp23, terms = "veg_cover_avg[n=25]", condition = c(effort = 2))
veg_pred

predict_cpue_veg <- ggplot()+
  geom_ribbon(data=veg_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=veg_cover_avg, y=cpue),size=1, alpha=0.6, width=0.1)+
  geom_line(data=veg_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab(" ") +
  xlab("Average Vegetation Cover (%)") +
  scale_x_continuous(n.breaks = 10)
predict_cpue_veg

# slope
slope_pred <- ggpredict(glmm_exp23, terms = "slope[n=25]", condition = c(effort = 2))
slope_pred

predict_cpue_slope <- ggplot()+
  geom_ribbon(data=slope_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "seagreen4")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=slope, y=cpue),size=1, alpha=0.6, width=0.1)+
  geom_line(data=slope_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8, size =12),
        axis.text.y=element_text(size=12),
        axis.title = element_text(size=14))+
  ylab("Fecund Tui Chub CPUE (Fish/Hour)") +
  xlab("Benthic Slope") +
  scale_x_continuous(n.breaks = 10)
predict_cpue_slope

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "predict_slope_color_prez.png", units = "in", width = 8, height = 6, res=600)
predict_cpue_slope
dev.off()

# mesh_size
mesh_pred <- ggpredict(glmm_exp23, terms = "mesh_num[n=25]", condition = c(effort = 3))
mesh_pred

predict_cpue_mesh <- ggplot()+
  geom_ribbon(data=mesh_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=mesh_num, y=cpue),size=1, alpha=0.6, width=0.1)+
  geom_line(data=mesh_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab(" ") +
  xlab("Mesh Integer") +
  scale_x_continuous(breaks =c(1,2,3,4,5,6))
predict_cpue_mesh

# arrange onto one page
right_column <- plot_grid(predict_cpue_veg, predict_cpue_slope, predict_cpue_mesh, ncol = 1)
left_column <- plot_grid(predict_cpue_sub, predict_cpue_temp, ncol = 1)

p <- plot_grid(left_column, right_column, align = "h", rel_widths = c(1,.6),
               ncol = 2)
y.grob <- textGrob("Fecund Tui Chub (fish/hr)", 
                   gp=gpar(col="black", fontsize=14), rot=90)
grid.arrange(arrangeGrob(p, left = y.grob))



#### PRINT TABLES ----
library(gt)
library(furrr)
library(broom.mixed)

setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

# conditional effects
conditional <- broom::tidy(glmm_exp23, component = "cond", effects = "fixed", conf.int = TRUE) %>% gt() %>% opt_stylize(style = 1, color = "gray") %>% fmt_number(decimals = 2)
gtsave(conditional, "conditional_exp23_2.docx")

hurdle <- broom::tidy(glmm_exp23, component = "zi", effects = "fixed", conf.int = TRUE) %>% gt() %>% opt_stylize(style = 1, color = "gray")%>% fmt_number(decimals = 2)
gtsave(hurdle, "hurdle_exp23_2.docx")

lrtest <- gt(lr) %>% fmt_number(decimals = 2)
gtsave(lrtest, "lrtest_exp23.docx")
