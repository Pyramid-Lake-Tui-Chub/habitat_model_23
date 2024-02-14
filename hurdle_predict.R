#### INSTALL PACKAGES ----
library(tidyverse)
library(ggeffects)
library(gridExtra)
library(grid)

theme_set(theme_cowplot(font_size = 12))

## HURDLE MODEL PREDICT ##

## TEMPERATURE ##
temp_pred0 <- ggpredict(m2, terms = c("temp_avg[all]"), type = "zi_prob", condition=c(effort = mean(log(exp23$effort))))

predict_cpue_temp0 <- ggplot()+
  geom_ribbon(data=temp_pred0, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_line(data=temp_pred0, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  scale_y_continuous(limits = c(0,1))+
  ylab(" ") +
  xlab("Average Temperature (C)") +
  scale_x_continuous(n.breaks = 8)
predict_cpue_temp0

## SUBSTRATE ##
sub_pred0 <- ggpredict(m2, terms = "cmecs_reduce[all]", type = "zi_prob", condition = c(effort = mean(log(exp23$effort))))

# making an x column to re-order data for plots
sub_pred0$x <- factor(sub_pred$x, levels=c("mud", "sand_mud", "sand", "gravelly", "gravel_mixes"))

predict_cpue_sub0 <- ggplot() +
  geom_point(data= sub_pred0, aes(x=x, y=predicted), colour = "darkslategray4", size = 4, shape = 19)+
  geom_errorbar(data= sub_pred0, aes(x=x, ymin=conf.low, ymax=conf.high), size = 0.6) +
  scale_y_continuous(limits = c(0,1))+
  ylab(" ") +
  scale_x_discrete(name = " ", labels = c("Mud", "Sand/Mud", "Sand", "Gravelly", "Gravel Mixes"))
predict_cpue_sub0

## VEGETATION COVER ##
veg_pred0 <- ggpredict(m2, terms = "veg_cover_avg[all]", type = "zi_prob", condition = c(effort = mean(log(exp23$effort))))

predict_cpue_veg0 <- ggplot()+
  geom_ribbon(data=veg_pred0, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_line(data=veg_pred0, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  scale_y_continuous(limits = c(0,1))+
  ylab(" ") +
  xlab("Average Vegetation Cover (%)") +
  scale_x_continuous(n.breaks = 10)
predict_cpue_veg0

## SLOPE ##
slope_pred0 <- ggpredict(m2, terms = "slope[all]", type = "zi_prob", condition = c(effort = mean(log(exp23$effort))))

predict_cpue_slope0 <- ggplot()+
  geom_ribbon(data=slope_pred0, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_line(data=slope_pred0, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  scale_y_continuous(limits = c(0,1))+
  ylab("") +
  xlab("Benthic Slope (Delta(Vertical m))") +
  scale_x_continuous(n.breaks = 10)
predict_cpue_slope0

## MESH SIZE ##
mesh_pred0 <- ggpredict(m2, terms = "mesh_num[n=25]", type = "zi_prob", condition = c(effort = mean(log(exp23$effort))))

predict_cpue_mesh0 <- ggplot()+
  geom_ribbon(data=mesh_pred0, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_line(data=mesh_pred0, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 0, vjust=0.8))+
  scale_y_continuous(limits = c(0,1))+
  ylab(" ") +
  xlab("Mesh Integer") +
  scale_x_continuous(breaks =c(1,2,3,4,5,6))
predict_cpue_mesh0

#### PRINT FIGURES ----
setwd("C:\\RCreations\\ROutput\\ModelPredictions")

## COMBINES ##
png(filename = "predict0_withPanelLabels.png", units = "in", width = 8, height = 6, res=600)

right_column <- cowplot::plot_grid(predict_cpue_veg0, predict_cpue_slope0, predict_cpue_mesh0, ncol = 1, labels = c("C", "D", "E"), vjust = 1.1, hjust = -6)
left_column <- cowplot::plot_grid(predict_cpue_sub0, predict_cpue_temp0, ncol = 1, labels = "AUTO", vjust = 1.1, hjust = -6)

p <- plot_grid(left_column, right_column, align = "h", rel_widths = c(1,.6), ncol = 2)
y.grob <- textGrob("Probability of 0 Fecund Tui Chub (fish/hr)", gp=gpar(col="black", fontsize=14), rot=90)

grid.arrange(arrangeGrob(p, left = y.grob))

dev.off()