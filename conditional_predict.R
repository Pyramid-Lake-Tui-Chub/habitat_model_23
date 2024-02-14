#### LOAD PACKAGES ----
library(tidyverse)
library(ggeffects)
library(cowplot)
library(gridExtra)
library(grid)
library(ggplotify)

theme_set(theme_cowplot(font_size = 12))

## CONDITIONAL MODEL PREDICT ##
## Can use "terms" argument in ggpredict() to change which category predictions are estimated at

## TEMP
temp_pred <- ggpredict(m2, terms = c("temp_avg[all]"), condition = c(effort = mean(log(exp23$effort))))

predict_cpue_temp <- ggplot()+
  geom_ribbon(data=temp_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=temp_avg, y=cpue),size=1, alpha=0.6, width=0.1)+
  geom_line(data=temp_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab(" ") +
  xlab("Average Temperature (C)") +
  scale_x_continuous(n.breaks = 8)
predict_cpue_temp

## SUBSTRATE
sub_pred <- ggpredict(m2, terms = "cmecs_reduce [all]", condition = c(effort = mean(log(exp23$effort))))

# making an x column to re-order data for plots
sub_pred$x <- factor(sub_pred$x, levels=c("mud", "sand_mud", "sand", "gravelly", "gravel_mixes"))

predict_cpue_sub <- ggplot() +
  geom_point(data= sub_pred, aes(x=x, y=predicted), colour = "darkslategray4", size = 4, shape = 19)+
  geom_errorbar(data= sub_pred, aes(x=x, ymin=conf.low, ymax=conf.high), size = 0.6) +
  geom_jitter(data=subset(exp23, count != 0), aes(x=reorder(cmecs_reduce, cmecs_reduce_ord), y=cpue),size=1, alpha=0.6, width=0.1)+
  ylab(" ") +
  scale_x_discrete(name = " ", labels = c("Mud", "Sand/Mud", "Sand", "Gravelly", "Gravel Mixes"))
predict_cpue_sub

## VEGETATION COVER
veg_pred <- ggpredict(m2, terms = "veg_cover_avg[all]", condition = c(effort = mean(log(exp23$effort))))

predict_cpue_veg <- ggplot()+
  geom_ribbon(data=veg_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=veg_cover_avg, y=cpue),size=1, alpha=0.6, width=0.1)+
  geom_line(data=veg_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab(" ") +
  xlab("Average Vegetation Cover (%)") +
  scale_x_continuous(n.breaks = 10)
predict_cpue_veg

## SLOPE
slope_pred <- ggpredict(m2, terms = "slope[all]", condition = c(effort = mean(log(exp23$effort))))

predict_cpue_slope <- ggplot()+
  geom_ribbon(data=slope_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=slope, y=cpue),size=1, alpha=0.6, width=0.1)+
  geom_line(data=slope_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab("") +
  xlab("Benthic Slope (Delta(Vertical m))") +
  scale_x_continuous(n.breaks = 10)
predict_cpue_slope

# MESH SIZE
mesh_pred <- ggpredict(m2, terms = "mesh_num[n=25]", condition = c(effort = mean(log(exp23$effort))))

predict_cpue_mesh <- ggplot()+
  geom_ribbon(data=mesh_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=mesh_num, y=cpue),size=1, alpha=0.6, width=0.1)+
  geom_line(data=mesh_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 0, vjust=0.8))+
  ylab(" ") +
  xlab("Mesh Integer") +
  scale_x_continuous(breaks =c(1,2,3,4,5,6))
predict_cpue_mesh

## COMBINED FIGURE & PRINT
setwd("C:\\RCreations\\ROutput\\ModelPredictions")
png(filename = "predict_all_withPanelLabels.png", units = "in", width = 8, height = 6, res=600)

right_column <- cowplot::plot_grid(predict_cpue_veg, predict_cpue_slope, predict_cpue_mesh, ncol = 1, labels = c("C", "D", "E"), hjust = -3.5)
left_column <- cowplot::plot_grid(predict_cpue_sub, predict_cpue_temp, ncol = 1, labels = "AUTO", hjust = -3.5)
p <- cowplot::plot_grid(left_column, right_column, align = "h", rel_widths = c(1,.6),
                        ncol = 2)
y.grob <- textGrob("Fecund Tui Chub (fish/hr)", 
                   gp=gpar(col="black", fontsize=16), rot=90)
grid.arrange(arrangeGrob(p, left = y.grob))
dev.off()
