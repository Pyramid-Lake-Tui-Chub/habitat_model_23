#### PLOTS FOR PUBLICATION ----

## SUBSTRATE PREDICTION AND REAL DATA: EXP23 ##

# experimental model prediction plus observations: substrate
predict_cpue_sub <- ggplot() +
  geom_boxplot(data= sub_pred, aes(x=x, y=confint), fill = "grey25", alpha = 0.6) +
  geom_jitter(data=subset(exp23, count != 0), aes(x=reorder(cmecs_reduce, cmecs_reduce_ord), y=cpue),size=2, alpha=0.6, width=0.2)+
  ylab("Fecund Tui Chub CPUE (Fish/Hour)") +
  scale_x_discrete(name = " ", labels = c("Mud", "Sand/Mud", "Sand", "Gravelly", "Gravel Mixes"))
predict_cpue_sub

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "predict_sub.png", units = "in", width = 8, height = 6, res=600)
predict_cpue_sub
dev.off()

# experimental model prediction plus observations: temperature
temp_pred <- ggpredict(glmm_exp23, terms = c("temp_avg[n=25]"), condition = c(effort = 4))

predict_cpue_temp <- ggplot()+
  geom_ribbon(data=temp_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=temp_avg, y=cpue),size=2, alpha=0.6, width=0.2)+
  geom_line(data=temp_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab("Fecund Tui Chub CPUE (Fish/Hour)") +
  xlab("Average Temperature (C)") +
  scale_x_continuous(n.breaks = 8)
predict_cpue_temp

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "predict_temp.png", units = "in", width = 8, height = 6, res=600)
predict_cpue_temp
dev.off()

# experimental model prediction plus observations: temperature
predict_cpue_veg <- ggplot()+
  geom_ribbon(data=veg_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "seagreen4")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=veg_cover_avg, y=cpue),size=2, alpha=0.6, width=0.2)+
  geom_line(data=veg_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab("Fecund Tui Chub CPUE (Fish/Hour)") +
  xlab("Average Vegetation Cover (%)") +
  scale_x_continuous(n.breaks = 10)
predict_cpue_veg

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "predict_veg.png", units = "in", width = 8, height = 6, res=600)
predict_cpue_veg
dev.off()

# combined!
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "predict_all.png", units = "in", width = 8, height = 6, res=600)
right_column <- plot_grid(predict_cpue_veg, predict_cpue_slope, predict_cpue_mesh, ncol = 1)
left_column <- plot_grid(predict_cpue_sub, predict_cpue_temp, ncol = 1)
p <- plot_grid(left_column, right_column, align = "h", rel_widths = c(1,.6),
               ncol = 2)
y.grob <- textGrob("Fecund Tui Chub (fish/hr)", 
                   gp=gpar(col="black", fontsize=10), rot=90)
grid.arrange(arrangeGrob(p, left = y.grob))
dev.off()

################################################################################
## DEPTH VS. TEMP ##

# Maybe want to spruce
temp_depth <- ggplot(exp23, aes(x=temp_avg, y=med_depth_m)) + 
  geom_point()+
  #geom_smooth(method=loess) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_y_reverse() +
  scale_color_brewer(palette = "Spectral") +
  labs(x= "Average Temperature (C)", y= "Depth")
temp_depth

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "depthvtemp_exp23.png", units = "in", width = 8, height = 6, res=600)
temp_depth
dev.off()

#### MEDIAN DEPTH: 2022, prove ability to drop ----
depth_cpe_22 <- ggplot(core22, aes(x=net, y=cpue)) + 
  geom_boxplot(fill="grey25", alpha = 0.7)+
  scale_x_discrete(labels = c("<10 m","10-20 m", "20-30 m")) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs(x= " ", y= "Fecund Tui Chub CPUE (fish/hour)")
depth_cpe_22

# export
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "2022_depth_catch.png", units = "in", width = 8, height = 6, res=600)
depth_cpe_22
dev.off()

#### OVERALL SCUBA PERCENTAGES ----
sub_overall <- ggplot(sum_scuba_verif, aes(x= sub_class, y = substrate_avg)) +
  geom_bar(stat = "identity", fill = "indianred3", alpha = 0.7) +
  scale_x_discrete(name = " ", labels = c("Silt", "Clay", "Sand", "Small Gravel", "Gravel", "Cobble", "Boulder", "Bedrock")) +
  scale_y_continuous(name = "% Substrate")+
  coord_flip()
sub_overall

setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "SCUBA_overall_color.png", units = "in", width = 8, height = 6, res=600)
sub_overall
dev.off()

#### SLOPE ----
slope_pred <- ggpredict(glmm_exp23, terms = "slope[n=25]", condition = c(effort = 2))
slope_pred

predict_cpue_slope <- ggplot()+
  geom_ribbon(data=slope_pred, aes(x=x, ymin=conf.low, ymax=conf.high), alpha=0.6, fill = "grey25")+
  geom_jitter(data=subset(exp23, count != 0), aes(x=slope, y=cpue),size=2, alpha=0.6, width=0.2)+
  geom_line(data=slope_pred, aes(x=x, y=predicted), lwd=1)+
  theme(axis.text.x = element_text(angle = 45, vjust=0.8))+
  ylab("Fecund Tui Chub CPUE (Fish/Hour)") +
  xlab("Slope") +
  scale_x_continuous(n.breaks = 10)
predict_cpue_slope

setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "slope_predict.png", units = "in", width = 8, height = 6, res=600)
predict_cpue_slope
dev.off()

#### JULIAN DAY
date <- ggplot(core22, aes(x=date_time_pull, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm, color="grey15") +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "", y= "CPUE of Fecund Fish (fish/hour)")
date

setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "date.png", units = "in", width = 8, height = 6, res=600)
date
dev.off()
#### VEGETATION VS. RESPONSE ALL YEARS ----
library(gridExtra)
# column just for this
habData_master$scheme <- paste(habData_master$net_type, habData_master$year)

# New facet label names for scheme variable
scheme_labs <- c("AFS 2022", "AFS 2023", "CUSTOM 2023")
names(scheme_labs) <- c("CORE 2022", "CORE 2023", "EXP 2023")
log(as.numeric(cpue) +1)
veg_cover_all3 <- ggplot(habData_master, aes(x=as.numeric(veg_cover_avg), y=log(as.numeric(cpue) +1))) + 
  geom_jitter(size = 1.2, width = 0.2, alpha = 0.7) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_y_continuous(limits= c(0,3))+
  labs(x= "Percent Vegetation Cover (%)", y= "Fecund Fish CPUE (fish/hour)") +
  facet_wrap(~scheme, ncol = 1, labeller = labeller(scheme=scheme_labs))
veg_cover_all3

setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\ROutput")

png(filename = "veg_cover_all.png", units = "in", width = 7, height = 6, res=600)
veg_cover_all3
dev.off()
