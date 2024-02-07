#### INSTALL PACKAGES ----
library(tidyverse)
library(tidyr)
library(plotrix)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(dplyr)
theme_set(theme_cowplot())

################################################################################
####                    CORE 2022 OPTIMAL PREDICTORS                        ####
################################################################################
#### CONTINUOUS PREDICTORS ----
################################################################################
#### VEGETATION HEIGHT AVERAGE ----
veg_height <- ggplot(fullnets_22, aes(x=veg_height_avg, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Average Vegetation Height (m)", y= "CPUE of Fecund Fish (fish/hour)")
veg_height

################################################################################
#### VEGETATION COVER AVERAGE ----
veg_cover <- ggplot(fullnets_22, aes(x=veg_cover_avg, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Average Vegetation Cover (%)", y= "CPUE of Fecund Fish (fish/hour)")
veg_cover

################################################################################
#### SLOPE ----
slope <- ggplot(fullnets_22, aes(x=slope, y=cpue)) + 
  geom_point()+
  geom_smooth() +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Slope", y= "CPUE of Fecund Fish (fish/hour)")
slope

################################################################################
#### MEDIAN DEPTH ----
med_depth <- ggplot(core22, aes(x=med_depth_m, y=cpue)) + 
  geom_point()+
  #geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Median Net Depth (m)", y= "Fecund Tui Chub CPUE (fish/hour)")
med_depth

################################################################################
#### TEMPERATURE AVERAGE ----
temp_avg <- ggplot(fullnets_22, aes(x=temp_avg, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Average Temperature (C)", y= "CPUE of Fecund Fish (fish/hour)")
temp_avg

################################################################################
#### TEMPERATURE VS. Depth ----
temp_depth <- ggplot(fullnets_22, aes(x=temp_avg, y=med_depth_m)) + 
  geom_point()+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_y_reverse() +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Average Temperature (C)", y= "Depth (m)")
temp_depth
################################################################################
#### CMECS REDUCED CATEGORIES ----
cmecs_reduce <- ggplot(fullnets_22, aes(x=cmecs_reduce, y=cpue)) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "CMECS Reduced Group", y= "CPUE of Fecund Fish (fish/hour)")
cmecs_reduce

################################################################################
####                     SUB-OPTIMAL PREDICTORS                             ####
################################################################################
#### CONTINUOUS PREDICTORS ----
# SAND
sand <- ggplot(fullnets_22, aes(x=sand, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Sand (%)", y= "CPUE of Fecund Fish (fish/hour)")
sand

# SILT
silt <- ggplot(fullnets_22, aes(x=silt, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Silt (%)", y= "CPUE of Fecund Fish (fish/hour)")
silt

# DATE
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

date <- lm(cpue~date_time_pull, core22)
summary(date)
################################################################################
#### CATEGORICAL PREDICTORS ----

# CMECS GROUPS
cmecs_group <- ggplot(fullnets_22, aes(x=cmecs_group, y=cpue)) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "CMECS Group", y= "CPUE of Fecund Fish (fish/hour)")
cmecs_group

# CMECS SUB GROUPS
cmecs_subgroup <- ggplot(fullnets_22, aes(x=cmecs_subgroup, y=cpue)) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "CMECS SubGroup", y= "CPUE of Fecund Fish (fish/hour)")
cmecs_subgroup

################################################################################


