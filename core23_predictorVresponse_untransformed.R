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
veg_height <- ggplot(core23, aes(x=veg_height_avg, y=cpue)) + 
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
veg_cover <- ggplot(core23, aes(x=veg_cover_avg, y=cpue)) + 
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
slope <- ggplot(core23, aes(x=slope, y=cpue)) + 
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
med_depth <- ggplot(core23, aes(x=med_depth_m, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Median Depth (m)", y= "CPUE of Fecund Fish (fish/hour)")
med_depth

################################################################################
#### TEMPERATURE AVERAGE ----
temp_avg <- ggplot(core23, aes(x=temp_avg, y=cpue)) + 
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
#### CMECS REDUCED CATEGORIES ----
cmecs_reduce <- ggplot(core23, aes(x=cmecs_reduce, y=cpue)) + 
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
sand <- ggplot(core23, aes(x=sand, y=cpue)) + 
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
silt <- ggplot(core23, aes(x=silt, y=cpue)) + 
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

# BEDROCK
bedrock <- ggplot(core23, aes(x=bedrock, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Bedrock (%)", y= "CPUE of Fecund Fish (fish/hour)")
bedrock

# BOULDER
boulder <- ggplot(core23, aes(x=boulder, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Boulder (%)", y= "CPUE of Fecund Fish (fish/hour)")
boulder

# GRAVEL
gravel <- ggplot(core23, aes(x=gravel, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Gravel (%)", y= "CPUE of Fecund Fish (fish/hour)")
gravel

# SMALL GRAVEL
small_gravel <- ggplot(core23, aes(x=small_gravel, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Small Gravel (%)", y= "CPUE of Fecund Fish (fish/hour)")
small_gravel

# COBBLE
cobble <- ggplot(core23, aes(x=cobble, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Cobble (%)", y= "CPUE of Fecund Fish (fish/hour)")
cobble

# HARD CLAY
hard_clay <- ggplot(core23, aes(x=hard_clay, y=cpue)) + 
  geom_point()+
  geom_smooth(method=lm) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_color_brewer(palette="Dark2") +
  labs(x= "Hard Clay (%)", y= "CPUE of Fecund Fish (fish/hour)")
hard_clay

################################################################################
#### CATEGORICAL PREDICTORS ----

# CMECS GROUPS
cmecs_group <- ggplot(core23, aes(x=cmecs_group, y=cpue)) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "CMECS Group", y= "CPUE of Fecund Fish (fish/hour)")
cmecs_group

# CMECS SUB GROUPS
cmecs_subgroup <- ggplot(core23, aes(x=cmecs_subgroup, y=cpue)) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12, angle = 45),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "CMECS SubGroup", y= "CPUE of Fecund Fish (fish/hour)")
cmecs_subgroup

################################################################################