#### INSTALL PACKAGES ----
library(tidyverse)
library(tidyr)
library(plotrix)
library(cowplot)
library(reshape2)
library(RColorBrewer)
library(gridExtra)
library(knitr)
library(hrbrthemes)
library(viridis)
library(dplyr)
theme_set(theme_cowplot())

################################################################################
####                    CORE 2022 OPTIMAL PREDICTORS                        ####
################################################################################
#### CONTINUOUS PREDICTORS ----
################################################################################
#### VEGETATION HEIGHT AVERAGE ----
veg_height <- ggplot(exp23, aes(x=veg_height_avg, y=cpue)) + 
  geom_point()+
  #geom_smooth(method=lm) +
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
veg_cover <- ggplot(exp23, aes(x=veg_cover_avg, y=cpue)) + 
  geom_point()+
  #geom_smooth(method=lm) +
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
slope <- ggplot(exp23, aes(x=slope, y=cpue)) + 
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
med_depth <- ggplot(exp23, aes(x=med_depth_m, y=cpue)) + 
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
## add a depth_bin category to look at depth vs. temperature effects
exp23 <- exp23 %>% mutate(depth_bin = case_when(med_depth_m >= 0 & med_depth_m <= 2 ~ 1,
                                                med_depth_m > 2 & med_depth_m <= 4 ~ 2,
                                                med_depth_m > 4 & med_depth_m <= 6 ~ 3,
                                                med_depth_m > 6 & med_depth_m <= 8 ~ 4,
                                                med_depth_m > 8 & med_depth_m <= 10 ~ 5,
                                                med_depth_m > 10 & med_depth_m <= 12 ~ 6,
                                                med_depth_m > 12 & med_depth_m <= 14 ~ 7,
                                                med_depth_m > 14 & med_depth_m <= 16 ~ 8,
                                                med_depth_m > 16 & med_depth_m <= 18 ~ 9,
                                                med_depth_m > 18 ~ 10))


temp_avg <- ggplot(exp23, aes(x=temp_avg, y=log(cpue + 1))) + 
  geom_point()+
  #geom_smooth(method=loess) +
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  scale_y_reverse() +
  scale_color_brewer(palette = "Spectral") +
  labs(x= "Average Temperature (C)", y= "Tui Chub CPUE (fish/hr)")
temp_avg

################################################################################
#### CMECS REDUCED CATEGORIES ----
cmecs_reduce <- ggplot(exp23, aes(x=cmecs_reduce, y=slope)) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "CMECS Reduced Group", y= "CPUE of Fecund Fish (fish/hour)")
cmecs_reduce

#### Mesh Size CATEGORIES ----
mesh_size <- ggplot(exp23, aes(x=mesh_num, y=cpue)) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "CMECS Reduced Group", y= "CPUE of Fecund Fish (fish/hour)")
mesh_size

################################################################################
####                     SUB-OPTIMAL PREDICTORS                             ####
################################################################################
#### CONTINUOUS PREDICTORS ----
# SAND
sand <- ggplot(exp23, aes(x=sand, y=cpue)) + 
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
silt <- ggplot(exp23, aes(x=silt, y=cpue)) + 
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
bedrock <- ggplot(exp23, aes(x=bedrock, y=cpue)) + 
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
boulder <- ggplot(exp23, aes(x=boulder, y=cpue)) + 
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
gravel <- ggplot(exp23, aes(x=gravel, y=cpue)) + 
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
small_gravel <- ggplot(exp23, aes(x=small_gravel, y=cpue)) + 
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
cobble <- ggplot(exp23, aes(x=cobble, y=cpue)) + 
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
hard_clay <- ggplot(exp23, aes(x=hard_clay, y=cpue)) + 
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
cmecs_group <- ggplot(exp23, aes(x=cmecs_group, y=cpue)) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "CMECS Group", y= "CPUE of Fecund Fish (fish/hour)")
cmecs_group

# CMECS SUB GROUPS
cmecs_subgroup <- ggplot(exp23, aes(x=cmecs_subgroup, y=cpue)) + 
  geom_boxplot(color= "black", fill = "turquoise4", alpha = 0.6)+
  theme(plot.title = element_text(size=16, hjust = 0.5),
        axis.text.x = element_text(size=12, angle = 45),
        axis.text.y=element_text(size=12),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14)) +
  labs( x= "CMECS SubGroup", y= "CPUE of Fecund Fish (fish/hour)")
cmecs_subgroup

################################################################################