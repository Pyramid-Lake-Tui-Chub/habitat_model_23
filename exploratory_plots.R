#### INSTALL PACKAGES ----
library(tidyverse)
library(tidyr)
library(reshape2)
library(knitr)
library(dplyr)
library(lubridate)
library(data.table)
library(plotrix)

##################################################################################
###                                                                            ###
##################################################################################
#### Substrate Classification Plots ----

## Hydroacoustics ##
sub_classes <- ggplot(data= subset(hydro_data), 
                      aes(x= depth, y=sub_perc_depth, 
                          color=factor(bottom_type_num), 
                          group=bottom_type_num)) +
  geom_point(size=1, alpha= 0.75) +
  geom_smooth(method=loess) +
  scale_x_continuous(breaks=c(5,10,15,20), limits = c(0,20)) +
  scale_y_continuous(breaks=c(25, 50, 75, 100)) +
  ggtitle("Hydroacoustics Substrate Class Output (for Specified Site)") +
  theme(panel.background= element_rect(color="black"),
        panel.grid = element_line(color="gray"),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        axis.title.y= element_text(size=12),
        axis.text.x= element_text(size=12, angle=45, hjust=1)) +
  labs(x="Depth(m)", y="Substrate Class Percent", color="Subtrate Types") +
  scale_color_brewer(palette= "Dark2") 
sub_classes

## SCUBA ##

# bottom type by depth
scuba_sub_classes <- ggplot(data= scuba_data, 
                            aes(x= deep_m, y=sub_class_perc, 
                                color=factor(sub_class), 
                                group=sub_class)) +
  geom_point(size=1, alpha= 0.75)+
  geom_smooth(method=loess)+
  scale_x_continuous(breaks=c(0,5,10,15,20)) +
  scale_y_continuous(breaks=c(0, 25, 50, 75, 100)) +
  ggtitle("SCUBA Substate Class Output (for Specified Site)") +
  theme(panel.background= element_rect(color="black"),
        panel.grid = element_line(color="gray"),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        axis.title.y= element_text(size=12),
        axis.text.x= element_text(size=12, angle=45, hjust=1)) +
  labs(x="Deep Depth(m)", y="Substrate Class Percent", color="Subtrate Types") +
  scale_color_brewer(palette= "Dark2") 
scuba_sub_classes

# depth distribution of substrate
# facet wrap on substrate class
scuba_dist <- ggplot(scuba_data, 
                     aes(x=deep_m, y=sub_class_perc)) +
  geom_count(col="tomato3") +
  theme(panel.background= element_rect(color="black"),
        panel.grid = element_line(color="gray"),
        plot.title = element_text(hjust=0.5),
        legend.position = "bottom", 
        axis.title.y= element_text(size=12),
        axis.text.x= element_text(size=12, angle=45, hjust=1)) +
  facet_wrap(~sub_class, ncol=1)
scuba_dist

# trying to figure out hydro data...
# overall percent of each substrate type by depth
overall <- scuba_data %>% group_by(deep_m, sub_class) %>% summarise(sum=sum(sub_class_perc))
count <- scuba_data %>% group_by(deep_m) %>% summarise(n=n())

scuba_5less <- subset(scuba_data, deep_m > 5) %>% group_by(sub_class) %>% summarise(sum=sum(sub_class_perc))
count_5less <- subset(scuba_data, deep_m > 5) %>% group_by(sub_class) %>% summarise(n=n())

scuba_5plus <- subset(scuba_data, deep_m < 5) %>% group_by(sub_class) %>% summarise(sum=sum(sub_class_perc))
count_5plus <- subset(scuba_data, deep_m < 5) %>% group_by(sub_class) %>% summarise(n=n())

scuba_5less <- scuba_5less %>% mutate(over_perc = (sum/411))
scuba_5plus <- scuba_5plus %>% mutate(over_perc = (sum/155))

###################################################################################################
###                                                                                             ###
###################################################################################################
#### CMECS Exploration ----

# histogram of cmecs subgroups
cmecs_bar1 <- ggplot(habData_master, aes(x=cmecs_subgroup, fill=cmecs_subgroup)) + 
              geom_bar() +
              theme(axis.text = element_text(angle = 45))
cmecs_bar1

# count by cmecs subgroup
cmecs_counts_sub <- habData_master %>% count(cmecs_subgroup)

# histogram of cmecs groups
cmecs_bar2 <- ggplot(habData_master, aes(x=cmecs_group, fill=cmecs_group)) + 
  geom_bar() +
  theme(axis.text = element_text(angle = 45))
cmecs_bar2

# count by cmecs group
cmecs_counts_group <- habData_master %>% count(cmecs_group)

# histogram of cmecs reduced groups
cmecs_bar3 <- ggplot(habData_master, aes(x=cmecs_reduce, fill=cmecs_reduce)) + 
  geom_bar() +
  theme(axis.text = element_text(angle = 45))
cmecs_bar3

# count by cmecs reduced group
cmecs_counts_reduce <- habData_master %>% count(cmecs_reduce)