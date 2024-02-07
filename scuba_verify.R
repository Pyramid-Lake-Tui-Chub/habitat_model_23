#### INSTALL PACKAGES ----
library(DescTools)
library(Hmisc)
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
#### DATA READ IN ----
setwd("C:\\Documents\\Pyramid_Lake\\RCreations\\RProjects\\habitat_model_2023")
scuba_verif <-read.csv("scuba_master_2.csv")

#### DATA MANIPULATION ----
# transform date
scuba_verif$date <- mdy(scuba_verif$date)

# delete unnecessary columns
scuba_verif <- scuba_verif[,c(1,2,3,7,8,10,13:30)]

# change bedrock and boulder to integer
scuba_verif$bedrock <- as.integer(scuba_verif$bedrock)
scuba_verif$boulder <- as.integer(scuba_verif$boulder)

# change scuba names for merge
names(scuba_verif)[c(4:5)] <- c("deep_m_scuba", "shallow_m_scuba")

scuba_verif <- scuba_verif %>% pivot_longer(cols = "bedrock":"hard_clay",
                                          names_to = "sub_class",
                                          values_to = "sub_class_perc")

#### PLOT OF OVERALL SCUBA SUBSTRATE CLASSES ----
sum_scuba_verif <- scuba_verif %>% group_by(sub_class) %>% 
  summarise(substrate_avg = mean(sub_class_perc, na.rm = T))

# barplot of % substrates
# order for plot
sum_scuba_verif$sub_class <- factor(sum_scuba_verif$sub_class, 
                                    level = c("silt", "hard_clay", "sand", "small_gravel", "gravel", "cobble", "boulder", "bedrock"))

sub_overall <- ggplot(sum_scuba_verif, aes(x= sub_class, y = substrate_avg)) +
  geom_bar(stat = "identity", fill = "grey40") +
  scale_x_discrete(name = " ", labels = c("Silt", "Clay", "Sand", "Small Gravel", "Gravel", "Cobble", "Boulder", "Bedrock")) +
  scale_y_continuous(name = "% Substrate")+
  coord_flip()
sub_overall

# dataset above the modal max depth
scuba_verif <- scuba_verif %>% mutate(above = ifelse(deep_m_scuba > 6.1, TRUE, FALSE))
sum(scuba_verif$above, na.rm=TRUE)
summarise(scuba_verif$above=FALSE)

scuba_deep <- subset(scuba_verif, above == TRUE)

#### VERIFY ASSUMPTIONS BASED ON SCUBA ----
## Mode
scuba_verif %>% group_by(site, date) %>% Mode(max(deep_m_scuba))

# Percent Complex

# flip scuba sub_classes to long format
# change bedrock and boulder to integer
scuba_long <- scuba_verif %>% pivot_longer(cols = "bedrock":"hard_clay",
                                          names_to = "sub_class",
                                          values_to = "sub_class_perc")

sum <- scuba_long %>% group_by(sub_class) %>% summarise(mean = mean(sub_class_perc), n=n(), sum= sum(sub_class_perc), na.rm = TRUE)
sum <- sum %>% mutate(sum = mean*count)

# Percent Veg Cover
mean(scuba_verif$veg_cover, na.rm=TRUE)

## Histograms

# Deep Depth
hist(scuba_verif$deep_m_scuba)

# Distribution of nets classed w/hydros
hydro_nets <- subset(habData_master, !is.na(date_hydro) & year == 2023)

ggplot(hydro_nets, aes(x=med_depth_m)) +
  geom_histogram(binwidth = 1) +
  scale_x_continuous(n.breaks = 35)


# summary statistics for a dataframe
Hmisc::describe(hydro_nets)


