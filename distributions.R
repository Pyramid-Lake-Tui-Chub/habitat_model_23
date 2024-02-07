#### INSTALL PACKAGES ----
library(ggplot2)
library(dplyr)

################################################################################
####                           FULL NETS 22                                 ####
################################################################################
#### RESPONSE ----
cpue_dist <- ggplot(fullnets_22, aes(cpue)) +
  geom_histogram(binwidth=0.1)
cpue_dist

#### INPUTS ----

## CONTINUOUS
temp_dist <- ggplot(fullnets_22, aes(temp_avg)) +
  geom_histogram(binwidth=0.1)
temp_dist

vegH_dist<- ggplot(fullnets_22, aes(veg_height_avg)) +
  geom_histogram(binwidth=0.1)
vegH_dist
  
vegC_dist <- ggplot(fullnets_22, aes(veg_cover_avg)) +
  geom_histogram(binwidth=0.1)
vegC_dist

slope_dist <- ggplot(fullnets_22, aes(log10(slope + 0.9))) +
  geom_histogram(binwidth=0.1)
slope_dist

## CATEGORICAL
cmecs_bar3 <- ggplot(fullnets_22, aes(x=cmecs_reduce, fill=cmecs_reduce)) + 
  geom_bar() +
  theme(axis.text = element_text(angle = 45))
cmecs_bar3

################################################################################
####                           CORE 23                                      ####
################################################################################
#### RESPONSE ----
cpue_dist <- ggplot(core23, aes(cpue)) +
  geom_histogram(binwidth=0.1)
cpue_dist

#### INPUTS ----

## CONTINUOUS
temp_dist <- ggplot(core23, aes(temp_avg)) +
  geom_histogram(binwidth=0.1)
temp_dist

vegH_dist<- ggplot(core23, aes(veg_height_avg)) +
  geom_histogram(binwidth=0.1)
vegH_dist

vegC_dist <- ggplot(core23, aes(veg_cover_avg)) +
  geom_histogram(binwidth=0.1)
vegC_dist

slope_dist <- ggplot(core23, aes(log10(slope + 0.1))) +
  geom_histogram(binwidth=0.1)
slope_dist

## CATEGORICAL
cmecs_bar3 <- ggplot(core23, aes(x=cmecs_reduce_ord, fill=cmecs_reduce_ord)) + 
  geom_bar() +
  theme(axis.text = element_text(angle = 45))
cmecs_bar3

################################################################################
####                            EXP 23                                      ####
################################################################################
#### RESPONSE ----
cpue_dist <- ggplot(exp23, aes(cpue)) +
  geom_histogram(binwidth=0.1)
cpue_dist

#### INPUTS ----

## CONTINUOUS
temp_dist <- ggplot(exp23, aes(temp_avg)) +
  geom_histogram(binwidth=0.1)
temp_dist

vegH_dist<- ggplot(exp23, aes(veg_height_avg)) +
  geom_histogram(binwidth=0.1)
vegH_dist

vegC_dist <- ggplot(exp23, aes(veg_cover_avg)) +
  geom_histogram(binwidth=0.1)
vegC_dist

slope_dist <- ggplot(exp23, aes(log(slope + 0.9))) +
  geom_histogram(binwidth=0.1)
slope_dist

## CATEGORICAL
cmecs_bar3 <- ggplot(exp23, aes(x=cmecs_reduce_ord, fill=cmecs_reduce_ord)) + 
  geom_bar() +
  theme(axis.text = element_text(angle = 45))
cmecs_bar3
