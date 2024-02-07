#### INSTALL PACKAGES ----
library(ggplot2)
library(GGally)
library(gridExtra)

################################################################################
####                          CORE 22                                       ####
################################################################################
#### SCATTER PLOT MATRICES (continuous) USING GGPAIRS ----

# only using 1 continuous variable
cor_matrix_scat23 <- ggpairs(core22[,c(14,31)]) + theme_bw()
cor_matrix_scat23

################################################################################
#### BOXPLOTS FOR CATEGORICAL VS CONTINUOUS ----
# (CMECS Reduced Groups in separate plots)

#### CALCULATE CORRELATION VALUES: CMECS Reduced Groups ----

# Average Temperature
temp_cor22 <- cor(fullnets_22$cmecs_reduce_ord, fullnets_22$temp_avg , method = "pearson", use="complete.obs")
temp_cor22

#### CONTINUOUS VS. CMECS REDUCED GROUP ----

# Average Temperature
temp_cmecsR22 <- ggplot(fullnets_22, mapping = aes(x = as.factor(cmecs_reduce), y = temp_avg)) + 
  geom_boxplot() +
  labs( x= "CMECS Reduced Group", y= "Average Temperature (C)") +
  annotate("text", x=1.5, y=0.65, size=4, label="Pearson Cor = 0.11" ) +
  theme_bw()
temp_cmecsR22

################################################################################
####                          CORE 23                                       ####
################################################################################
#### SCATTER PLOT MATRICES (continuous) USING GGPAIRS ----
cor_matrix_scat23 <- ggpairs(core23[,10:14]) + theme_bw()
cor_matrix_scat23

################################################################################
#### BOXPLOTS FOR CATEGORICAL VS CONTINUOUS ----
# (CMECS Reduced Groups in separate plots)

#### CALCULATE CORRELATION VALUES: CMECS Reduced Groups ----

# Average Vegetation Height
veg_height_cor23 <- cor(core23$cmecs_reduce_ord, core23$veg_height_avg, method="spearman", use="complete.obs")

# Average Vegetation Cover
veg_cover_cor23 <- cor(core23$cmecs_reduce_ord, core23$veg_cover_avg, method = "spearman", use="complete.obs")

# Median Depth
med_dep_cor23 <- cor(core23$cmecs_reduce_ord, core23$med_depth_m , method = "spearman", use="complete.obs")

# Average Temperature
temp_cor23 <- cor(core23$cmecs_reduce_ord, core23$temp_avg , method = "spearman", use="complete.obs")

cmecs_corvalues23 <- list(veg_height_cor23, veg_cover_cor23, med_dep_cor23, temp_cor23)
cmecs_corvalues23

#### CONTINUOUS VS. CMECS REDUCED GROUP ----

# Average Vegetation Height
vegH_cmecsR23 <- ggplot(core23, mapping = aes(x = as.factor(cmecs_reduce), y = veg_height_avg)) + 
  geom_boxplot() +
  labs( x= "CMECS Reduced Group", y= "Average Vegetation Height (m)") +
  annotate("text", x=1.5, y=0.65, size=4, label="Sp. Cor = 0.76" ) +
  theme_bw()
vegH_cmecsR23

# Average Vegetation Cover
vegC_cmecsR23 <- ggplot(core23, mapping = aes(x = as.factor(cmecs_reduce), y = veg_cover_avg)) + 
  geom_boxplot() +
  labs( x= "CMECS Reduced Group", y= "Average Vegetation Cover (m)") +
  annotate("text", x=3, y= 75, size=4, label="Sp. Cor = 0.04" ) +
  theme_bw()
vegC_cmecsR23

# Median Depth
medD_cmecsR23 <- ggplot(core23, mapping = aes(x = as.factor(cmecs_reduce), y = med_depth_m)) + 
  geom_boxplot() +
  labs( x= "CMECS Reduced Group", y= "Median Depth (m)") +
  annotate("text", x=1.5, y=0.65, size=4, label="Sp. Cor = -0.40" ) +
  theme_bw()
medD_cmecsR23

# Average Temperature
temp_cmecsR23 <- ggplot(core23, mapping = aes(x = as.factor(cmecs_reduce), y = temp_avg)) + 
  geom_boxplot() +
  labs( x= "CMECS Reduced Group", y= "Average Temperature (C)") +
  annotate("text", x=1.5, y=0.65, size=4, label="Sp. Cor = 0.31" ) +
  theme_bw()
temp_cmecsR23

#### ALL CONTINUOUS VS. CMECS PLOTS ----
cmecs_cor23 <- grid.arrange(vegH_cmecsR23, vegC_cmecsR23, medD_cmecsR23, temp_cmecsR23, 
                          nrow=2, ncol=2)
cmecs_cor23

################################################################################
####                          EXP 23                                       ####
################################################################################
#### SCATTER PLOT MATRICES (continuous) USING GGPAIRS ----
cor_matrix_scat_exp23 <- ggpairs(exp23[,c(6,7,14,31,36)]) + theme_bw()
cor_matrix_scat_exp23

################################################################################
#### BOXPLOTS FOR CATEGORICAL VS CONTINUOUS ----
# (CMECS Reduced Groups in separate plots)

#### CALCULATE CORRELATION VALUES: CMECS Reduced Groups ----

# Average Vegetation Height
veg_height_corexp23 <- cor(exp23$cmecs_reduce_ord, exp23$veg_height_avg, method="spearman", use="complete.obs")

# Average Vegetation Cover
veg_cover_corexp23 <- cor(exp23$cmecs_reduce_ord, exp23$veg_cover_avg, method = "spearman", use="complete.obs")

# Median Depth
med_dep_corexp23 <- cor(exp23$cmecs_reduce_ord, exp23$med_depth_m , method = "spearman", use="complete.obs")

# Average Temperature
temp_corexp23 <- cor(exp23$cmecs_reduce_ord, exp23$temp_avg , method = "spearman", use="complete.obs")

cmecs_corvalues_exp23 <- list(veg_height_corexp23, veg_cover_corexp23, med_dep_corexp23, temp_corexp23)
cmecs_corvalues_exp23

#### CONTINUOUS VS. CMECS REDUCED GROUP ----

# Average Vegetation Height
vegH_cmecsRexp23 <- ggplot(exp23, mapping = aes(x = as.factor(cmecs_reduce), y = veg_height_avg)) + 
  geom_boxplot() +
  labs( x= "CMECS Reduced Group", y= "Average Vegetation Height (m)") +
  annotate("text", x=1.5, y=0.65, size=4, label="Sp. Cor = 0.10" ) +
  theme_bw()
vegH_cmecsRexp23

# Average Vegetation Cover
vegC_cmecsRexp23 <- ggplot(exp23, mapping = aes(x = as.factor(cmecs_reduce), y = veg_cover_avg)) + 
  geom_boxplot() +
  labs( x= "CMECS Reduced Group", y= "Average Vegetation Cover (m)") +
  annotate("text", x=2, y= 82, size=4, label="Sp. Cor = 0.15" ) +
  theme_bw()
vegC_cmecsRexp23

# Median Depth
medD_cmecsRexp23 <- ggplot(exp23, mapping = aes(x = as.factor(cmecs_reduce), y = med_depth_m)) + 
  geom_boxplot() +
  labs( x= "CMECS Reduced Group", y= "Median Depth (m)") +
  annotate("text", x=1.5, y=0.65, size=4, label="Sp. Cor = -0.40" ) +
  theme_bw()
medD_cmecsRexp23

# Average Temperature
temp_cmecsRexp23 <- ggplot(exp23, mapping = aes(x = as.factor(cmecs_reduce), y = temp_avg)) + 
  geom_boxplot() +
  labs( x= "CMECS Reduced Group", y= "Average Temperature (C)") +
  annotate("text", x=1.5, y=0.65, size=4, label="Sp. Cor = 0.31" ) +
  theme_bw()
temp_cmecsRexp23

#### ALL CONTINUOUS VS. CMECS PLOTS ----
cmecs_cor_exp23 <- grid.arrange(vegH_cmecsRexp23, vegC_cmecsRexp23, medD_cmecsRexp23, temp_cmecsRexp23, 
                          nrow=2, ncol=2)
cmecs_cor_exp23
