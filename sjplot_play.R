#### LOAD PACKAGES ----
library(tidyverse)
library(sjPlot)
library(geeasy)

################################################################################
view_df(exp23, show.frq = T)

exp23 %>%
  plot_frq(mesh_num)

plot_model(glmm_exp23, type = "pred", p.shape = TRUE)

plot_residuals(glmm_exp23,
               geom.size = 2,
               remove.estimates = c(9:10),
               show.lines = FALSE,
               show.resid = FALSE,
               show.pred = TRUE,
               show.ci = TRUE)

plotEst(glmm_exp23)
