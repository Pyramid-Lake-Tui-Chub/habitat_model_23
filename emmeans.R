#### INSTALL PACKAGES ----
library(tidyverse)
library(emmeans)
library(broom)
library(export)

setwd("C:\\RCreations\\ROutput\\ModelOutput&Tables")

## NON-ADJUSTED MARGINAL MEANS PAIRWISE COMPARISON##
# cmecs_reduce -- unadjusted
em_cmecs <- emmeans(m2, pairwise ~ "cmecs_reduce", component = "cond", adjust = "none")

## EXPORT ##
# working directory set earlier, change if need be

# pull emmGrid objects out of emmeans output which is a list
emm_1 <- as.emmGrid(em_cmecs$emmeans)
emm_2 <- as.emmGrid(em_cmecs$contrast)

# tidy emmGrid objects
noAdj <- tidy(emm_1, confint = TRUE)
noAdj2 <- tidy(emm_2, confint = TRUE)

# export using package(export) to word Table
# can change the format of export, look at help vignette
table2office(
  x = noAdj,
  file = "emmeans_noAdjust",
  type = c("DOC"),
  append = FALSE,
  digits = 2,
  digitspvals = 4,
  trim.pval = 1e-16,
  width = NULL,
  height = NULL,
  font = ifelse(Sys.info()["sysname"] == "Windows", "Arial", "Helvetica")[[1]],
  pointsize = 12,
  add.rownames = FALSE
)

table2office(
  x = noAdj2,
  file = "emmeans_noAdjust",
  type = c("DOC"),
  # append to the file from above, so cool!
  append = TRUE,
  digits = 2,
  digitspvals = 4,
  trim.pval = 1e-16,
  width = NULL,
  height = NULL,
  font = ifelse(Sys.info()["sysname"] == "Windows", "Arial", "Helvetica")[[1]],
  pointsize = 12,
  add.rownames = FALSE
)

## ADJUSTED ##
# cmecs_reduce -- adjusted
em_cmecsAdj <- emmeans(m2, pairwise ~ "cmecs_reduce", component = "cond", adjust = "tukey")

## EXPORT ##

# pull emmGrid objects out of emmeans output which is a list
emm_3 <- as.emmGrid(em_cmecsAdj$contrast)
# no need for emmeans section, same as non-adjusted

# tidy emmGrid objects
adj <- tidy(emm_3, confint = TRUE)

# export using package(export) to word Table
# can change the format of export, look at help vignette

table2office(
  x = adj,
  file = "emmeans_adjust",
  type = c("DOC"),
  append = FALSE,
  digits = 2,
  digitspvals = 4,
  trim.pval = 1e-16,
  width = NULL,
  height = NULL,
  font = ifelse(Sys.info()["sysname"] == "Windows", "Arial", "Helvetica")[[1]],
  pointsize = 12,
  add.rownames = FALSE
)

## OTHER THINGS I DIDN'T USE IN THE PUBLICATION ##
# temp_avg
# mean at intervals of 5C
emmeans(m2, "temp_avg", component = "cmean", 
        at = list(temp_avg = c(0, 5, 10, 15, 20, 25)))
emmeans(m2, "temp_avg", type = "response", component = "zi",
        at = list(temp_avg = c(0, 5, 10, 15, 20, 25)))
ref_grid(m2)

# slope
# marginal mean at intervals of 0.2 increases of slope
emmeans(m2, "slope", component = "cmean", 
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))
emmeans(m2, "slope", type = "response", component = "zi", 
        at = list(slope = c(0.2, 0.4, 0.6, 0.8, 1, 1.2, 1.4)))

# veg cover
emmeans(m2, "veg_cover_avg", component = "cmean")
emmeans(m2, "veg_cover_avg", type = "response", component = "zi")

# pairwise comparison of cmecs_reduce
emmeans(m2, pairwise ~ cmecs_reduce)

# partial regression cmecs_reduce
emmip(m2, cmecs_reduce ~ poly(temp_avg, 2), cov.reduce = range)
emmip(m2, cmecs_reduce ~ scale(log(veg_cover_avg + 0.9)), cov.reduce = range)
emmip(m2, cmecs_reduce ~ poly(mesh_num, 2), cov.reduce = range)
emmip(m2, cmecs_reduce ~ scale(log(slope + 0.9)), cov.reduce = range)
