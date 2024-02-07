#### LOAD PACKAGES ---- 
library(outliers)
library(dplyr)
#### DATA MANIPULATION ----
one <- subset(exp23, cmecs_reduce == "mud")
two <- subset(exp23, cmecs_reduce == "sand_mud")
three <- subset(exp23, cmecs_reduce == "sand")
four <- subset(exp23, cmecs_reduce == "gravelly")
five <- subset(exp23, cmecs_reduce == "gravel_mixes")

#### OUTLIER TEST ----
## Mud
grubbs.test(one$cpue, type = 10, opposite = FALSE, two.sided = FALSE)
hist(one$cpue, n=30)

## Sand/Mud
grubbs.test(two$cpue, type = 10, opposite = FALSE, two.sided = FALSE)
hist(two$cpue, n=30)
