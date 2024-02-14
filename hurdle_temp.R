x <- seq(7.5, 25, 0.25)
poly(x,2)
y <- -0.38 - 54.07*poly(x,2)[,1] + 12.26*poly(x,2)[,2]
plot(x,y)
plot(x,plogis(y))

library(glmmTMB)
library(ggeffects)

data("Salamanders")
Salamanders$DOP_z <- sjmisc::std(Salamanders$DOP)
fit_scale = glmmTMB(count~spp + poly(cover, 3) + mined + DOP_z+(1|site), 
                    ziformula=~DOY, 
                    dispformula = ~spp,
                    data = Salamanders, 
                    family=nbinom2)

ggpredict(fit_scale, c("cover"), type = "fe.zi") %>% plot()
