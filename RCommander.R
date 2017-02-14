
data(Prestige, package="car")
summary(Prestige)
library(abind, pos=17)
# Table for prestige:
with(Prestige, tapply(prestige, list(type), mean, na.rm=TRUE))
# Table for women:
with(Prestige, tapply(women, list(type), mean, na.rm=TRUE))
scatterplotMatrix(~census+education+income+prestige+women, reg.line=FALSE, 
  smooth=TRUE, spread=FALSE, span=0.5, ellipse=FALSE, levels=c(.5, .9), 
  id.n=0, diagonal = 'histogram', data=Prestige)
Prestige$log2income <- with(Prestige, log2(income))
RegModel.1 <- lm(prestige~education+log2income+women, data=Prestige)
summary(RegModel.1)
oldpar <- par(oma=c(0,0,3,0), mfrow=c(2,2))
plot(RegModel.1)
par(oldpar)
qqPlot(RegModel.1, simulate=TRUE, id.method="y", id.n=2)
RegModel.2 <- lm(prestige~education+log2income, data=Prestige)
summary(RegModel.2)
compareCoefs(RegModel.1, RegModel.2)

