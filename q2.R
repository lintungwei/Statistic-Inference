library(dplyr)

par(mfrow=c(1,1))

boxplot(len~dose+supp, data=ToothGrowth,ylab="Length", xlab = "ToothGrowth data: length vs dose+supplement")
xyplot(len~dose+supp, data=ToothGrowth)
xyplot(len~dose|supp, data=ToothGrowth, type="b")
plot.new()
bwplot(len~dose|supp, data=ToothGrowth, type="l")
