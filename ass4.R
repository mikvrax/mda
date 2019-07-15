library("MASS")
library("car")
library("ggplot2")

dataset <- quine
days <- dataset$Days
eth <- dataset$Eth
sex <- dataset$Sex

#a
# plot the value distribution for the groups of factors
ggplot(dataset,aes(Sex,Days,fill=factor(Eth))) + geom_boxplot()
boxplot(days ~ eth*sex)

#b
# compute the mean of factor groups
tapply(dataset$Days,list(dataset$Eth,dataset$Sex),mean)
# compute the standard deviation of factor groups
tapply(dataset$Days,list(dataset$Eth,dataset$Sex),sd)
# create the cell means plot
interaction.plot(dataset$Eth,dataset$Sex,dataset$Days)
# count the number of observations in each cell
xtabs(~ Eth + Sex,data=dataset)

dataset2 <- data.frame(days,eth,sex)
cell1 <- dataset2[dataset2["eth"] == "A" & dataset2["sex"] == "F",]
cell2 <- dataset2[dataset2["eth"] == "N" & dataset2["sex"] == "F",]
cell3 <- dataset2[dataset2["eth"] == "A" & dataset2["sex"] == "M",]
cell4 <- dataset2[dataset2["eth"] == "N" & dataset2["sex"] == "M",]

std1 <- sd(cell1$days)
mean1 <- mean(cell1$days)
std2 <- sd(cell2$days)
mean2 <- mean(cell2$days)
std3 <- sd(cell3$days)
mean3 <- mean(cell3$days)
std4 <- sd(cell4$days)
mean4 <- mean(cell4$days)

means <- cbind(mean1,mean2,mean3,mean4)
plot(means[,])
lines(cbind(1,2),cbind(mean1,mean2))
lines(cbind(3,4),cbind(mean3,mean4))
#c
# perform two-way ANOVA with deviation-regression coding
model <- lm(Days ~Eth+Sex+Eth:Sex,data=dataset,contrasts=list(Eth="contr.sum",Sex="contr.sum"))
# get the model information such as the coefficients
summary(model)
# perform type-III and type-II tests to populate the ANOVA table
Anova(model,type="III")
Anova(model,type="II")
anova(model)
