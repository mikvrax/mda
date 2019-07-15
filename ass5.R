library("car")
library("MASS")

# load data and perform multiple linear regression
dataset <- road
model1 <- lm(deaths ~ drivers + popden + fuel,data=dataset)

# leverage: compute hat values and plot them along others
hv <- hatvalues(model1)
plot(1:26,hv,main="Hat-Values",xlab="Observation index",ylab="Hat-Values")
identify(1:26,hv,rownames(dataset))
influenceIndexPlot(model1,labels=row.names(dataset),id.n=3)

# outliers: compute studentized residuals and plot them
rstudent(model1)
qqPlot(model1,labels=row.names(dataset),id.n=3)

# coefficient influence: compute Cook's distance, DFBETAS, COVRATIO and plot them
cooks.distance(model1)
influencePlot(model1,labels=row.names(dataset),id.n=3)
dfbetas(model1)
dfbetasPlots(model1,labels=row.names(dataset),id.n=3)
covrs <- covratio(model1)
plot(1:26,covrs,main="COVRATIO",xlab="Observation index",ylab="COVRATIO")
identify(1:26,covrs,rownames(dataset))

#joint influence of variables: added-variable plots
avPlots(model1,labels=row.names(dataset),id.n=3)

#b
# remove Maine, DC and California rows 
dataset2 <- dataset[-(c(5,9,19)),]
# perform multiple linear regression on the rest and compare model results
model2 <- lm(deaths ~ drivers + popden + fuel,data=dataset2)
summary(model1)
summary(model2)

#c
# display quantile comparison plot, histogram and non-parametric density estimation plot of studentized residuals in the same figure
par(mfrow = c(1,3))
qqPlot(model2)
hist(rstudent(model2))
plot(density(rstudent(model2)))

boxplot(subset(dataset2^(1/5),select=c("drivers","popden","fuel")))
par(mfrow = c(1,3))
model3 <- lm(deaths ~ drivers + popden + fuel,data=log(dataset2))
qqPlot(model3)
hist(rstudent(model3))
plot(density(rstudent(model3)))
