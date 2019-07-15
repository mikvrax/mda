library(car)
library(carData)

#Q1
dataset <- Angell
moral <- dataset$moral
hetero <- dataset$hetero
mobility <- dataset$mobility

par(mfrow = c(2,2))
hist(moral)
dm <- density(moral)
plot(dm)
qqnorm(moral)
qqline(moral)
boxplot(moral, main="Moral Integration")


hist(hetero)
dh <- density(hetero)
plot(dh)
qqnorm(hetero)
qqline(hetero)
boxplot(hetero,main="Ethnic Heterogenity")

hist(mobility)
dm2 <- density(mobility)
plot(dm2)
qqnorm(mobility)
qqline(mobility)
boxplot(mobility,main="Geographic Mobility")

#Q2a
SkewnessRatio <- function(v)
{
	quants <- quantile(v,names=FALSE)
	med <- median(v)
	return((quants[4] - med)/(med - quants[2]))
}

#Q2b
# needs values between 0 and 1
FoldedPowers <- function(v,q)
{
  return(v^q -(1-v)^q)
}

SkewnessRatio(moral)
SkewnessRatio(moral^(1/2))
SkewnessRatio(moral^(1/3))
SkewnessRatio(moral^(1/4))

par(mfrow = c(1,2))
boxplot(moral,main="Moral Integration")
boxplot(moral^(1/4),main="Transformed Moral Integration")

SkewnessRatio(hetero)
SkewnessRatio(logit(hetero))
SkewnessRatio(FoldedPowers(hetero/100,0.14))
SkewnessRatio(FoldedPowers(hetero/100,0.41))
SkewnessRatio(hetero^(-1/2))
SkewnessRatio(hetero^(-1))
boxplot(hetero,main="Ethnic Heterogenity")
boxplot(hetero^(-1),main="Transformed Ethnic Heterogenity")

SkewnessRatio(mobility)
SkewnessRatio(logit(mobility))
SkewnessRatio(FoldedPowers(mobility/100,0.14))
SkewnessRatio(FoldedPowers(mobility/100,0.41))
SkewnessRatio(mobility^(-1/2))
SkewnessRatio(mobility^(-1/4))
boxplot(mobility,main="Geographic Mobility")
boxplot(mobility^(-1/4),main="Transformed Geographic Mobility")




#Q3a
dataset <- UN
lifeExpF <- na.omit(dataset$lifeExpF)
ppgdp <- na.omit(dataset$ppgdp)
group <- na.omit(dataset$group)

par(mfrow = c(1,2))
plot(ppgdp,lifeExpF,main="Life Expectancy and Per Capita GDP",xlab="Per Capita GDP",ylab="Female Life Expectancy in Years")
lines(lowess(ppgdp,lifeExpF),col=2)

plot(log(ppgdp),lifeExpF^(5),main="Transfomed Life Expectancy and Per Capita GDP",xlab="log(Per Capita GDP)",ylab="(Female Life Expectancy in Years)^5")
lines(lowess(log(ppgdp),lifeExpF^(5)),col=2)
#Q3b
par(mfrow = c(1,1))
boxplot(lifeExpF ~ group, main="Life Expectancy and Country type", xlab="Country Group", ylab="Female Life Expectancy in Years")
# Do for every categorical value,fit line
oecd <- group == "oecd"
other <- group == "other"
africa <- group == "africa"

quants_oecd <- quantile(lifeExpF[oecd])
quants_other <- quantile(lifeExpF[other])
quants_africa <- quantile(lifeExpF[africa])
hinge_spread <- c(quants_oecd[4] - quants_oecd[2],quants_other[4] - quants_other[2],quants_africa[4] - quants_africa[2]) 
medians <- c(median(lifeExpF[oecd])+1,median(lifeExpF[other])+1,median(lifeExpF[africa])+1)

plot(log(medians),log(hinge_spread))
lines(lowess(log(medians),log(hinge_spread)))
lifeExpF^(1-(-4))
boxplot(lifeExpF^(5) ~ group, main="Life Expectancy and Country type", xlab="Country Group", ylab="Female Life Expectancy in Years")

spreadLevelPlot(lifeExpF ~ group)
boxplot(lifeExpF^(5.013509) ~ group, main="Life Expectancy and Country type", xlab="Country Group", ylab="Female Life Expectancy in Years")

