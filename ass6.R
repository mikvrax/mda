library("car")
library("nlme")
library("forecast")

dataset <- read.csv("kamyr_digester_subset.csv")
plot(dataset$Y.Kappa,xlab="Hours",ylab="Kappa number")
lines(dataset$Y.Kappa)

xreg <- cbind(ChipRate=dataset$ChipRate,BFCMratio=dataset$BF.CMratio,UCZAA=dataset$UCZAA,TlowerExt2=dataset$T.lowerExt.2,WhiteFlow=dataset$WhiteFlow.4,LowerHeatT3=dataset$Lower.HeatT.3,ChipMass4=dataset$ChipMass.4,BlackFlow2=dataset$BlackFlow.2,SteamHeatF3=dataset$SteamHeatF.3,WeakLiquorF=dataset$WeakLiquorF,AAWhiteSt4=dataset$AAWhiteSt.4)
model1 <- lm(Y.Kappa ~  ChipRate + BF.CMratio + UCZAA + T.lowerExt.2 + WhiteFlow.4 + Lower.HeatT.3 + ChipMass.4 + BlackFlow.2 + SteamHeatF.3 + WeakLiquorF + AAWhiteSt.4, data=dataset)
summary(model1)


qqPlot(residuals(model1))
shapiro.test(residuals(model1))

acf(residuals(model1))
pacf(residuals(model1))
durbinWatsonTest(model1,max.lag = 5)


auto.arima(dataset$Y.Kappa,xreg=xreg,ic="aic")
model2 <- arima(dataset$Y.Kappa, order = c(1L,0L,0L),xreg = xreg)
coef(model2)

pvalues <- (1 - pnorm(abs(model2$coef)/sqrt(diag(model2$var.coef))))*2
pvalues
xreg2 <- cbind(TlowerExt2=dataset$T.lowerExt.2,WhiteFlow=dataset$WhiteFlow.4,LowerHeatT3=dataset$Lower.HeatT.3,BlackFlow2=dataset$BlackFlow.2,SteamHeatF3=dataset$SteamHeatF.3)
auto.arima(dataset$Y.Kappa,xreg=xreg2,ic="aic")
model3 <- arima(dataset$Y.Kappa,xreg=xreg2,order=c(2,0,0),method="ML")
coef(model3)

