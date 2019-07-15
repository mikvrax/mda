library(car)

#Q1a
dataset <- Ericksen
undercount <- dataset$undercount
minority <- dataset$minority
crime <- dataset$crime
language <- dataset$language

plot(minority,undercount)
slr <- lm(undercount ~ minority)
summary(slr)
coefs <- coefficients(slr)
lines(coefs[1] + coefs[2] * minority)
lines(fitted.values(slr))
lines(minority,fitted.values(slr))
#Q1b
slr <- lm(undercount ~  crime)
summary(slr)
slr <- lm(undercount ~ language)
summary(slr)

#Q2a
mlr <- lm(undercount ~ minority + crime + language)
summary(mlr)
confint(mlr)
#Q2b
coefs <- coefficients(mlr)
y <- coefs[1] + coefs[2]*26.1 + coefs[3]*49 + coefs[4]*0.2
new <- data.frame(minority=26.1,crime=49,language=0.2)
predict(mlr,new)

