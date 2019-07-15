library(car)

dataset <- Salaries
salary <- dataset$salary
rank <- dataset$rank
service <- dataset$yrs.service
phd <- dataset$yrs.since.phd

#a
model0 <- lm(salary ~ service + phd)
summary(model0)
model1 <- lm(salary ~ 1)

#incremental,overall
anova(model0,model1)

#sequential
anova(model0)

#factor coding
#b
contrasts(rank)
model2 <- lm(salary ~ service + phd + rank)
summary(model2)
#c
anova(model0,model2)

#d
model3 <- lm(salary ~ service + phd + rank + service:rank + phd:rank)
model4 <- lm(salary ~ service + phd + rank + phd:rank)
anova(model4,model3)

model5 <- lm(salary ~ service + phd + rank + service:rank)
anova(model5,model3)

dataset2 <- data.frame(rank,phd,service,salary)
assocProf <- dataset2[dataset2["rank"] == "AssocProf",]
asstProf <- dataset2[dataset2["rank"] == "AsstProf",]
prof <- dataset2[dataset2["rank"] == "Prof",]

model6 <- lm(salary ~ service + phd,data=assocProf)
model7 <- lm(salary ~ service + phd,data=asstProf)
model8 <- lm(salary ~ service + phd,data=prof)

coefficients(model3)
coefficients(model6)
coefficients(model7)
coefficients(model8)
