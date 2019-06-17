#find the source
getwd()
#list the files in folder
list.files("dataSets")
#load the data
states.data <- readRDS("dataSets/states.rds")
#get labels
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])
#examine  the last few labels
tail(states.info,8)

#summary of expense and csa columns, all rows
sts.ex.sat <- subset(states.data, select = c("expense", "csat"))
summary(sts.ex.sat)

#correlation between exp and csat
cor(sts.ex.sat)

#scatterplot of expense vs csat
plot(sts.ex.sat)

#fit regression model
sat.mod <- lm(csat ~ expense, data = states.data)
summary(sat.mod) #shows regression coefficients table

#what if no variation btwn states for percentagees
summary(lm(csat ~ expense + percent, data = states.data))

#examine model object
class(sat.mod)
names(sat.mod)
methods(class = class(sat.mod))[1:9]  #WTF

#more info of fit
confint(sat.mod)

hist(residuals(sat.mod))
#linear reg assumptions
plot(sat.mod)

#comparing models

#fit another model- adding house and senate as predictors
sat.voting.mod <- lm(csat ~ expense + house + senate, data = na.omit(states.data))
sat.mod <- update(sat.mod, data = na.omit(states.data))

#compare using anova funct(analysis of variance)

anova(sat.mod, sat.voting.mod)
coef(summary(sat.voting.mod))

# Exercise 0
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])

tail(states.data, 8)

sat.ener.mod <- lm(energy ~ metro, data = states.data)
summary(sat.ener.mod)
plot(sat.ener.mod)

sat.ener.mod1 <- lm(energy ~ metro + green + waste + pop, data = states.data)
summary(sat.ener.mod1)
plot(sat.ener.mod1)

str(states.data$region)
states.data$region<- factor(states.data$region)
#add region to the model
sat.region<- lm(csat ~ region, data = states.data)


#show results
coef(summary(sat.region))#show regression coefficients table

anova(sat.region)# show ANOVA table

contrasts(states.data$region)

coef(summary(lm(csat ~ C(region, base = 4),data = states.data)))


#Exercise 1 add interaction

states.data$waste<- factor(states.data$waste)

sat.region.waste <- lm(csat ~ region*waste, data = states.data)
#show results
coef(summary(sat.region.waste))
plot(sat.region.waste)

#regression with binary outcomes
getwd()
#list the files in folder
list.files("dataSets")
#load the data
NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels
str(NH11$hypev)#check structure of  hypev
levels(NH11$hypev)#check levels of hypev
#collapse all missing to NA
NH11$hypev <- factor(NH11$hypev, levels = c("2 No", "1 Yes"))
# run regression model
hyp.out <- glm(hypev~age_p+ sex + sleep + bmi, 
                 data = NH11, family = "binomial")
coef(summary(hyp.out))

hyp.out.tab <- coef(summary(hyp.out))
hyp.out.tab[,"Estimate"] <- exp(coef(hyp.out))
hyp.out.tab

#create a dataset with predictors set at desired levels
predDat <- with(NH11, expand.grid(age_p = c(33, 63),
                                  sex = "2 Female",
                                  bmi = mean(bmi, na.rm = TRUE),
                                  sleep = mean(sleep, na.rm = TRUE)))


predDat
cbind(predDat,predict(hyp.out, type = "response",
                        interval = "confidence",
                       newdata = predDat))
##I still can't find or install the nececssary package
library(devtools)
library(effects)



#exercise 2

str(NH11$everwrk)
NH11$everwrk <- factor(NH11$everwrk, levels = c("2 No", "1 Yes"))
wk.age.mar <- glm(everwrk~age_p + r_maritl, data = NH11,
               family = "binomial")

coef(summary(wk.age.mar))
wk.age.mar<- coef(summary(wk.age.mar))

summary(wk.age.mar)

cbind(NH11$everwrk,predict(wk.age.mar, type = "response",
                      interval = "confidence",
                      newdata = NH11$everwrk))

plot(allEffects(wk.age.mar))

#exercise 3
install.packages("multilevel")
data(bh1996, package = "multilevel")

library(lme4)
grp1 <- lmer(WBEING ~ 1 + (1 | GRP), data = bh1996)
summary(grp1)

#add 2 pred. HRS and LEAD

grp2 <- lmer(WBEING ~ HRS + LEAD + (1 | GRP), data = bh1996)
summary(grp2)

grp3 <- lmer(WBEING ~ HRS + LEAD +(1 + HRS | GRP), 
             data = bh1996)
anova(grp2, grp3)










