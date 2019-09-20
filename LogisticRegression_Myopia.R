# Determining causes for myopia by Harshith

# 1. Loading Data in R
# Check the working directory
getwd()
# Set working directory and read the data
# replace C:/Harshith with the file location on your hard drive
setwd("C:/Harshith")

#Load the data into RStudio (Read the data into a data frame)
myopia = read.csv('myopia.csv', row.names = 'ID')

#Check for the number of missing values in the dataframe
sum(is.na(myopia))

#Check the structure of the data
str(myopia)

#Check the summary of the data frame
summary(myopia)


#Besides the target variable MYOPIC, there are three categorical variables GENDER, MOMMY and DADMY.  
#Convert the data into factor
myopia$MYOPIC = factor(myopia$MYOPIC)
myopia$GENDER = factor(myopia$GENDER)
myopia$MOMMY = factor(myopia$MOMMY)
myopia$DADMY = factor(myopia$DADMY)
head(myopia)

#Visualize the three categorical variables

### par can be used to set or query graphical parameters. 
###Parameters can be set by specifying them as arguments to par in tag = value form, 
###or by passing them as a list of tagged values.

par(mfrow=c(1,3))
plot(myopia$MYOPIC, myopia$GENDER, xlab = "MYOPIC", ylab = "GENDER")
plot(myopia$MYOPIC, myopia$MOMMY, xlab = "MYOPIC", ylab = "MOMMY")
plot(myopia$MYOPIC, myopia$DADMY, xlab = "MYOPIC", ylab = "DADMY")

# Create a model to take a look at the fit with all categorical variables GENDER, MOMMY and DADMY.
#Initial exploratory fit
model1 <- glm(MYOPIC ~ GENDER + MOMMY + DADMY, data = myopia, family = binomial(logit))
summary(model1)

summary(model1)$coefficients


myopia.table <- xtabs(~ MOMMY + DADMY + MYOPIC, data = myopia)
ftable(myopia.table)

summary(myopia.table)

model2 <- glm(MYOPIC ~ ., data = myopia, family = binomial(logit))
summary(model2)$coefficients


## AL, ACD, LT and VCD have similar p-values. A closer examination shows that similar to DIOPTERHR, 
## there is a linear functional relationship. AL = ACD + LT + VCD. 
## The maximum absolute difference between two sides of the equation is 0.008.

### MYOPIC ~ (AL, ACD, LT, VCD) model selection

model.eyeball.no.AL <- step(glm(MYOPIC ~ .^2 , data = 
                              myopia[, c('MYOPIC', 'ACD', 'LT', 'VCD')], family = binomial(logit)))
summary(model.eyeball.no.AL)$coefficients


model.eyeball.no.ACD <- step(glm(MYOPIC ~ .^2 , data = 
                               myopia[, c('MYOPIC', 'AL', 'LT', 'VCD')], family = binomial(logit)))
summary(model.eyeball.no.ACD)$coefficients



model.eyeball.no.LT <- step(glm(MYOPIC ~ .^2 , data = 
                              myopia[, c('MYOPIC', 'AL', 'ACD', 'VCD')], family = binomial(logit)))
summary(model.eyeball.no.LT)$coefficients


model.eyeball.no.VCD <- step(glm(MYOPIC ~ .^2 , data = 
                               myopia[, c('MYOPIC', 'AL', 'ACD', 'LT')], family = binomial(logit)))
summary(model.eyeball.no.VCD)$coefficients


## Interestingly, removing ACD results in all other three variables statistically prominent (model.eyeball.no.ACD), 
## but keeping ACD, eventually only ACD alone is prominent. 
## So instead of removing just one term, we remove AL, LT and VCD, while keeping ACD.

## Let us remove these terms in the next iteration.


model3 <- glm(MYOPIC ~ . - STUDYYEAR - SPHEQ - AL - LT - VCD - DIOPTERHR, data = myopia, family = binomial(logit))
summary(model3)$coefficients



model3.step <- step(model3)
summary(model3.step)$coefficients


model3.2.step <- step(glm(MYOPIC ~ .^2, data = myopia[, c('MYOPIC', 'AGE', 'GENDER', 'ACD', 'SPORTHR', 'READHR',
                                      'COMPHR', 'STUDYHR', 'TVHR', 'MOMMY', 'DADMY')], family = binomial(logit)))

summary(model3.2.step)$coefficients

model3.step.2.step <- step(glm(MYOPIC ~ .^2, data = myopia[, c('MYOPIC', 'GENDER', 'ACD', 'SPORTHR',
                                                      'READHR','MOMMY', 'DADMY')], family = binomial(logit)))

summary(model3.step.2.step)$coefficients


model3.step.2.step.no.gender <- glm(formula = MYOPIC ~ ACD + SPORTHR + READHR + MOMMY + DADMY + GENDER:SPORTHR, 
                            data = myopia[, c("MYOPIC", "GENDER", "ACD", "SPORTHR", "READHR", "MOMMY", "DADMY")], 
                            family = binomial(logit))

summary(model3.step.2.step.no.gender)

###Some goodness-of-fit tests###

#Model Validity
with(pchisq(deviance, df.residual), data = model3.step.2.step.no.gender)

# H_0 independent hypothesis
with(pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = F), 
     data = model3.step.2.step.no.gender)


par(mfrow=c(1,2))
plot(predict(model3.step.2.step.no.gender), resid(model3.step.2.step.no.gender), ylab = 'Deviance residual')
plot(predict(model3.step.2.step.no.gender, type = 'response'), resid(model3.step.2.step.no.gender), 
                                                               ylab = 'Deviance residual')
#install.packages("pROC")
library(pROC)

result <- roc(myopia$MYOPIC,predict(model3.step.2.step.no.gender,type = "response"))
plot(result, print.auc = TRUE)




###MODEL INTERPRETATION

se = sqrt(vcov(model3.step.2.step.no.gender)['MOMMY1', 'MOMMY1'] + 
            vcov(model3.step.2.step.no.gender)['DADMY1', 'DADMY1'] - 
            2 * vcov(model3.step.2.step.no.gender)['DADMY1', 'MOMMY1'])
pnorm(model3.step.2.step.no.gender$coefficients[['MOMMY1']] - 
        model3.step.2.step.no.gender$coefficients[['DADMY1']], se)



SPORTHR.seq = seq(min(myopia$SPORTHR), max(myopia$SPORTHR))

newdata = data.frame(SPORTHR = SPORTHR.seq,
                     male.parents_good = predict(model3.step.2.step.no.gender, newdata = with(myopia,
                      data.frame(GENDER = factor(0), ACD = mean(ACD), SPORTHR = SPORTHR.seq,
                      READHR = mean(READHR), MOMMY = factor(0), DADMY = factor(0))), type = 'response'),
                     female.parents_good = predict(model3.step.2.step.no.gender, newdata = with(myopia, 
                      data.frame(GENDER = factor(1), ACD = mean(ACD), SPORTHR = SPORTHR.seq,
                      READHR = mean(READHR), MOMMY = factor(0), DADMY = factor(0))), type = 'response'),
                     male.parents_myopic = predict(m.s.2.s.no.gender, newdata = with(myopia, 
                      data.frame(GENDER = factor(0), ACD = mean(ACD), SPORTHR = SPORTHR.seq,
                      READHR = mean(READHR), MOMMY = factor(1), DADMY = factor(1))), type = 'response'),
                     female.parents_myopic = predict(m.s.2.s.no.gender, newdata = with(myopia, 
                      data.frame(GENDER = factor(1), ACD = mean(ACD), SPORTHR = SPORTHR.seq,
                      READHR = mean(READHR), MOMMY = factor(1), DADMY = factor(1))), type = 'response'))

library(ggplot2)
ggplot(data = newdata, aes(x = SPORTHR)) + 
  geom_line(aes(y = male.parents_good,col = 'male, parents good')) + 
  geom_line(aes(y = female.parents_good, col = 'female, parents good')) + 
  geom_line(aes(y = male.parents_myopic, col = 'male, parents myopic')) + 
  geom_line(aes(y = female.parents_myopic, col = 'female, parents myopic')) + 
  ylab("Probability")

