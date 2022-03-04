library(wooldridge)
library(tidyverse)

k_401k <- as_tibble(k401k)
view(k_401k)

# Find the average participation rate and the average match rate in the sample of plans.
mean(k_401k$prate)
#87.36291
mean(k_401k$mrate)
#0.7315124

#Now, estimate the simple regression equation
lm(prate ~ mrate, k_401k)
#prate = 5.861 * mrate + 83.075

#And report the results along with the sample size and R-squared.
str(summary(lm(prate ~ mrate, k_401k)))
#n = 1534
#R^2 = 0.0747

#Interpret the intercept in your equation. Interpret the coefficient on mrate
#The intercept is the estimated participation rate when the plan match rate is 0
#The slope coefficient simply indicates an increase in prate by 5.681 per every additional unit in mrate

#Find the predicted prate when mrate = 3.5.
5.861 * 3.5 + 83.075
#103.5885
#Is this a reasonable prediction?
#No, a percentage greater than 100% does not make sense
#Explain what is happening here.
#The linear model's flaw of being unable to account for infeasible values is showing

#How much of the variation in prate is explained by mrate? Is this a lot in your opinion?
#~7.47%, which seems like quite a lot considering all of the other posible factors

ceo_sal2 <- as_tibble(ceosal2)
view(ceo_sal2)

#Find the average salary and the average tenure in the sample.
mean(ceo_sal2$salary)
#865.8644
mean(ceo_sal2$ceoten)
#7.954802

#How many CEOs are in their first year as CEO (that is, ceoten = 0)?
sum(as.integer(ceo_sal2$ceoten == 0))
#5

#What is the longest tenure as a CEO?
max(ceo_sal2$ceoten)
#37

# Estimate the simple regression model
lm(lsalary ~ ceoten, ceo_sal2)
#log(salary) = 6.505498 + 0.009724 * ceoten

#What is the (approximate) predicted percentage increase in salary given one more year as a CEO?
#~0.9724%

sleep_75 <- as_tibble(sleep75)
view(sleep_75)

#Report your results in equation form along with the number of observations and R2
str(summary(lm(sleep ~ totwrk, sleep_75)))
#sleep = 3586.3770 - 0.1507 * totwrk
#n = 706
#R^2 = 0.103

#What does the intercept in this equation mean?
#It estimates the sleep of someone not working

#If totwrk increases by 2 hours, by how much is sleep estimated to fall?
120 * -0.1507
#-18.084 minutes
# Do you find this to be a large effect?
#Not particularly, but some may find 18 minutes a big deal

wage_2 <- as_tibble(wage2)
view(wage_2)
#Use the data in WAGE2 to estimate a simple regression
#explaining monthly salary (wage) in terms of IQ score (IQ).
lm(wage ~ IQ, wage_2)
#wage = 116.992 + 8.303 * IQ

#Find the average salary and average IQ in the sample.
mean(wage_2$wage)
#957.9455
mean(wage_2$IQ)
#101.2824

#What is the sample standard deviation of IQ
sd(wage_2$IQ)
#15.05264

#Estimate a simple regression model where a one-point increase in IQ
#changes wage by a constant dollar amount. 
#wage = 116.992 + 8.303 * IQ

#Use this model to find the predicted increase in wage for an increase in 
#IQ of 15 points.
8.303 * 15
#$124.545 increse in wage for 15 extra IQ

#Does IQ explain most of the variation in wage?
str(summary(lm(wage ~ IQ, wage_2)))
#R^2 = 0.0955, so no IQ does not explain most of the variation in wage

#Now, estimate a model where each one-point increase in IQ has the
#same percentage effect on wage.
lm(lwage ~ IQ, wage_2)
#log(wage) = 5.886994 + 0.008807 * IQ

#If IQ increases by 15 points, what is the approximate percentage increase in predicted wage?
0.008807 * 15
#~0.132105 or 13.2105% increase

#Write down a model (not an estimated equation) that implies a constant
#elasticity between rd and sales. Which parameter is the elasticity?
#log(rd) = B_0 + B_1 * log(sales) + u
#B_1 is the elasticity

#Now, estimate the model using the data in RDCHEM.
#Write out the estimated equation in the usual form.
View(rdchem)
lm(lrd ~ lsales, rdchem)
#log(rd) = -4.105 + 1.076 * log(sales)

#What is the estimated elasticity of rd with respect to sales?
#elasticity ~ 1.076

#Explain in words what this elasticity means.
#For every percentage increase in sales, there's about a 1.076% increase in rd