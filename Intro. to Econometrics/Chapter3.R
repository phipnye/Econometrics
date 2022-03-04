library(tidyverse)
library(wooldridge)
#bwght = b0 + b1cigs + b2faminc + u
#What is the most likely sign for b2?
#b2 is likely positive since higher income generally leads to better health
#and nutrition, which generally leads to heavier children at birth


#Do you think cigs and faminc are likely to be correlated?
#Possibly, if anything I would guess a negative correlation

#Now, estimate the equation with and without faminc, using the data in BWGHT.
lm(bwght ~ cigs, bwght)
#bwght = 119.7719 - 0.5138 * cigs
lm(bwght ~ cigs + faminc, bwght)
#bwght = 116.97413 - 0.46341 * cigs + 0.09276 * faminc

#Include the sample size and R-squared
summary(lm(bwght ~ cigs, bwght))$r.squared
#R^2 = 0.02272912
NROW(bwght$bwght)
#n = 1388

summary(lm(bwght ~ cigs + faminc, bwght))$r.squared
#R^2 = 0.02980484
#n = 1388

#Use the data in HPRICE1 to estimate the model
#price = b0 + b1sqrft + b2bdrms + u
#where price is the house price measured in thousands of dollars
#Write out the results in equation form.
lm(price ~ sqrft + bdrms, hprice1)
#price = -19.3150 + 0.1284 * sqrft + 15.1982 * bdrms
15.1982 * 1000
#What is the estimated increase in price for a house with one more
#bedroom, holding square footage constant?
#$15198.20 increase

#What is the estimated increase in price for a house with an additional
#bedroom that is 140 square feet in size?
15.1982 * 1000 + 140 * 1000 * 0.1284
#$33174.20 increase

#What percentage of the variation in price is explained by square
#footage and number of bedrooms?
summary(lm(price ~ sqrft + bdrms, hprice1))$r.squared
#R^2 = 0.6319184
#About 63.2% of the variation is explained for in this model by the ind. var.

#The first house in the sample has sqrft = 2,438 and bdrms = 4.
#Find the predicted selling price for this house from the OLS regression line.
-19.3150 + 0.1284 * 2438 + 15.1982 * 4
#$354157

#The actual selling price of the first house in the sample was $300,000
#(so price = 300). Find the residual for this house.
300 - (-19.3150 + 0.1284 * 2438 + 15.1982 * 4)
#residual = -54.147
#Does it suggest that the buyer underpaid or overpaid for the house?
#Not necessarily, it just suggest our estimated model predicts

#Estimate a model relating annual salary to firm sales and market value.
#Make the model of the constant elasticity variety for both independent
#variables. Write the results out in equation form.
view(ceosal2)
lm(lsalary ~ lsales + lmktval, ceosal2)
#log(salary) = 4.6209 + 0.1621 * log(sales) + 0.1067 * log(mktval)

#Add profits to the model from part (i)
lm(lsalary ~ lsales + lmktval + profits, ceosal2)
#log(salary) = 4.687 + 0.1614 * log(sales) + 0.09573 * log(mktval) + .00003566 * profits

#Why can this variable not be included in logarithmic form?
sum(as.integer(ceosal2$profits < 0))
#Because there are 9 observations where the profits are negative

#Would you say that these firm performance variables explain most of
#the variation in CEO salaries?
summary(lm(lsalary ~ lsales + lmktval + profits, ceosal2))$r.squared
#R^2 = 0.2993366
#No, these variables only account for about 30% of the variation

#Add the variable ceoten to the model in part (ii)
lm(lsalary ~ lsales + lmktval + profits + ceoten, ceosal2)
#log(salary) = 4.558 + .1622 * log(sales) + .1018 * log(mktval) + .00002905 * profits + .01168 * ceoten

#What is the estimated percentage return for another year of CEO tenure,
#holding other factors fixed
#1.168%

#Find the sample correlation coefficient between the variables
#log(mktval) and profits
cov(ceosal2$lmktval, ceosal2$profits) / (sd(ceosal2$lmktval) * sd(ceosal2$profits))
#corr(log(mktval), profits) = 0.7768976

#Are these variables highly correlated?
#Yes, they are pretty highly correlated
#What does this say about the OLS estimators?
#It's likely there's a lot of variance in our OLS estimators
#This does NOT suggest they are biased

#Use the data in ATTEND for this exercise
#Obtain the minimum, maximum, and average values for the variables
#atndrte, priGPA, and ACT
min(attend$atndrte)
#6.25
max(attend$atndrte)
#100
mean(attend$atndrte)
#81.70956
min(attend$priGPA)
#0.857
max(attend$priGPA)
#3.93
mean(attend$priGPA)
#2.586775
min(attend$ACT)
#13
max(attend$ACT)
#32
mean(attend$ACT)
#22.51029

#Estimate the model: atndrte = b0 + b1priGPA + b2ACT + u
lm(atndrte ~ priGPA + ACT, attend)
#atndrte = 75.700 + 17.261 * priGPA - 1.717 * ACT

#What is the predicted atndrte if priGPA = 3.65 and ACT = 20?
75.700 + 17.261 * 3.65 - 1.717 * 20
#104.3627

cnt <- 0
for(i in seq_along(attend$ACT)){
  if(attend$ACT[i] == 20 & near(attend$priGPA[i], 3.65, .01)){
    cnt <- cnt + 1
  }
}
cnt
rm(i,cnt)
#Yes, there's one observation with ACT = 20 and priGPA = 3.65

#If Student A has priGPA = 3.1 and ACT = 21 and Student B has
#priGPA = 2.1 and ACT = 26, what is the predicted difference in their
#attendance rates?
17.261 * (3.1 - 2.1) - 1.717 * (21 - 26)
#25.846

#Confirm the partialling out interpretation of the OLS estimates by
#explicitly doing the partialling out for Example 3.2. This first
#requires regressing educ on exper and tenure and saving the residuals,
#r1_hat.
r1_hat <- unname(lm(educ ~ exper + tenure, wage1)[["residuals"]])

#Then, regress log(wage) on r1_hat.
lm(lwage ~ r1_hat, wage1)
#log(wage) = 1.62327 + 0.09203 + r1_hat

#Compare the coefficient on r1_hat with the coefficient on educ in the
#regression of log(wage) on educ, exper, and tenure
lm(lwage ~ educ + exper + tenure, wage1)
#log(wage) = 0.284360 + 0.092029 * educ + 0.004121 * exper + 0.022067 * tenure

#Use the data set in WAGE2 for this problem
#Run a simple regression of IQ on educ to obtain the slope coefficient
lm(IQ ~ educ, wage2)[["coefficients"]][2]
#3.533829

#Run the simple regression of log(wage) on educ,
#and obtain the slope coefficient
lm(lwage ~ educ, wage2)[["coefficients"]][2]
#0.0598392

#Run the multiple regression of log(wage) on educ and IQ, and obtain the
#slope coefficients
lm(lwage ~ educ + IQ, wage2)[["coefficients"]][2:3]
#0.039119899, 0.005863131

#verify the bias equation holds
#such that beta1_tilda = beta1_hat + beta2_hat * delta1
#delta1 = 3.533829
#beta1_tilda = 0.0598392
#beta1_hat = 0.039119899
#beta2_hat = 0.005863131
0.039119899 + 0.005863131 * 3.533829
#0.0598392 == 0.0598392
#indeed, the equality holds