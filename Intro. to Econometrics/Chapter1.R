#install.packages("wooldridge")
library(wooldridge)
library(tidyverse)

# Use the data in WAGE1 for this exercise.
View(wage1)

#Find the average education level in the sample.
mean(wage1$educ)
#12.56274

#What are the lowest and highest years of education?
max(wage1$educ)
#highest level = 18
min(wage1$educ)
#lowest level = 0

#Find the average hourly wage in the sample.
mean(wage1$wage)
#5.896103
#Does it seem high or low?
#It seems low

#The wage data are reported in 1976 dollars.
#Using the Internet or a printed source, find the 
#Consumer Price Index (CPI) for the years 1976 and 2013.
cpi_1976 <- 55.6
cpi_2013 <- 230.280
inflation_7613 <- (cpi_2013 - cpi_1976) / cpi_1976
(inflation_7613)
#Use the CPI values from part (iii) to find the average hourly wage in 2013 dollars.
mean(wage1$wage) * inflation_7613
#18.52394
#Now does the average hourly wage seem reasonable?
#Yes, the average hourly wage seems much more reasonable now

#How many women are in the sample?
sum(wage1$female)
#252
#How many men?
NROW(wage1$female) - sum(wage1$female)
#274

# Use the data in BWGHT to answer this question.
View(bwght)
#How many women are in the sample?
NROW(bwght$faminc)
#1388
#And how many report smoking during pregnancy?
sum(as.integer(bwght$cigs > 0))
#212 (assuming cigs > 0 denotes reported smoking)

# What is the average number of cigarettes smoked per day?
mean(bwght$cigs)
#2.087176
#Is the average a good measure of the “typical” woman in this case? Explain.
#No, this average is for woman who are pregnant, not necessarily the "typical" woman

#Among women who smoked during pregnancy,
#what is the average number of cigarettes smoked per day?
sum(bwght$cigs) / sum(as.integer(bwght$cigs > 0))
#13.66509
#How does this compare with your answer from part (ii), and why?
#The average was heavily diluted in part (ii) by women who didn't smoke

# Find the average of fatheduc in the sample.
mean(bwght$fatheduc)
#returns NA, removing NA observations
mean(bwght$fatheduc, na.rm = TRUE)
#13.18624
#Why are only 1,192 observations used to compute this average?
#Because several observations had NA as an entry

# Report the average family income and its standard deviation in dollars.
mean(bwght$faminc) * 1000
#$29026.66
sd(1000 * bwght$faminc)
#18739.28

#The data in MEAP01 are for the state of Michigan in the year 2001.
#Use these data to answer the following questions.
View(meap01)
#Find the largest and smallest values of math4.
max(meap01$math4)
#100
min(meap01$math4)
#0
#Does the range make sense? Explain.
#Yes, these are percentages

# How many schools have a perfect pass rate on the math test?
sum(as.integer(meap01$math4 == 100))
#38
# What percentage is this of the total sample?
sum(as.integer(meap01$math4 == 100)) / NROW(meap01$math4)
#0.02084476

#How many schools have math pass rates of exactly 50%
sum(as.integer(meap01$math4 == 50))
#17

#Compare the average pass rates for the math and reading scores.
mean(meap01$math4)
#71.909
mean(meap01$read4)
#60.06188
#Which test is harder to pass?
#It appears the reading test is harder to pass

#Find the correlation between math4 and read4.
cov(meap01$math4, meap01$read4) / (sd(meap01$math4) * sd(meap01$read4))
#0.8427281
#What do you conclude?
#There's a pretty strong correlation between passing one test and passing the other
#It seems likely that if you pass 1, you'll pass both

# The variable exppp is expenditure per pupil. 
#Find the average of exppp along with its standard deviation.
mean(meap01$exppp)
#5194.865
sd(meap01$exppp)
#1091.89

#Would you say there is wide variation in per pupil spending?
#Yes, a std. deviation of about 20% of the avg. spending per pupil seems
#pretty substantial

#Suppose School A spends $6,000 per student and School B spends $5,500 per student.
#By what percentage does School A’s spending exceed School B’s?
(6000 - 5500) / 5500
#0.09090909
#Compare this to 100 * [log(6,000) – log(5,500)],
#which is the approximation percentage difference based on the difference
#in the natural logs.
100 * (log(6000) - log(5500))
#8.701138

# The data in JTRAIN2 come from a job training experiment
#conducted for low-income men during 1976–1977
View(jtrain2)
#Use the indicator variable train to determine the fraction of men receiving job training
sum(jtrain2$train)
#185

# The variable re78 is earnings from 1978, measured in thousands of 1982
# dollars. Find the averages of re78 for the sample of men receiving job
# training and the sample not receiving job training.
sm <- 0
nm <- 0
for(i in seq_along(jtrain2$train)){
  if(jtrain2$train[i] == 1){
    sm <- sm + jtrain2$re78[i]
    nm <- nm + 1
  }
}
(sm / nm)
#avg for those who received training: 6.349145
sm <- 0
nm <- 0
for(i in seq_along(jtrain2$train)){
  if(jtrain2$train[i] == 0){
    sm <- sm + jtrain2$re78[i]
    nm <- nm + 1
  }
}
(sm / nm)
#avg for those who didn't receive training: 4.554802
rm(sm, nm)
#Is the difference economically large?
#Yes, I would say $1794.34 is economically large

# The variable unem78 is an indicator of whether a man is unemployed
#or not in 1978. What  fraction of the men who received job training
#are unemployed?
den <- 0
num <- 0
for(i in seq_along(jtrain2$train)){
  if(jtrain2$train[i] == 1){
    den <- den + 1
    num <- num + jtrain2$unem78[i]
  }
}
num / den
#0.2432432
den <- 0
num <- 0
for(i in seq_along(jtrain2$train)){
  if(jtrain2$train[i] == 0){
    den <- den + 1
    num <- num + jtrain2$unem78[i]
  }
}
num / den
rm(num,den, i)
#What about for men who did  not receive job training?
#0.3538462

#Comment on the difference
#About 11% lower unemployment rate for those who received job training

#From parts (ii) and (iii), does it appear that the job training program was effective?
#Somewhat, it's difficult to give a hard answer because there are so many other factors involved
#What would make our conclusions more convincing?
#If we could create a ceteris paribus condition to test on

#The data in FERTIL2 were collected on women living in the Republic of Botswana
#in 1988. The variable children refers to the number of living children.
#The variable electric is a binary indicator equal to 
#one if the woman’s home has electricity, and zero if not.
View(fertil2)
#Find the smallest and largest values of children in the sample.
max(fertil2$children)
#13
min(fertil2$children)
#0
#What is the average of children?
mean(fertil2$children)
#2.267828

#What percentage of women have electricity in the home?
100 * (sum(fertil2$electric, na.rm = TRUE) / sum(!is.na(fertil2$electric)))
#14.02019%

# Compute the average of children for those without electricity
# and do the same for those with electricity.
n <- 0
d <- 0
for(i in seq_along(fertil2$electric)){
  if(as.logical(fertil2$electric[i]) & !is.na(fertil2$electric[i])){
    n <- n + fertil2$children[i]
    d <- d + 1
  }
}
n / d
#avg for those w/ electricity: 1.898527
n <- 0
d <- 0
for(i in seq_along(fertil2$electric)){
  if(!as.logical(fertil2$electric[i]) & !is.na(fertil2$electric[i])){
    n <- n + fertil2$children[i]
    d <- d + 1
  }
}
n / d
rm(i,n,d)
#avg for those w/o electricity: 2.327729
#Comment on what you find.
#On average, women without electricity have more children

#From part (iii), can you infer that having electricity “causes” women to have fewer children?
#Certainly not, the ceteris paribus condition has not been sufficiently satisfied
#Thus, causal effect cannot be derived from this result

#Use the data in COUNTYMURDERS to answer this question.
#Use only the year 1996. The variable murders is the number of murders
#reported in the county. The variable execs 
#is the number of executions that took place of people sentenced to death
#in the given county. Most states in the United States have the death penalty,
#but several do not
View(countymurders)
county_murders <- as_tibble(countymurders) %>%
  filter(year == 1996)

#How many counties are there in the data set?
NROW(unique(county_murders$countyid))
#2197
#Of these, how many have zero murders? What percentage of counties
#have zero executions? (Remember, use only the 1996 data.)
county_murders <- group_by(county_murders, countyid) %>%
  mutate(
    yn_murders = as.integer(sum(murders, na.rm = T) > 0)
  )
NROW(county_murders$yn_murders) - sum(county_murders$yn_murders)
#1051
100 * 1051 / 2197
#47.83796%

#What is the largest number of murders?
max(county_murders$murders)
#1403
#What is the largest number of executions?
max(county_murders$execs)
#3
#Compute the average number of executions and explain why it is so small
mean(county_murders$execs)
#0.01593081, it's so small people executions are rarely performed

#Compute the correlation coefficient between murders and execs and describe what you find.
cov(county_murders$execs, county_murders$murders) / (sd(county_murders$execs) * sd(county_murders$murders))
#0.2095042, there's a slight but not overwhelming positive correlation in this sample

# The data set in ALCOHOL contains information on a sample of men in the United States.
#Two key variables are self-reported employment status and alcohol abuse (along with many other variables). 
#The variables employ and abuse are both binary, or indicator, variables:
#they take on only the values zero and one.
View(alcohol)

#What percentage of the men in the sample report abusing alcohol? 
sum(alcohol$abuse) / NROW(alcohol$abuse) * 100
#9.916514%

#What is the employment rate?
(1 - sum(alcohol$employ) / NROW(alcohol$abuse)) * 100
#10.18123%

#Consider the group of men who abuse alcohol.
#What is the employment rate?
n <- 0
d <- 0
for(i in seq_along(alcohol$abuse)){
  if(as.logical(alcohol$abuse[i])){
    n <- n + (1 - alcohol$employ[i])
    d <- d + 1
  }
}
n / d * 100
#12.73101%

# What is the employment rate for the group of men who do not abuse alcohol?
n <- 0
d <- 0
for(i in seq_along(alcohol$abuse)){
  if(!as.logical(alcohol$abuse[i])){
    n <- n + (1 - alcohol$employ[i])
    d <- d + 1
  }
}
n / d * 100
rm(n,d,i)
#9.900542

#Discuss the difference in your answers to parts (ii) and (iii).
#Does this allow you to conclude that alcohol abuse causes unemployment?
#In this sample, the unemployment rate was higher for those who abuse alcohol
#This does not imply causality, just a positive correlation

#The data in ECONMATH were obtained on students from a large university
#course in introductory microeconomics. For this problem, we are
#interested in two variables: score, which is the final course 
#score, and econhs, which is a binary variable indicating whether a
#student took an economics course in high school
View(econmath)

#How many students are in the sample?
NROW(econmath$age)
#856
#How many students report taking an economics course in high school?
sum(econmath$econhs)
#317

#Find the average of score for those students who did take a high school economics class.
n <- 0
d <- 0
for(i in seq_along(econmath$econhs)){
  if(as.logical(econmath$econhs[i])){
    n <- n + econmath$score[i]
    d <- d + 1
  }
}
n / d
#avg for those who did take econ in hs: 72.07593
#How does it compare with the average of score for those who did not?
n <- 0
d <- 0
for(i in seq_along(econmath$econhs)){
  if(!as.logical(econmath$econhs[i])){
    n <- n + econmath$score[i]
    d <- d + 1
  }
}
n / d
#avg for those who did take econ in hs: 72.90792
rm(n,d,i)

#Do the findings in part (ii) necessarily tell you anything about
#the causal effect of taking high school economics on college course peformance?
#No, ceteris paribus effect has not been established

# If you want to obtain a good causal estimate of the effect of taking 
#a high school economics course using the difference in averages,
#what experiment would you run?
#I would conduct a controlled experiment that randomly assigns students to
#take or not take a hs econ class and that test them again and compare the avg scores