# ALY6015
# Group Delta
# Final Project: Initial Analysis Report

# Un-comment to install package as needed
#install.packages("ggplot2")
#install.packages("corrplot")
#install.packages("ggcorrplot")
#install.packages("ggforce")
#install.packages("ggbeeswarm")
#install.packages("FSA")
#install.packages("DescTools")
#install.packages("caTools")

library(ggplot2)  # advanced histograms plotting 
library(corrplot) # correlation matrix visualization
library(ggcorrplot)
library(RColorBrewer) # colors
library(dplyr) # for the pipe commands
library(leaps) # best subset regression
library(ggforce)
library(ggbeeswarm)
library(tidyverse)
library(psych)
library(FSA)
library(car)
library(caTools)
library(DescTools)
library(caret)

# Read banking data set. When prompted navigate and open banking.csv file
banking <- read.table(file.choose(), sep = ",", header = TRUE, stringsAsFactors = FALSE)

# EDA
# Displaying no. of rows and columns 
dim(banking)

# Displaying the structure of the data
str(banking)

# View summary information of data
summary(banking)

# Displaying the number of NULL values in the dataset
which(is.na(banking))

# Plotting a Barplot of Number of Clients by Call Success
ggplot(banking, aes(x = Target, fill = Target)) +
  geom_bar() + ggtitle("Barplot of Number of Clients by Call Success") + 
  xlab("Call Success") + ylab("Number of Clients") + theme(plot.title = element_text(hjust = 0.5))

# Plotting a Jitter Plot of Bank Balance and Marital Status
ggplot(data = banking) +
  aes(y = balance, x = marital) +
  geom_jitter() + ggtitle("Jitter Plot of Bank Balance and Marital Status") + 
  xlab("Marital Status") + ylab("Bank Balance") + theme(plot.title = element_text(hjust = 0.5))

# Plotting a Jitter Plot of Bank Balance and Education Level
ggplot(data = banking) +
  aes(y = balance, x = education) +
  geom_jitter() + ggtitle("Jitter Plot of Bank Balance and Education Level") + 
  xlab("Education Level") + ylab("Bank Balance") + theme(plot.title = element_text(hjust = 0.5))

# Plotting a Stacked Barplot of Number of clients by Profession and Success of Call
ggplot(banking, aes(x=job, fill=Target)) +
  geom_bar(position="stack", stat="count") +
  scale_fill_hue(c = 40) + ggtitle("Stacked Barplot of Number of clients by Profession and Loan Approval") +
  xlab("Job Type") + ylab("Number of Clients") + theme(plot.title = element_text(hjust = 0.5))

# Plotting a Stacked Barplot of Number of clients by Marital Status and Success of Call
ggplot(banking, aes(x=marital, fill=Target)) +
  geom_bar(position="stack", stat="count") +
  scale_fill_hue(c = 40) + ggtitle("Stacked Barplot of Number of clients by Marital Status and Success of Call") +
  xlab("Marital Status") + ylab("Number of clients") + theme(plot.title = element_text(hjust = 0.5))


# Plotting a Histogram of Age of Clients
hist(banking$age, main = "Histogram of Age of Clients", xlab = "Age of Clients")

##############################
##############################

# QUESTION: Is there a relationship between a bank member’s education level and their bank balance? 

#Some entries show negative balance
(debt = subset(banking,balance<0) %>% group_by(education) %>% tally() %>% data.frame())

#Checking class type of Balance variable
class(banking$balance)

#Checking Class type of Education Level variable
class(banking$education)

#Converting Character Variable into factors with different levels
banking$education=as.factor(banking$education)

#Checking updated Class of Education Level variable
class(banking$education)

#Boxplot of Balance vs Education
boxplot(banking$balance~banking$education,xlab="Education Level",ylab="Balance",main="Balance vs Education")

#Dataframe for number of occurances of each education level
(data2 = banking %>% group_by(education) %>% tally() %>% data.frame())

#Dataframe of balance for each education level
(newdata = banking %>% group_by(education) %>% summarize(sum(balance)) %>% data.frame())

#Bar plot of Balance by Education level
plot = barplot(newdata$sum.balance.,names.arg = newdata$education,main="Bar Plot of Balance by Education Level",xlab="Education Level",col=c("red","blue","yellow","green"),yaxt="n")

#Adding numbers to top of bars
text(plot, newdata$sum.balance.-1110000,paste(newdata$sum.balance.),cex=1.2)

#subset of education level primary
primary = subset(banking[,-c(1,2,3,5,7:17)],education=="primary")

#subset of education level secondary
secondary = subset(banking[,-c(1,2,3,5,7:17)],education=="secondary")

#subset of education level tertiary
tertiary = subset(banking[,-c(1,2,3,5,7:17)],education=="tertiary")

#subset of education level unknown
unknown = subset(banking[,-c(1,2,3,5,7:17)],education=="unknown")

#combining data frames
testing.data = rbind(primary,secondary,tertiary,unknown)

#mean balance for primary education
mean(primary$balance)

#mean balance for secondary education
mean(secondary$balance)

#mean balance for tertiary education
mean(tertiary$balance)

#median balance for primary education
median(primary$balance)

#median balance for secondary education
median(secondary$balance)

#median balance for tertiary education
median(tertiary$balance)

#ANOVA test to check relation between education level and bank balance

#Null Hyp, H0: There is no statistically significant relation between education level and bank balance
#Alt Hyp, Ha: There is statistically significant relation between education level and bank balance

#significance level
(a=0.05)

#running the anova test
anova = aov(balance~education,data=testing.data)

#view the model summary
summary(anova)

#save summary to an object
a.summary = summary(anova)

# DOF Num
d.f.numerator = a.summary[[1]][1,"Df"]
d.f.numerator

# DOF Den
d.f.denominator = a.summary[[1]][2,"Df"]
d.f.denominator

#Extract F test value
(F.value = a.summary[[1]][1,"F value"])

#Extract p value value
(p.value = a.summary[[1]][1,"Pr(>F)"])

#critical value
crit.val = qf(1-a, d.f.numerator, d.f.denominator)
crit.val

#comparing p-value with alpha
ifelse(p.value < a, "Reject H0", "cannot reject H0")

#Reject H0 

##############################
##############################

# QUESTION: Does 'campaign' (number of contacts performed before this campaign and for this client) 
# increase the chance of 'outcome' to result in failure?

# H0: There is no difference in mean outcome being failure, success, other or unknown
# H1: There is a difference in mean outcome being failure, success, others or unknown

# Setting significance level
a<-0.05

# Creating a matrix by extracting the two columns 'balance' and 'duration' 
mat<-subset(banking, select = c(campaign,poutcome))

# Displaying the matrix
#mat

#unique(mat$poutcome)

# Converting poutcome as factor
mat$poutcome<-as.factor(mat$poutcome)

# ANOVA results
anova<-aov(campaign ~ poutcome, data=mat)

# Summary of the test results
anova_summary<-summary(anova)

# Displaying the summary of the results
anova_summary

# Extracting DFN
dfn=anova_summary[[1]][1,"Df"]

# Extracting DFD
dfd=anova_summary[[1]][2,"Df"]

# Calculating Critical Value
cv=qf(1-a, dfn, dfd)

# Displaying Critical Value
cv

# Extracting P-value from the summary
pval<-anova_summary[[1]][[1,"Pr(>F)"]]

if(pval < a)
{
  # Print the decision
  print("Reject the Null Hypothesis. ")
  # Print the next step
  print("Performing Scheffe Test:")
  # Performing Scheffe Test as the sample sizes are not equal
  ScheffeTest(anova)
} else #fail to reject the null hypothesis
{
  # Print the decision
  print("Fail to reject the Null Hypothesis")
}

##############################################################################

# QUESTION: Is there a difference in the number of calls made to a customer during 
# both the campaigns (previous and current)?

# Null Hypothesis, H0: There is no difference in the number of times a customer is called 
# during the current and the previous campaigns
# Alternate Hypothesis H1: There is a difference in the number of times a customer is called 
# during the current and the previous campaigns.

# Setting significance level
a<-0.05

# Creating a dataframe by extracting the two columns 'campaign' and 'previous' 
mat<-data.frame(time=c(banking$campaign, banking$previous), 
                group=rep(c("Current","Previous"), 
                          times=c(length(banking$campaign),length(banking$previous))))

# Creating a boxplot
boxplot(time~group, data=mat)

# Conducting Wilcox test 
result<-wilcox.test(time~group, data=mat, alternative="greater", conf.level = 0.95, correct=FALSE, exact = FALSE)

# Displaying the result
result

# Printing the decision after comparing the results using if-else
# if the a>(result$p.value), Reject the null hypothesis
# else fail to reject the null hypothesis
ifelse(result$p.value > a, "Fail to reject the null hypothesis","Reject the Null Hypothesis")

##############################
##############################


# QUESTION: Which factor has the highest correlation with bank balance? 

## Correlation with all variables
model.matrix(~0+., data=banking) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)


### Correlation with just numeric variables ####

## Since most variables are characters, make a new dataframe with only numeric variables 
banking.numeric <- data.frame(banking$age, banking$balance, banking$day, banking$duration, 
                              banking$campaign, banking$pdays, banking$previous)


## Display few entries of the new banking data frame.
headTail(banking.numeric)


## Correlation matrix with only numeric variables 
Correlation.Balance <-cor(banking.numeric)
round(Correlation.Balance,3)


## Visualizing the correlation 
corrplot(Correlation.Balance, tl.cex= 0.5,
         method =  "shade", 
         addCoef.col = "Brown",
         title = "Correlation Matrix for Bank Balance with numeric variables",
         mar = c(1,1,1,1))


## Running regression with respect to Balance 
Regression.Model.Balance <- lm(banking.numeric$banking.balance ~ 
                                 banking.numeric$banking.age +
                                 banking.numeric$banking.duration)


## Summarize results 
summary(Regression.Model.Balance)


##### What variables impact the housing status 

# Create Train and Test set - maintain % of event rate (80/20 split) 
set.seed(123) 
trainIndex <- createDataPartition(banking$housing, p = 0.8, list = FALSE, times = 1) 

## Training Dataset
caret_train <- banking[ trainIndex,] 
dim(caret_train)

## Test Dataset
caret_test <- banking[-trainIndex,] 
dim(caret_test)


## Generalized Linear Regression with all variables to check significant variables 
Model1 <- glm(as.factor(housing) ~ ., data= caret_train, family= binomial(link="logit"))   ## running the regression
summary(Model1)
round(coef(Model1),3)
round(exp(coef(Model1)),3)


##############################
##############################

# QUESTION: What is the main cause of loans?

# Isolate certain variables
new_data = subset(banking, select = -c(contact, day, month, duration, campaign,
                                       pdays, previous, poutcome, Target))

# Correlation matrix
model.matrix(~0+., data=new_data) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

### HOUSING LOAN ###

# Highest correlation with housing loan - blue collar jobs

ggplot(data = banking) +
  aes(y = housing, x = job) +
  geom_sina() +
  coord_flip()

### PERSONAL LOAN ###

# Highest correlation with personal loan - credit card in default
# Biggest cause - secondary education

ggplot(data = banking) +
  aes(y = loan, x = education) +
  geom_sina() +
  coord_flip()

### Chi-Square Test of Independence

# Isolating the data about blue-collar jobs and housing loans with the value "yes"
housing_yes = new_data[new_data$job == "blue-collar" & new_data$housing == "yes",]
housing_yes = subset(housing_yes, select = c(job, housing))
housing_yes

# Number of occurrences
housing_yes_num = nrow(housing_yes)
housing_yes_num

# Isolating the data about blue-collar jobs and housing loans with the value "no"
housing_no = new_data[new_data$job == "blue-collar" & new_data$housing == "no",]
housing_no = subset(housing_no, select = c(job, housing))
housing_no

# Number of occurrences
housing_no_num = nrow(housing_no)
housing_no_num

# Isolating the data about secondary education and  loans with the value "yes"
loan_yes = new_data[new_data$education == "secondary" & new_data$loan == "yes",]
loan_yes = subset(loan_yes, select = c(education, loan))
loan_yes

# Number of occurrences
loan_yes_num = nrow(loan_yes)
loan_yes_num

# Isolating the data about secondary education and  loans with the value "no"
loan_no = new_data[new_data$education == "secondary" & new_data$loan == "no",]
loan_no = subset(loan_no, select = c(education, loan))
loan_no

# Number of occurrences
loan_no_num = nrow(loan_no)
loan_no_num

# Constructing the dataframe for Chi-Square Test of Independence

# H0: The presence of loans is independent on the blue-collar job type or secondary education..
# H1: The presence of loans is dependent on the blue-collar job type or secondary education.

# Setup significance level
alpha = 0.05

# Create a vector for each row
r1 = c(housing_yes_num, housing_no_num)
r2 = c(loan_yes_num, loan_no_num)

# State the number of rows for the matrix
rows = 2

# Create the matrix
matrx = matrix(c(r1, r2), nrow = rows, byrow = TRUE)

# Naming the rows and columns of the matrix
rownames(matrx) = c("Blue-collar Job", "Secondary Education")
colnames(matrx) = c("Yes Loan", "No Loan")

# View the matrix and confirm that it matched the data
matrx

# Run the Chi-square test
result = chisq.test(matrx)

# Critical value
qchisq(alpha, result$parameter, lower.tail = FALSE)

# View the results and p-value
result$statistic # Chi-square test value
result$p.value # Chi-square p-value
result$parameter # degrees of freedom
result

# Compare the p-value to alpha and make the decision
# Reject the null hypothesis
ifelse(result$p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

# Chi-square test of independence has shown that the p-value is < 2.2e-16 which
# is very close to zero and is less than our significance level. Therefore, we
# can conclude that the presence of loans is dependent on the blue-collar job type or secondary education.

################################

# QUESTION: Is there a difference between the balance of different job types?

# H0: there is no effect of the job type on the balance
# H1: job type affects the balance

# Alpha level
alpha = 0.05

# Isolating job types and balance variables
job_data = subset(banking, select = c(job, balance))

# Correlation matrix for job types and balance
model.matrix(~0+., data=job_data) %>% 
  cor(use="pairwise.complete.obs") %>% 
  ggcorrplot(show.diag=FALSE, type="lower", lab=TRUE, lab_size=2)

# ANOVA test
anova <- aov(formula= balance ~ job, data=job_data)

# Summary of the test
a.summary = summary(anova)
a.summary

# Degrees of freedom
# Between groups
df.numerator = a.summary[[1]][1, "Df"]
df.numerator

# Within groups
df.denominator = a.summary[[1]][2, "Df"]
df.denominator

# Critical value
qf(p=alpha, df1=df.numerator, df2=df.denominator, lower.tail=FALSE)

# Extract the F test value from the summary
F.value = a.summary[[1]][[1, "F value"]]
F.value

# Extract the p-value value from the summary
p.value = a.summary[[1]][[1, "Pr(>F)"]]
p.value

# Compare the p-value to alpha and make the decision
ifelse(p.value > alpha, "Fail to reject the null hypothesis", "Reject the null hypothesis")

# Reject H0

# Scatter plot for each job type and balance
ggplot(data = banking) +
  aes(y = balance, x = job) +
  geom_sina() +
  coord_flip()

