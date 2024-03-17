############## Week 11 Lab Logistic Regression #########################
# We will build a model to predict if a customer will purchase the product (MYDEPV: purchase the product==1) given their age and income and the "Price" that the company may offer (10, 20 or 30 dollars). 
#The company would like to use this information to determine not only a good base price but also whether or not certain groups might be 
# willing to purchase the product if sent a special offer.


###################################################
# Step 1: Set the Working Directory
###################################################
#set working directory

###################################################
# Step 2: Read in and Examine the Data
###################################################
myData <- read.csv("survey.csv",header=TRUE, stringsAsFactors = TRUE)

#1. Explore data: Use table() and summary() functions to understand the variables. 
#MyDEPV is the dependent variable, and it means whether or not a customer buy the product. 

# table(myData)
summary(myData)

# create correlation matrix using Mydata after dropping the first column since it is the dependent variable.
cor.mat <- cor(myData[,-1])
cor.mat

# transform Price variable as a factor variable. 
## Use relevel Function to re-level the Price factor with value 30 as the base reference.
myData$Price <-as.factor(myData$Price)
myData$pricefactor = relevel(as.factor(myData$Price), "30")

##2. split training/test data
set.seed(2)
train.index <- sample(c(1:dim(myData)[1]), dim(myData)[1]*0.6)  
trainData <- myData[train.index, ]
validationData <- myData[-train.index, ]

###################################################
#Step 3: Build and Review the Logistic Regression Model
###################################################

#3 Build a logistic regression model using the train dataset: MYDEPV is the dependent variable.  

mylogit <- glm(MYDEPV ~ Income + Age + Price,
           data = trainData, family = binomial(link="logit"),
           na.action=na.pass)
#4 Print out the summary of the mylogit model
summary(mylogit)


# One unit change of Income, the log odds of Purchase increases by 0.12 
# Compared with a Price of 10, Purchase decision at a Price 20 decreases the log odds of Purchase by 0.58

#log odds ratio of purchase = (-6)   + 0.12*Income +0.032*Age +  (-0.58)*Price20 + (-2)*Price30 
#logA = ax + by + c
# A = exp(ax + by +c)


# We will now change the reference level at price point 30 then run the logistic model again using the training data. 
# this will change the factor variable base level to 30 to see the effect of this baseline to the model. Compared to the results from a different base level (10), 
#this result shows that customers are more likely to purchase the product. 
mylogit2 = glm(MYDEPV ~ Income + Age + pricefactor  ,
            data = trainData, family = binomial(link="logit"), na.action=na.pass)
summary(mylogit2)

###################################################
#Step 4: variable selection: 
#5: use step function to decide the best variables included in the model mylogit2.
stepModel <- step(mylogit2)

#############################################
#This data includes only four predictor variables, including the new variable pricefactor we just created. 
#Use all the independent variables to build a model mylogit3.
mylogit3 <- glm(MYDEPV ~ ., data = trainData, family = binomial(link = "logit"), na.action = na.pass)

summary(mylogit3)
#6. run step() function to find the best variables that should be included in the model mylogit3. 
step(mylogit3)

###################################################
#Step 5: Rerun Logistic Regression
###################################################

#7. Create mylogit4 by building a logistic regression model using the variables you selected as a result of #6
#Warning: because the variables are too small, variable selection does not influence the model
#performance in this case. But we do this as a practice. 

mylogit4 <- glm(MYDEPV ~ pricefactor + Income + Age, data = trainData, family = binomial(link = "logit"), na.action = na.pass)

summary(mylogit4)

###################################################
#Step 6: Model Evaluation using Confusion Matrix
###################################################
##Evaluate the model using confusion matrix. Think about using the right threshold based on the goal of the project
#load the library caret
library(caret)

#8: predict the value for the validation dataset using the predict function and mylogit4 model you just built. You should set the type="response" to have probability value predicted.
pred <- predict(mylogit4, validationData, type = "response")

#9: build confusion matrix using confusionMatrix() function. Set the threshold as 0.5 (and with different values). What is the accuracy of the model? 
plot(pred)
confusionMatrix(as.factor(ifelse(pred < 0.8, "0", "1")), as.factor(validationData$MYDEPV))

###################################################
#Step 7: Use Logistic Regression as a Classifier
###################################################
#10: Classify new data using the model you just built
set.seed(2)
#create a new data 
newdata2 <- data.frame(Age= round(runif(10,min(myData$Age),max(myData$Age))),
                        Income= round(runif(10,min(myData$Income),max(myData$Income))),
                        pricefactor = as.factor(round((runif(10,10,30)/10))*10))

newdata2$Prob <- predict(mylogit4, newdata2, type = "response")