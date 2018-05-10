rm(list=ls())
library(caTools)
library(gridExtra)
library(ggplot2)
library(splitstackshape)
library(caret)
library(zoom) 
library(dplyr)
library(psych)
library(corrplot)
library(outliers)

#setwd("F:\\AA_TSK\\Marvel_DS\\Hw2")
df <- read.csv("Data.csv")

dput(colnames(df))

Columns <- c("Motor", "Screw", "Pgain", "Vgain")

print(colSums(is.na(df))) # Checking for missing values

# checking above we see No Missing Value


# Exploring data structure and summary
summary(df)
str(df)

df %>% summarise_all(funs(n_distinct(.)))

" if we see above all the predictors has only 4 and 5 
unique values in 115 observation which shows all the predictors all categorical "

describe(df)

# Converting the numeric predictors into factor variables

for(i in Columns)
{
  df[i] <- as.factor(unlist(df[i]))
}

#chisq.test(table(c(rep(1,10), rep(2,10)),c(rep(2,10), rep(4,10))))
#----- Checking Association between all the categorical variables (Chi- Square Test)------

Assocation_P_value <- matrix( nrow = 4, ncol = 4) #Creating a empty matrix
row.names(Assocation_P_value) <- Columns #Naming its row 
colnames(Assocation_P_value) <- Columns #Naming its column 

# Chi-square test for association 
for(i in Columns)
  for(j in Columns)
    Assocation_P_value[i,j] <-chisq.test(table(unlist(df[j]), unlist(df[i])), simulate.p.value = T)$p.value


significance_of_association <- 1-Assocation_P_value

print(significance_of_association)

corrplot(significance_of_association) 
zm()


"Above plot is not a typical correlation plot where you see correlation coefficent, 
The above plot signifies [1 - pvalue]. I have called it significance of association
if it is greater than .95 then we can say that two categorical variables are
associated with each other and we may choose to keep one of those" 

"if we look at the values in [significance_of_association] or corrplot we can say
that Pgain and Vgain are associated with each other and we may choose remove one of it"


#========== Lets take three scenario Keeping All, Keeping Pgain and Keeping Vgain ===============

Features <-"Vgain" #"Pgain" or "All",  # selecting one keeps that as features

if(Features == "All")
  df <- df[c("Motor", "Screw", "Pgain", "Vgain", "Class")]
if(Features == "Pgain")
  df <- df[c("Motor", "Screw","Pgain" , "Class")]
if(Features == "Vgain")
  df <- df[c("Motor", "Screw","Vgain" , "Class")]


# ========================= Exploring Data ============================

# Checking in the categorical variable if any of the class is minimal 

for(i in Columns)
  {
  print(i)
  print(prop.table(table(unlist(df[i]))))
  }  # Proportion of each level in all the variables

"if we look at above all of the levels in each variable holds significant 
proportion wrt each other so there is no outlier  in the categorical predictors variables"

# ---------------------------Exploring Target Variable ------------------

p1 <- qplot(x = df$Class, geom = "histogram", bins = 20, col=I("black"), 
            xlab = "Class")

p2 <- qplot(x = 1:length(df$Class),y = df$Class, geom = "point", 
            xlab = "Observation", ylab = "Class" )

p3 <- qplot(y = df$Class, x = "", geom = "boxplot", xlab = "Class")

grid.arrange(p1, p2, p3, ncol=3, newpage = FALSE)
zm()

"IF we look at the above plots It appears that there is an Outliear ,
But we Can't be sure as we have no idea about business. It could be an anamoly

To explore that we will build the model with Outlier/Anamoly and without that
and Will compare the results and will reach a conclusion"

#============================== Outlier Treatment ============================


# Setting Outlier = TRUE removes the outlier and Setting Outlier = FALSE keeps the outlier from the target variable

"Although i am not a big fan of outlier removal using statistiscal method, having said that
we can use z -test for checking if the apperant anamoly is outlier (statistical) "


Outlier <- TRUE # TRUE or FALSE  

if(Outlier == TRUE)
  {
df<-subset(x = df, scores(df$Class , type="z", prob=0.95) == FALSE)  #z-test with .95 confidance

# Exploring Data after outlier removal
p1 <- qplot(x = df$Class, geom = "histogram", bins = 20, col=I("black"), 
            xlab = "Class")
p2 <- qplot(x = 1:length(df$Class),y = df$Class, geom = "point", 
            xlab = "Observation", ylab = "Class" )
p3 <- qplot(y = df$Class, x = "", geom = "boxplot", xlab = "Class")

grid.arrange(p1, p2, p3, ncol=3)
zm()
}

#================ Splitting Train and Test Set (Stratified Split) ===========================

"As all the Predictor variabls are Categorical variable so stratified split is 
required to keep the Train Set and Test set all the levels 

If we don't do stratified split then its quite possible that one level is missing in
the train-set and that level has gone into test-set and thus model will give wrong result
"
 
set.seed(200)

if(Features == "All")
{
  
Test_set1<-stratified(df, "Motor", size = 2, keep.rownames = T)
Test_set2<-stratified(df, "Screw", size = 2 , keep.rownames = T)
Test_set3<-stratified(df, "Pgain", size = 2 , keep.rownames = T)
Test_set4<-stratified(df, "Vgain", size = 2 , keep.rownames = T)

Test_data <- rbind(Test_set1, Test_set2, Test_set3, Test_set4)  # Test set in case of all 
}

if(Features == "Pgain")
{
  Test_set1<-stratified(df, "Motor", size = 2, keep.rownames = T)
  Test_set2<-stratified(df, "Screw", size = 2 , keep.rownames = T)
  
  Test_set3<-stratified(df, "Pgain", size = 2 , keep.rownames = T)
  
  Test_data <- rbind(Test_set1, Test_set2, Test_set3)  # Test set in case of Pgain 
  
}

if(Features == "Vgain")
{
  Test_set1<-stratified(df, "Motor", size = 2, keep.rownames = T)
  Test_set2<-stratified(df, "Screw", size = 2 , keep.rownames = T)
  
  Test_set3<-stratified(df, "Vgain", size = 2 , keep.rownames = T)
  
  Test_data <- rbind(Test_set1, Test_set2, Test_set3)  # # Test set in case of Vgain 
  
}

head(Test_data)  # Test Set Consists of Row number #, Test_set4

# Removing Test data from sample to keep the Train set free of test set for fair testing, with the help of row number (rn)

Train_set <- df[-as.numeric(Test_data$rn),]   

Test_set<- Test_data[,-1]  # Removing Row number from test data

summary(Train_set)
summary(Test_set)

"If we look at the summary of Train and Test set we can see that train and test
both consists of all the levels as a result of statified sampling 
and about ~ 25-35% of the data is dedicated for Testing"


# ============ Actual target variable for Comparing with Model outcome======

Y_actual <- Test_set$Class
std_y_actual <- sd(Y_actual) # Standard deviation of Y_actual 

#======================= Building Model ============================

model <- lm(formula = Class ~., data = Train_set)

summary(model)

"Multiple R-squared:  0.5485,	Adjusted R-squared:  0.4532
If we look at the Model summary at least one of the levels has shown
p - value < .05, which shows that they hold significance"


model$coefficients
model$residuals

qplot(x = 1:length(model$residuals), y = model$residuals , geom = "point")

# Above Error plot is randomly distributed around zero

qplot( x = model$residuals , geom = "histogram", bins = 10)

# Above Error histogram looks normally distributed around mean zero to ckeck for normality, Q-Q plot

qqnorm(model$residuals)
qqline( model$residuals)

# if we look at above Q-Q plot we can say that error is Normal distribution with some devation

plot(x = Train_set$Class, y = model$residuals)
lines(1:50,rep(0,50), type = "l", col = "green", lwd = 2) # constant zero line

"Above Error vs Y plot shows most of the data is scattered around zero
 Although some pattern can be seen in Y and error residuals which may be due to 
 presence of nonlinearity which linear model is not able to explain"

sum(model$residuals) # Sum of residuals is near zero 

#===========================Prediction on Test set data ==============
#col_index

Y_predicted <-predict(model, newdata = Test_set)

postResample(pred = Y_predicted, obs = Y_actual)

#qplot(x = Y_predicted, y = Y_actual , geom = "point", xlab = "Predicted", ylab = "Actual")

plot(Y_predicted,Y_actual,  lwd = 2, pch=21,  bg="black") 
lines(1:50,1:50, type = "l", col = "green", lwd = 2)  #45 degree Line
zm()

# plot(Y_predicted -Y_actual)
# hist(Y_predicted -Y_actual )

qplot(x = 1:length(Y_predicted), y = Y_predicted -Y_actual , geom = "point")

# Above Error plot is randomly distributed around zero

hist( Y_predicted -Y_actual)

qplot( x = Y_predicted -Y_actual , geom = "histogram", bins = 5)


# Above Error histogram is expected to normally distributed around mean nearly zero if enough data is there

#To test for normality Q-Q plot

qqnorm(model$residuals)
qqline( model$residuals)
zm()

# if we look at the above plot it appears normally distributed

"Discussion"

#====================================================================
#Model With Outlier (Metrics on Test Set) Outlier <- FALSE # Keeping all

"
 RMSE     Rsquared       MAE 
8.0121506  0.7656868 5.8148780 
"
#====================================================================

#Model Without Outlier (Metrics on Test Set), Outlier <- TRUE
"
RMSE      Rsquared       MAE 
5.7269743 0.8925765 5.0303331
"

#====================================================================

"We look at the above metrics we can say that after removing the outlier
the model performance has increased significantly, so solely on the modeling 
point of view it is good to remove the outlier 
but its always good to confirm with business"

#==================================================================
# Conclusion

"if we compare the RMSE 5.7 which is smaller than
standard deviation 16.49 which can be interprated that model is 
able to explain the variation in data"






