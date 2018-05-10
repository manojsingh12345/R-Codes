library(caTools)
library(gridExtra)
library(ggplot2)
library(splitstackshape)
library(caret)
library(zoom) 
library(dplyr)
library(psych)

set.seed(100)

#setwd("F:\\AA_TSK\\Marvel_DS\\Hw2")
df <- read.csv("Data.csv")

dput(colnames(df))

Columns <- c("Motor", "Screw", "Pgain", "Vgain")

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

# Exploring Data
p1 <- qplot(x = df$Class, geom = "histogram", bins = 20, col=I("black"), 
            xlab = "Class")

p2 <- qplot(x = 1:length(df$Class),y = df$Class, geom = "point", 
            xlab = "Observation", ylab = "Class" )

p3 <- qplot(y = df$Class, x = "", geom = "boxplot", xlab = "Class")

grid.arrange(p1, p2, p3, ncol=3, newpage = FALSE)
zm()

"IF we look at the aove plots It appears that there is an Outliear ,
But we Can't be sure as we have no idea about business. It could be an anamoly

To explore that we will build the model with Outlier/Anamoly and without that
and Will compare the results and will reach a conclusion"

#============================== Outlier Treatment ============================

# Setting Outlier = TRUE removes the outlier and Setting Outlier = FALSE keeps the outlier 
Outlier <- TRUE # TRUE or FALSE  

if(Outlier ==TRUE)
  {
df<-subset(x = df, df$Class < quantile(df$Class, 0.999))

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

If we don't do stratified split its quite possible that one level is missing in
the train-set and that level has gone into test-set and thus model will give wrong result
"

Test_set1<-stratified(df, "Motor", size = 1, keep.rownames = T)
Test_set2<-stratified(df, "Screw", size = 1 , keep.rownames = T)
Test_set3<-stratified(df, "Pgain", size = 1 , keep.rownames = T)
Test_set4<-stratified(df, "Vgain", size = 1 , keep.rownames = T)

Test_data <- rbind(Test_set1, Test_set2, Test_set3, Test_set4) # Test Set Consists of Row number

head(Test_data)

# Removing Test data from sample to keep the Train set free of test set for fair testing

Train_set <- df[-as.numeric(Test_data$rn),]   

Test_set<- Test_data[,-1]  # Removing Row number from test data

summary(Train_set)
summary(Test_set)

"If we look at the summary of Train and Test set we can see that train and test
both consists of all the levels as a result of statified sampling 
and about ~ 20% of the data is dedicated for Testing"


# ============ Class in the Actual data set for Comparing with Model outcome======

Y_actual <- Test_set$Class
std_y_actual <- sd(Y_actual) # Standard deviation of Y_actual  is 16.4926

#======================= Building Model ============================

model <- lm(formula = Class ~., data = Train_set)

summary(model)

"Multiple R-squared:  0.8689,	Adjusted R-squared:  0.8443 
If we look at the Model summary at least one of the levels has shown
p - value < .05, which shows that they hold significance"


model$coefficients
model$residuals

qplot(x = 1:length(model$residuals), y = model$residuals , geom = "point")

# Above Error plot is randomly distributed around zero

qplot( x = model$residuals , geom = "histogram", bins = 10)

# Above Error histogram is normally distributed around mean zero

qplot(x = Train_set$Class, y = model$residuals , geom = "point")

"Above Error vs Y plot shows most of the data is scattered around zero
 Although some pattern can be seen in Y and error residuals which may be due to 
 presence of nonlinearity which linear model is not able to explain"

#===========================Prediction on Test set data ==============

Y_predicted <-predict(model, newdata = Test_set[,-5])

postResample(pred = Y_predicted, obs = Y_actual)

#qplot(x = Y_predicted, y = Y_actual , geom = "point", xlab = "Predicted", ylab = "Actual")

plot(Y_predicted,Y_actual,  lwd = 2, pch=21,  bg="black") #col = "red",
lines(1:50,1:50, type = "l", col = "green", lwd = 2)  #45 degree Line
zm()

# plot(Y_predicted -Y_actual)
# hist(Y_predicted -Y_actual )

qplot(x = 1:length(Y_predicted), y = Y_predicted -Y_actual , geom = "point")

# Above Error plot is randomly distributed around zero

hist( Y_predicted -Y_actual)


# Above Error histogram is  distributed around mean nearly zero

zm()

"Discussion"

#====================================================================
#Model With Outlier (Metrics on Test Set) Outlier <- FALSE

"
RMSE      Rsquared       MAE 
8.1910192 0.7752484 5.8630084 
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






