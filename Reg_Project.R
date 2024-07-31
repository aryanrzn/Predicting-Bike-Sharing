#Libraries
library(ggplot2)
library(fastDummies)
library(lmtest)
library(MASS)
library(faraway)
library(car)
#Data
Bike_Sharing <- read.csv("/Users/rezanejad/Desktop/Reg Project/day.csv",header=T)
#Structure of Data
str(Bike_Sharing)
summary(Bike_Sharing)

#Cleaning Data
Bike_Sharing_New = Bike_Sharing[,c("season", "yr", "mnth", "weekday","workingday", "weathersit",
                                 "temp", "atemp", "windspeed","casual")]
dim(Bike_Sharing_New)
Bike_Sharing_Dummy <- dummy_cols(Bike_Sharing_New, select_columns = c("season","yr","weekday","mnth","weathersit"), TRUE)
Bike_Sharing_Dummy <- subset(Bike_Sharing_Dummy, select = -c(season_1,weekday_0,yr_0,mnth_1,weathersit_dim(Bike_Sharing_Dummy)
#Histogram                              
qplot(Bike_Sharing_New$casual, geom="histogram", binwidth = 5, main ="Histogram of count of daily rental bikes", 
      xlab ="count of daily rental bike", fill=I("red"), col=I("red"))+ theme_classic()

#Pair Plot
pairs(~.,data=Bike_Sharing_New,main="Scatterplot Matrix")
library(corrplot)
par(mfrow=c(1,1))
cor_matrix <- cor(Bike_Sharing_New,method = "pearson",use = "complete.obs")
corrplot(cor_matrix,method = "circle", type = "upper" , addCoef.col = "black")

#Calculating VIF
model = lm(casual ~ ., data = Bike_Sharing_Dummy)
#cor(Bike_Sharing_Dummy)which(vif(model)>10)
Bike_Sharing_Dummy = subset(Bike_Sharing_Dummy, select = -c(atemp,season_3))
r_squared <- summary(model)$r.squared
adjusted_r_squared <- summary(model)$adj.r.squared
f_statistic <- summary(model)$fstatistic[1]
p_value <- summary(model)$fstatistic[4]
cat("R squared is", r_squared,"\n","Adjusted R squared is", adjusted_r_squared,"\n","F-statistics is", f_statistic,"\n",
    "P_value is smaller than 2.2e-16")

#Model evaluation
model_1 = lm(casual ~ ., data = Bike_Sharing_Dummy)

#Checking equal variance assumption
bptest(model_1)

#Checking normality assumption
shapiro.test(resid(model_1))

#Cook’s distance
CD = cooks.distance(model_1)
which(CD > 4/length(CD))

#Cook’s distances with threshold = 4/n
out_i = which( CD > 4/length(CD))
Bike_Sharing_Dummy = Bike_Sharing_Dummy[-out_i,]

#Logarithmic transformation
model_3 = lm(log(casual) ~ ., data = Bike_Sharing_Dummy)
bptest(model_3)
shapiro.test(resid(model_3))
out_i = which(cooks.distance(model_3)>4 / length(cooks.distance(model_3)))
Bike_Sharing_Dummy = Bike_Sharing_Dummy[-out_i,]

#Box Cox Transformation
bc = boxcox(model_1)
lambda <- bc$x[which.max(bc$y)]
#Fit new linear regression model using the Box-Cox transformation 
model_4 <-lm(((casual^lambda-1)/lambda) ~ ., data = Bike_Sharing_Dummy)
out_i = which(cooks.distance(model_4)>4 / length(cooks.distance(model_4)))
Bike_Sharing_Dummy = Bike_Sharing_Dummy[-out_i,]
model_4 <- lm(((casual^lambda-1)/lambda) ~ ., data = Bike_Sharing_Dummy)
bptest(model_4)
shapiro.test(resid(model_4))

#Variable Selection
model_5= step(lm(((casual^(lambda) - 1)/(lambda))~1, data = Bike_Sharing_Dummy),scope
=((casual^(lambda) - 1)/(lambda)) ~ temp + workingday + yr_1 + season_4 + weathersit_2
+ mnth_3 + weathersit_3 + mnth_4 + weekday_6 + mnth_10 + mnth_9 + mnth_5 +
windspeed + weekday_3 + mnth_11 + weekday_2 + weekday_4 + weekday_1 + mnth_8
+ mnth_6 + mnth_7 + mnth_12 + mnth_2 , direction = "both",trace = 0)
model_5 = lm(((casual^(lambda) - 1)/(lambda)) ~ temp + workingday + yr_1 +mnth_2
+ mnth_4 + mnth_10 + weathersit_3 + weathersit_2 + mnth_5 + mnth_3 + mnth_9
+ mnth_6 + mnth_8 + mnth_7 + mnth_11 + mnth_12 + weekday_3 + weekday_6 +
weekday_2 + weekday_4 + weekday_1 + windspeed ,data = Bike_Sharing_Dummy)
summary(model_5)
bptest(model_5)
shapiro.test(resid(model_5))

#Semi_Final_Model and after that.
#Final model 
model <- lm(((casual^(lambda) - 1) / lambda) ~ temp +
              workingday + yr_1 + mnth_2 + mnth_4 + mnth_10 +
              weathersit_3 + weathersit_2 + mnth_5 + mnth_3 +
              mnth_9 + mnth_6 + mnth_8 + mnth_7 + mnth_11 + 
              mnth_12 + weekday_3 + weekday_6 + weekday_2 + 
              weekday_4 + weekday_1 + windspeed, data =
              Bike_Sharing_Dummy) 

#Quadratic effect   
#model without quadratic form 
model_6 <- lm(formula = ((casual^(lambda) - 1)/(lambda)) ~ temp +
                workingday + yr_1 + mnth_2 + mnth_4 + mnth_10 +
                weathersit_3 + weathersit_2 + mnth_5 + mnth_3 + mnth_9 +
                mnth_6 + mnth_8 + mnth_7 + mnth_11 + mnth_12 + weekday_3 +
                weekday_6 + weekday_2 + weekday_4 + weekday_1 + windspeed,
              data = Bike_Sharing_Dummy) 
summary(model_6) 
bptest(model_6) 
shapiro.test(resid(model_6))  

#Fit the first model without the quadratic term 
model_6 <- lm(formula = ((casual^(lambda) - 1)/(lambda)) ~ temp +
                workingday + yr_1 + mnth_2 + mnth_4 + mnth_10 +
                weathersit_3 + weathersit_2 + mnth_5 + mnth_3 + mnth_9 +
                mnth_6 + mnth_8 + mnth_7 + mnth_11 + mnth_12 + weekday_3 +
                weekday_6 + weekday_2 + weekday_4 + weekday_1 + windspeed,
              data = Bike_Sharing_Dummy)   

#Fit the second model with the quadratic term 
model_7 <- lm(formula = ((casual^(lambda) - 1)/(lambda)) ~ temp +
                workingday + yr_1 + mnth_2 + mnth_4 + mnth_10 +
                weathersit_3 + weathersit_2 + mnth_5 + mnth_3 + mnth_9 +
                mnth_6 + mnth_8 + mnth_7 + mnth_11 + mnth_12 + weekday_3 +
                weekday_6 + weekday_2 + weekday_4 + weekday_1 + windspeed +
                I(temp^2), data = Bike_Sharing_Dummy) 
bptest(model_7) 
shapiro.test(resid(model_7))  
bptest(model_6) 
shapiro.test(resid(model_6))  
anova(model_6, model_7)  
summary(model_6)$r.squared 
summary(model_6)$adj.r.squared 
summary(model_7)$r.squared 
summary(model_7)$adj.r.squared  
summary(model_7)$fstatistic 
pf(summary(model_7)$fstatistic[1], summary(model_7)$fstatistic[2],
   summary(model_7)$fstatistic[3], lower.tail = FALSE)  
plot(x = fitted(model_7), y = residuals(model_7), xlab = "Fitted
        Values", ylab = "Residuals", main = "Residual vs. Fitted
        Values Plot") 
abline(h = 0, col = "red", lty = 2)  
std_residuals <- rstandard(model_7) 
qqnorm(std_residuals, main = "Q-Q Plot of Standardized Residuals")
qqline(std_residuals, col = "red") 
scale_location_plot <- plot(model_7, which = 3) 
plot(model_7, which = 5) 
abline(h = c(0.5, 1), col = "red", lty = 2) 
influencePlot(model_7, id.method = "identify", main = "Influence
        Plot with Cook's distance")