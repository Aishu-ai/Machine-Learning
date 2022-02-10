#Libraries specifically installed
#knitr
#GGally
#regclass
#Metrics
#dpylr

#Reading the data
abalone=read.csv("Abalone_data.csv")
#Checking if the object is factor or not[factor in the sense categorical variables] Here sex i of 3 types, male,female and infant
is.factor(abalone$Sex)
#If not factor make it a factor
abalone$Sex <- as.factor(abalone$Sex)
#displays the internal structure of an object
str(abalone, give.attr = FALSE)
library(knitr)
#shows first 10 rows of data in a table form
kable(abalone[1:10,], digits = 4,format = 'markdown')
#can use head also(but we do not get a table here)
#head(abalone)

#gives mean,median etc. of each variable
summary(abalone)
library(GGally)
#build a great scatter-plot matrix
ggpairs(abalone, aes(colour = Sex, alpha = 0.8), title="Pairs plot for abalone dataset") + 
  theme_grey(base_size = 8)
#Adding Non-infant and infant(as M and F plots are almost the same)
abalone['Infant'] <- ifelse(abalone$Sex == 'I','I','NI')
abalone$Infant <- as.factor(abalone$Infant)
abalone$Sex <- as.factor(abalone$Sex)
str(abalone)

#Train and test split
set.seed(42)
#Splitting dataset randomly in train and test using 70/30 method
indexes <- sample(1:nrow(abalone), size = 0.3 * nrow(abalone))
abalone_train <- abalone[-indexes,]
abalone_test <- abalone[indexes,]


#Since age is usually rings+1.5, rings is taken as the dependent variable
#Additive Multiple Linear Regression Model
abalone_add <- lm(Rings ~ Sex+Length+Diameter+Height+ Whole_weight
                  +Shucked_weight+Viscera_weight
                  +Shell_weight,data = abalone_train)
summary(abalone_add)
#Multicollinearity:
library(regclass)
VIF(abalone_add)
#High correlation for whole_Weight and diameter is found


#Partial correlation coefficient between Whole_weight & Rings
#check variability in high collinearity variables
whole_weight_fit <- lm(Whole_weight ~ Sex + Length + Diameter + Height + Shucked_weight + Viscera_weight + Shell_weight, data=abalone_train)

abalone_add_without_whole_weight <- lm(Rings ~ Sex + Length + Diameter + Height
                                       + Shucked_weight + Viscera_weight + Shell_weight,data = abalone_train)
#correlation coefficient
cor(resid(whole_weight_fit),resid(abalone_add_without_whole_weight))

#Variance inflation factor of the additive model without the Whole_weight
VIF(abalone_add_without_whole_weight)
#Partial correlation coefficient between Diameter & Rings(without whole weight)
diameter_fit <- lm(Diameter ~ Sex + Length + Height + Shucked_weight + Viscera_weight + Shell_weight, data=abalone_train)

abalone_add_small <- lm(Rings ~ Sex + Length + Height + Shucked_weight + Viscera_weight + Shell_weight,data = abalone_train)
cor(resid(diameter_fit),resid(abalone_add_small))
VIF(abalone_add_small)
#is smaller for variables than abalone_add model

#F test to chose b/w Ho =abalone_add_small(without diameter and whole_weight)
#Ha=abalone_add(all variables)
anova(abalone_add_small,abalone_add)
#(null hypothesis..i.e rejects abalone_add_small)

#Running AIC on additive model(selects the best variables)
abalone_model_add_aic <- step(abalone_add, direction="backward", trace=0)
summary(abalone_model_add_aic)
#AIC selects the abalone_add without the length predictor and Sex-M also has less significance

#Function for residual plot
create_fitted_residuals <- function(model,name){
  plot(fitted(model), resid(model), col = "grey", pch = 20,
       xlab = "Fitted", ylab = "Residuals", main = paste("Fitted vs Residuals (",name,")"))
  abline(h = 0, col = "darkorange", lwd = 2)
}

#normal q-q plot
create_qq_normal <- function(model,name){
  qqnorm(resid(model), main = paste("Normal Q-Q Plot (",name,")"), col = "darkgrey")
  qqline(resid(model), col = "dodgerblue", lwd = 2)
}

#combines the qq and residual plot
model_assumptions <- function(model,name){
  par(mfrow = c(1, 2),oma=c(0,0,0,0))
  create_fitted_residuals(model,name)
  create_qq_normal(model,name)

}

#qq and residual plot for abalone_add without length
model_assumptions(abalone_model_add_aic,"abalone_model_add_aic")
#shows fan out effect

p <- length(coef(abalone_add))
n <- length(resid(abalone_add))
#regsubsets gives best models of all sizes, rss and adjusted R^2 etc.. of each model
all_abalone_add <- summary(regsubsets(Rings ~ Sex + Length + Diameter + Height + Whole_weight + Shell_weight + Shucked_weight + Viscera_weight , data=abalone_train))

#AIC values for each model
abalone_mod_aic <- n * log(all_abalone_add$rss / n) + 2 * (2:p)
#abalone_mod_aic

#plotting the AIC values against the number of variables each model
plot(abalone_mod_aic ~ I(2:p), ylab = "AIC", xlab = "p, number of parameters", 
     pch = 20, col = "dodgerblue", type = "b", cex = 2,
     main = "AIC vs Model Complexity")
#It's seen model with 8 variables gives least AIC value. 
#The analysis with Infant- I and NI(2 categorical variables is same as M,F,I as M and F have almost the same effect)
#To reduce the heteroscedasticity, we use log transforming the response i.e we use log(Rings)
#Additive Model with log response transformations

abalone_add_log_inf <- lm(log(Rings) ~ Infant + Length + Diameter + Height + Whole_weight + Shucked_weight + Viscera_weight + Shell_weight,data = abalone_train)
summary(abalone_add_log_inf)

model_assumptions(abalone_add_log_inf,"abalone_add_log_inf")
#The fan-out effect decreases (constant variance issue improves and its also much closer to normality)

library(orcutt)
#To address auto-correlation
coch = cochrane.orcutt(abalone_add_log_inf)
summary(coch)
#initial autocollinearity was not much but after remedy the Durbin-Watson factor goes to the normal range(1.5-2.5)
model_assumptions(coch,"coch")
library(Metrics)

#To calculate root mean square error(RMSE) for log transformation
log_rmse <- function(model,name){
  abalone_add_train_rmse  <- sqrt(mean((abalone_train$Rings - exp(fitted(model))) ^2))
  abalone_add_test_rmse <- sqrt(mean((abalone_test$Rings - exp(predict(model, newdata=abalone_test))) ^ 2))
  result <- data.frame('Model'=c(name),"RMSE Train"=c(abalone_add_train_rmse),"RMSE Test"=c(abalone_add_test_rmse))
}

#RMSE for training and test data
kable(log_rmse(abalone_add_log_inf,"Additive Log Model"), digits = 4,format = 'markdown')
kable(log_rmse(coch,"Additive Log Model with auto-correlation correction"), digits = 4,format = 'markdown')
#RMSE for model "without autocorrelation correction" gives less error

VIF(abalone_add_log_inf)
#Compromising on "no multicollineraity"


#Dropping columns weight.diff and infant from test data
dropcol <- c("weight.diff","Infant")
test_data <- abalone_test[, !(abalone_test %in% dropcol)]
library(dplyr)

#Taking 5 random samples from test data
sample <- sample_n(test_data, 5)

#predicted values
predicted <- round(exp(predict(abalone_add_log_inf, newdata=sample)))

#Age of abalone=1.5+number of rings
new_df <- data.frame("Actual no of Rings" = c(sample$Rings), 
                     "Predicted no of Rings" = c(predicted),
                     "Actual age of abalone" = c(round(sample$Rings + 1.5)), 
                     "Predicted age of abalone" = round(predicted + 1.5))

kable(new_df, digits = 4,format = 'markdown') 
#Confidence Interval
exp(predict(abalone_add_log_inf, newdata=sample,interval="confidence"))

#The model has almost constant variance, is close to being normal, has less auto correlation(Durvin-Watson factor isaround 1.36). It has
#high collinearity but the prediction intervals are in the same range so it doesnt matter much.

