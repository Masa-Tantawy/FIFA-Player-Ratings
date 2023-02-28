# Applied Regression Methods Project
# Masa Tantawy -- 900201312, 
# Malak Gaballa -- 900201683

library(lattice); library(fpc)
library(ggplot2); library(DescTools); library(broom)
library(olsrr); library(MASS); library(stats)

df= read.csv("FIFA Cleaned.csv", header=T); df=df[-1]
#names=df$name; attach(df)
dim(df); str(df); #View(df)
player_names= df$full_name
df= df[,-2]

#--------- STEP 1: Validating assumptions before fitting the model ------------
# 1.1: Index plot of each predictor variable
pdf(file="1.1 Predictors index plots.pdf")
op <- par(mfrow = c(2,2))  # 2 x 2 matrix of plots
for ( i in 2:ncol(df) ){
  plot(df[,i], pch=19, ylab= colnames(df)[i])
}
par(op)
dev.off() #turn off PDF plotting 

# 1.2: Plot of response variable vs each predictor variable
pdf(file="1.2 Response vs Predictors.pdf")
op <- par(mfrow = c(2,2))  # 2 x 2 matrix of plots
for ( i in 2:ncol(df) ){
  plot(df[,i], df$overall_rating, pch=19, ylab="Y", xlab= colnames(df)[i])
}
par(op)
dev.off() #turn off PDF plotting 

# 1.3: Histogram  and box plot of the response variable
hist(df$overall_rating, xlab = "Overall Rating",
     main= "Histogram of the Response Variable")
boxplot(df$overall_rating, pch=19, xlab = "Overall Rating",
        main= "Histogram of the Response Variable", horizontal=T)

# 1.4: Summary of each predictor variable
for ( i in 2:ncol(df) ){
  min= min(df[,i]); max= max(df[,i])
  mean= mean(df[,i]); median= median(df[,i])
  sd= sqrt(var(df[,i]))
  cat(colnames(df)[i],"\n", "Mean= ", mean,"\tMedian= ", median)
  cat( "\nMin= ", min, "\tMax=", max)
  cat( "\nStandard Deviation= ", sd)
  cat("\n\n")
}
cor=cor(df)
write.csv(cor,"Correlation Matrix.csv", row.names = T)

#----------- STEP 2: Adjustments to match assumptions-----------------
# 2.1:Transformations
# First, choosing the lambda for each variable
# (variables chosen based on plots in 1.2)
lambda=c(-2,-1.5,-1,-0.5,0, 0.5,1,1.5,2)
power.transf=function(x,lambda){
  op <- par(mfrow = c(3,3))  # 3 x 3 matrix of plots
  for(i in 1:9){
    df1=x^lambda[i] 
    if(lambda[i]==0) {df1=log(x)}
    plot(df1,df$overall_rating, pch=19,main=paste("lambda =",lambda[i]),
         xlab= )
  }
  par(op)
}
pdf(file="Transformation.pdf")
power.transf(df$value_euro,lambda);mtext("value_euro", side = 3, line = -1, outer = TRUE)
power.transf(df$wage_euro,lambda);mtext("wage_euro", side = 3, line = -1, outer = TRUE)
power.transf(df$release_clause_euro,lambda);mtext("release_clause_euro", side = 3, line = -1, outer = TRUE)
dev.off() #turn off PDF plotting 

# Second, transforming the variables:
df$value_euro = log(df$value_euro) 
df$wage_euro = log(df$wage_euro)
df$release_clause_euro = log(df$release_clause_euro)

# 2.2: standerdizing the  columns
for (i in c(1:7,32:ncol(df))){ 
  df[,i]=scale(df[,i])
}

#----------------- STEP 3: Fitting the models--------------------
# 3.1: Dropping a base for each categorical variable
# base categories: ir1, weak_foot_1, skills_2, work_rate_LL
df= df[,-c(9,14,19,23)]

#---------------------------- ITERATION 1-----------------------------
# 3.2: fitting a model of the response variable vs all the other predictors
m1=lm(overall_rating~.,data=df)
#summary(m1)

# 3.3: verifying assumption before making any conclusions
n=nrow(df)        # Sample size
beta_hat=m1$coef  # Estimated regression coefficients
yhat=m1$fitted   # Fitted values
e=m1$resid       # Ordinary residuals
IStudRes=rstandard(m1) # Internally studentized residuals
EStudRes=rstudent(m1)  # Externally studentized Residuals
p=m1$rank-1      # Number of predictors
d.f=m1$df        # Residuals degrees of freedom = n - p - 1

# 3.3.1: Plots after fitting the model
pdf(file="M1 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 1");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df) ){
  plot(df[,i], IStudRes, pch=19, xlab= colnames(df)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals") 
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m1),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m1),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m1)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 1", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

# It is visible from the qq-plot that there are outliers
outliers_qq = c(which(IStudRes >(5)), which(IStudRes <(-5)))


colour= rep("black", nrow(df)); colour[outliers_qq]= "red"
pdf(file="M1 Outliers.pdf")
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df) ){
  plot(df[,i],IStudRes, pch=19, xlab= colnames(df)[i], col=colour)
}
par(op)
dev.off() #turn off PDF plotting 

#---------------------------- ITERATION 2 -------------------------------
# Now: points "outliers_qq" will be removed from the data set
# it is visible that these points are outliers in the residuals vs X plots 
df= df[-outliers_qq,]; player_names= player_names[-outliers_qq]
m2= lm(overall_rating~., data=df)

# 3.3: verifying assumption before making any conclusions
n=nrow(df)        # Sample size
beta_hat=m2$coef  # Estimated regression coefficients
yhat=m2$fitted   # Fitted values
e=m2$resid       # Ordinary residuals
IStudRes=rstandard(m2) # Internally studentized residuals
EStudRes=rstudent(m2)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M2 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 2");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df) ){
  plot(df[,i], IStudRes, pch=19, xlab= colnames(df)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals") 
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m2),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m2),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m2)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 2", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

outliers_x= which(Hinf$p >0.2)
colour= rep("black", nrow(df)); colour[outliers_x]= "red"
pdf(file="M2 Outliers.pdf")
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df) ){
  plot(df[,i],IStudRes, pch=19, xlab= colnames(df)[i], col=colour)
}
par(op)
dev.off() #turn off PDF plotting 

#---------------------------- ITERATION 3-------------------------
# Now: points "outliers_x" will be removed from the data set
# it is visible that these points are the only ones with ir=5, so this variable will also be removed
df2= df[-outliers_x,-12];
m3= lm(overall_rating~., data=df2)
#summary(m3)

# 3.3: verifying assumption before making any conclusions
n=nrow(df2)        # Sample size
beta_hat=m3$coef  # Estimated regression coefficients
yhat=m3$fitted   # Fitted values
e=m3$resid       # Ordinary residuals
IStudRes=rstandard(m3) # Internally studentized residuals
EStudRes=rstudent(m3)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M3 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 3");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df2) ){
  plot(df2[,i], IStudRes, pch=19, xlab= colnames(df2)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals") 
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m2),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m2),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m3)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Resiuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 3", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting
#--> No improvement; back to model 2

#---------------------------- ITERATION 4-----------------------------
# adding interaction between height and skill moves
df_new=df
df_new$height_skills3= df_new$height_cm* df_new$skills_3
df_new$height_skills4= df_new$height_cm* df_new$skills_4
df_new$height_skills5= df_new$height_cm* df_new$skills_5

m4=lm(overall_rating~., data=df_new)
# 3.3: verifying assumption before making any conclusions
n=nrow(df_new)        # Sample size
beta_hat=m4$coef  # Estimated regression coefficients
yhat=m4$fitted   # Fitted values
e=m4$resid       # Ordinary residuals
IStudRes=rstandard(m4) # Internally studentized residuals
EStudRes=rstudent(m4)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M4 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 4");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df_new) ){
  plot(df_new[,i], IStudRes, pch=19, xlab= colnames(df_new)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals")
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m4),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m4),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m4)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Residuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 4", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

summary(m4)

#---------------------------- ITERATION 5---------------------
# adding interaction between height and preffered foot
df_new=df
df_new$height_pf= df_new$height_cm* df_new$preferred_foot
m5=lm(overall_rating~., data=df_new)
# 3.3: verifying assumption before making any conclusions
n=nrow(df_new)        # Sample size
beta_hat=m5$coef  # Estimated regression coefficients
yhat=m5$fitted   # Fitted values
e=m5$resid       # Ordinary residuals
IStudRes=rstandard(m5) # Internally studentized residuals
EStudRes=rstudent(m5)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M5 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 5");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df_new) ){
  plot(df_new[,i], IStudRes, pch=19, xlab= colnames(df_new)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals")
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m5),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m5),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m5)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Residuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 5", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

summary(m5)
#---------------------------- ITERATION 6-----------
# adding interaction between height and international reputation
df_new=df
df_new$height_ir2= df_new$height_cm* df_new$ir2
df_new$height_ir3= df_new$height_cm* df_new$ir3
df_new$height_ir4= df_new$height_cm* df_new$ir4
df_new$height_ir5= df_new$height_cm* df_new$ir5
m6=lm(overall_rating~., data=df_new)
# 3.3: verifying assumption before making any conclusions
n=nrow(df_new)        # Sample size
beta_hat=m6$coef  # Estimated regression coefficients
yhat=m6$fitted   # Fitted values
e=m6$resid       # Ordinary residuals
IStudRes=rstandard(m6) # Internally studentized residuals
EStudRes=rstudent(m6)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M6 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 6");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df_new) ){
  plot(df_new[,i], IStudRes, pch=19, xlab= colnames(df_new)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals")
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m6),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m6),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m6)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Residuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 6", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

summary(m6)
#---------------------------- ITERATION 7--------------------------
# adding interaction between height and weak foot
df_new=df
df_new$height_wf2= df_new$height_cm* df_new$weak_foot_2
df_new$height_wf3= df_new$height_cm* df_new$weak_foot_3
df_new$height_wf4= df_new$height_cm* df_new$weak_foot_4
df_new$height_wf5= df_new$height_cm* df_new$weak_foot_5
m7=lm(overall_rating~., data=df_new)
# 3.3: verifying assumption before making any conclusions
n=nrow(df_new)        # Sample size
beta_hat=m7$coef  # Estimated regression coefficients
yhat=m7$fitted   # Fitted values
e=m7$resid       # Ordinary residuals
IStudRes=rstandard(m7) # Internally studentized residuals
EStudRes=rstudent(m7)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M7 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 7");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df_new) ){
  plot(df_new[,i], IStudRes, pch=19, xlab= colnames(df_new)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals")
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m7),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m7),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m7)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Residuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 7", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

summary(m7)
#---------------------------- ITERATION 8---------------
#adding interaction between height and work rate
df_new=df
df_new$height_wr2= df_new$height_cm* df_new$work_rate_LM
df_new$height_wr3= df_new$height_cm* df_new$work_rate_LH
df_new$height_wr4= df_new$height_cm* df_new$work_rate_ML
df_new$height_wr5= df_new$height_cm* df_new$work_rate_MM
df_new$height_wr6= df_new$height_cm* df_new$work_rate_MH
df_new$height_wr7= df_new$height_cm* df_new$work_rate_HL
df_new$height_wr8= df_new$height_cm* df_new$work_rate_HM
df_new$height_wr9= df_new$height_cm* df_new$work_rate_HH

m8=lm(overall_rating~., data=df_new)
# 3.3: verifying assumption before making any conclusions
n=nrow(df_new)        # Sample size
beta_hat=m8$coef  # Estimated regression coefficients
yhat=m8$fitted   # Fitted values
e=m8$resid       # Ordinary residuals
IStudRes=rstandard(m8) # Internally studentized residuals
EStudRes=rstudent(m8)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M8 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 8");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df_new) ){
  plot(df_new[,i], IStudRes, pch=19, xlab= colnames(df_new)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals")
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m8),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m8),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m8)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Residuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 8", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

summary(m8)
#---------------------------- ITERATION 9---------------------
df_new=df
bin= rep(0, nrow(df_new)); bin[df_new$height_cm>0.5]=1
df_new$height_bin= bin

m9=lm(overall_rating~., data=df_new)
# 3.3: verifying assumption before making any conclusions
n=nrow(df_new)        # Sample size
beta_hat=m9$coef  # Estimated regression coefficients
yhat=m9$fitted   # Fitted values
e=m9$resid       # Ordinary residuals
IStudRes=rstandard(m9) # Internally studentized residuals
EStudRes=rstudent(m9)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M9 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 9");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df_new) ){
  plot(df_new[,i], IStudRes, pch=19, xlab= colnames(df_new)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals")
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m9),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m9),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m9)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Residuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 9", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

summary(m9)
#---------------------------- ITERATION 10-------
#adding interaction between age and value euro
df_new=df
df_new$age_value= df_new$age * df_new$value_euro

m10=lm(overall_rating~., data=df_new)
# 3.3: verifying assumption before making any conclusions
n=nrow(df_new)        # Sample size
beta_hat=m10$coef  # Estimated regression coefficients
yhat=m10$fitted   # Fitted values
e=m10$resid       # Ordinary residuals
IStudRes=rstandard(m10) # Internally studentized residuals
EStudRes=rstudent(m10)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M10 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 10");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df_new) ){
  plot(df_new[,i], IStudRes, pch=19, xlab= colnames(df_new)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals")
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m10),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m10),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m10)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Residuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 10", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

summary(m10)
#---------------------------- ITERATION 11 ------- 
#adding interaction between age and potential
df_new$age_potential= df_new$age * df_new$potential

m11=lm(overall_rating~., data=df_new)
# 3.3: verifying assumption before making any conclusions
n=nrow(df_new)        # Sample size
beta_hat=m11$coef  # Estimated regression coefficients
yhat=m11$fitted   # Fitted values
e=m11$resid       # Ordinary residuals
IStudRes=rstandard(m11) # Internally studentized residuals
EStudRes=rstudent(m11)  # Externally studentized Residuals

# 3.3.1: Plots after fitting the model
pdf(file="M11 Plots.pdf")
# 1. Checking linearity: refer to 1.2 above
# 2. Q-Q Plot of internally standardized residuals
qqnorm(IStudRes, ylab = "IStudRes",pch=19, sub= "Model 11");
qqline(EStudRes, datax = FALSE, distribution = qnorm,
       probs = c(0.25, 0.75), qtype = 7)
abline(col="red", v=0, h=0);
# 3. Standardized residuals against each predictor variable
op <- par(mfrow = c(2,2))
for ( i in 2:ncol(df_new) ){
  plot(df_new[,i], IStudRes, pch=19, xlab= colnames(df_new)[i],
       ylab="IStudRes", main="Residuals vs X")
}
par(op)
# 4. Standardized residuals against fitted values
plot(yhat, IStudRes, pch=19, xlab="Fitted Values", ylab="IStudRes",
     main="Residuals vs Y hat")
# 5. Index Plot of the standardized residuals
plot(IStudRes, pch=19, xlab="Index", ylab="IStudRes",
     main="Standerdized Residuals")
# 6.Checking Influence:
op <- par(mfrow = c(2,2)) 
# Cooks Distance
plot(cooks.distance(m11),pch=19,xlab="Index",ylab="Cook's Distance")
plot(ols_leverage(m11),pch=19,xlab="Index",ylab="Leverage Values")
# Hadi's Influence
Hinf=ols_hadi(m11)    
plot(Hinf$hadi, pch=19, ylab="Hadi's Influence", main="Hadi's Influence")
# Potential-Residual plot
plot(Hinf$res,Hinf$p,pch=19,xlab="Residuals",ylab="Potential")  # PR Plot
title("Potentia-Residual Plot")
mtext("Model 11", side = 3, line = -1, outer = TRUE)
par(op)
dev.off() #turn off PDF plotting

summary(m11)
#--------------------------- ITERATION 12 ------------
# 3.3.2: Checking for collinearity
vif= ols_vif_tol(m11)[,-2] # collinearity exists
X=as.matrix(df_new[,-1]);   R=cor(X)
e=eigen(R); L=e$val; V=e$vec; kappa=sqrt(L[1]/L)

# Principle component regression
W= X%*%V  # Principal components
colnames(W)=paste("W",(1:ncol(W)),sep="")  # Gives names to the columns of W
df_PC=data.frame(df_new$overall_rating, W)
colnames(df_PC)[1]= colnames(df)[1]
# Fitting a preliminary model; there should be no change
m12 = lm(overall_rating~., data=df_PC)
summary(m12)

# Removing the insignificant Ws
insignificant=c(15, 19, 20, 28, 41, 44, 54,55, 58, 60)+1
df_PC= df_PC[,-insignificant] #insignificant variables
m12= lm(df$overall_rating~W)
summary(m12)
alpha= as.matrix(m12$coef);alpha=alpha[-1]
beta_pc=V[,-insignificant]%*%alpha[-insignificant] # PC Regression Coefficients
y_hat_pc=X%*%beta_pc       # PC Regression Fitted Values

plot(y_hat_pc,df_PC$overall_rating, pch=19, main="Model 12" )
abline(col="red", v=0, h=0)


