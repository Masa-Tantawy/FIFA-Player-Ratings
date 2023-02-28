# Applied Regression Methods Project
# Masa Tantawy -- 900201312, 
# Malak Gaballa -- 900201683

# --------------------- DATA PREPARATION --------------------------
library(DescTools)
#install.packages("DMwR2")
library(DMwR2)
library(broom)

#setwd("~/Desktop/Fall 2022 Courses/Regression/Final Project")
df= read.csv("FIFA.csv", header=T, na.strings=c("")); names=df$name
#attach(df)
dim(df)
#View(df)

# 1.Omitting unexplained attributes and those for goalkeepers, name, ID, birthdate
df= df[,-c(1,2,4,9,19,21,23,27,30,60:92)]
#colnames(df)

# 2.Omitting goal keepers from the data set
df= df[(df$positions !="GK"),]

# 3.Omitting position variable as some players have multiple positions
df= df[,-5 ] 


# 4.Missing Values Imputation
missing= c()
for (x in 1:ncol(df)){ # detecting columns with missing values
  if (sum(is.na(df[x]))>0){
    missing= c(missing,x)
    cat(x,"\t",sum(is.na(df[x])),"\n")
    }
} # 9 columns with missing values
dim(df)
colnames(df)[missing]

# 4.1: Imputing missing values in numeric columns using kNN.
df2<-ImputeKnn(df[,c(7,8,14,15,16)],
               k = 10, scale = T, meth = "weighAvg", distData = NULL)
new_df=df
new_df[,c(7,8,14,15,16)] = df2[,]

# 4.2: Omitting the national rating & national team position because 
# they have a huge number of missing values 
# and omitting the 3 categorical columns with missing values
new_df= new_df[,-c(17,18,19,20)]

dim(new_df)
sum(is.na(new_df))
#summary(new_df)
#colSums(is.na(new_df))

# 5. Making the response variable the first column
names= c(colnames(new_df)[5], colnames(new_df)[-5])
new_df= new_df[,names]

# 6. Resampling:
plot(new_df$overall_rating, pch=19)
# Since the plot has a shape, we need to resample the data
new_df=new_df[sample(1:nrow(new_df), 15889, replace = F),]
set.seed(100)
plot(new_df$overall_rating, pch=19)

# 7. Breaking down the categorical variables into multiple indicator variables
new_df[,9]= as.factor(new_df[,9]);
new_df[,10]= as.factor(new_df[,10]); new_df[,11]= as.factor(new_df[,11]);
new_df[,12]= as.factor(new_df[,12]); new_df[,13]= as.factor(new_df[,13])

df2= as.data.frame(cat2bin(new_df,categorical=c(10:13))$data)
colnames(df2)[c(10:32)]= c("ir1", "ir2", "ir3","ir4","ir5",
                          "weak_foot_1","weak_foot_2","weak_foot_3","weak_foot_4","weak_foot_5",
                          "skills_2","skills_3","skills_4","skills_5",
                          "work_rate_LL","work_rate_LM","work_rate_LH",
                          "work_rate_ML","work_rate_MM","work_rate_MH",
                          "work_rate_HL","work_rate_HM","work_rate_HH")
colnames(df2)[c(1:9)]= colnames(new_df)[c(1:9)]
colnames(df2)[c(33:ncol(df2))]= colnames(new_df)[c(14:ncol(new_df))]
df2$preferred_foot= as.numeric(new_df$preferred_foot)-1
for( i in 1:ncol(df2)){
  df2[,i]= as.numeric(df2[,i])
}
df2$full_name= new_df$full_name
new_df= df2


# Now, the data is ready 
# Saving the data:
write.csv(new_df,"FIFA Cleaned.csv", row.names = T)




