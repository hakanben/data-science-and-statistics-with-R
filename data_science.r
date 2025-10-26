#1. Introduction
#a. Reading data from outside
heart<-read.csv("C:/Users/LENOVO/Desktop/heart.csv", header=TRUE)

#b. Checking row and column names
heart #controlling data
View(heart)
nrow(heart) #our data has 303 rows
ncol(heart) #our data has 14 columns
colnames(heart) #column names

#c. Summary information about the variables of the data set used
summary(heart)
names(heart)

#Explaining variables
#install.packages("ggplot2")
#install.packages("ggpubr")
library(ggplot2)
library(ggpubr)

#age: Age in years
heart$age
table(heart$age)
plot(heart$age, main="Frequency of Age", ylab="Frequency", 
     xlab="Age")
hist(heart$age, col=c("#d1001f","#a6ecfa")[as.factor(heart$sex)], breaks=20, main="Frequency of Age Respect to Gender", ylab="Frequency", 
     xlab="Age", prob=T)
lines(density(heart$age, adjust=1), col="red", lwd=2, lty="dotted")
legend(x="topright", legend=c("Female","Male"), col=c("#d1001f","#a6ecfa"),pch=19, title="Sex")

#sex: 1=male; 0=female 
heart$sex
hist(heart$sex, col=c("#d1001f","#a6ecfa"), main="Genders of Patients", xlab="0=Female; 1=Male")
table(heart$sex)

#cp: Chest pain type
heart$cp
table(heart$cp)
hist(heart$cp, main="Frequency of cp", xlab="cp", breaks=20)

#trestbps: Resting blood pressure (in mm Hg on admission to the hospital)
heart$trestbps
hist(heart$trestbps, breaks=20, main="Frequency of Blood Pressure", xlab="Blood Pressure", prob=T)
lines(density(heart$trestbps, adjust=1), col="red", lwd=2, lty="dotted")

#chol: Serum cholestoral in mg/dl
heart$chol
hist(heart$chol, breaks=20, main="Frequency of Cholestoral", xlab="Cholestoral", prob=T)
lines(density(heart$chol, adjust=1,), col="red", lwd=2, lty="dotted")

#fbs: Fasting blood sugar > 120 mg/dl (1 = true; 0 = false)
heart$fbs
table(heart$fbs)
hist(heart$fbs, breaks=20, main="Frequency of fbs", xlab="fbs") 

#restecg: Resting electrocardiographic results
heart$restecg
table(heart$restecg)
hist(heart$restecg, breaks=20, main="Frequency of restecg", xlab="restecg")

#thalach: Maximum heart rate achieved
heart$thalach
hist(heart$thalach, breaks=20, main="Frequency of Thalach", xlab="Thalach", prob=T)
lines(density(heart$thalach, adjust=1),col="red", lwd=2, lty="dotted")

#exang: Exercise induced angina (1 = yes; 0 = no)
heart$exang
table(heart$exang)
hist(heart$exang, breaks=20, main="Frequency of exang", xlab="exang")

#oldpeak: ST depression induced by exercise relative to rest
heart$oldpeak
table(heart$oldpeak)
hist(heart$oldpeak, breaks=20, main="Frequency of oldpeak", xlab="oldpeak", prob=T)
lines(density(heart$oldpeak, adjust=1), col="red", lwd=2, lty="dotted")


#d. Checking whether there is missing data and explaining what is done with the appropriate code if there is any
#Method-1
missing<-which(is.na(heart))
length(missing)

#Method-2
is.na(heart)

#Method-3
for (i in heart){
  if(any(is.na(heart))==1){
    print("You have missing value")
    }else{
      print("You do not have missing value")
    }
}


#We do not have missing value but if we have, we can apply different solutions like this;
#Removing missing values (for example: na.omit() function) (not always preferred)
#Mean imputation (fill with average value)
#Substitution (replacement with a new observation)
#Hot deck imputation (random value assignment)
#Cold deck imputation (the value of another observation with the closest observation characteristics)
#Regression imputation (predictive assignment process with regression model)
#Stochastic regression imputation (regression model estimation and aggregation with random residual value)
#We can show the missing values with the VIM package and we can fill them with the MICE package
#Additionally we can also use packages like Amelia, missForest, Hmisc, and mi


#2. Exploratory Data Analysis (EDA):
#a. Tables and groups to be used in your analysis
#b. Histogram, scatterplot, boxplot, mean plot, tables, barplot, etc. with, distributions of variables; Distribution within groups should be shown
#c. Different graphing packages such as Base R and ggplot should be used

ExploratoryDataAnalysis<-function(data){
  print("*****************************************************************************************************************")
  print("Structure of the data set:")
  print(str(data))
  
  print("*****************************************************************************************************************")
  print("First six rows of the data set:")
  print(head(data))
  
  print("*****************************************************************************************************************")
  print("Last six rows of the data set:")
  print(tail(data))
  
  print("*****************************************************************************************************************")
  print("Summary of the dataset:")
  print(summary(data))
  
  print("*****************************************************************************************************************")
  print("Missing Value Check:")
  if(sum(is.na(data))>0){
    print("There is missing data.")
  }else{
    print("There is no missing data.")
  }
  print("*****************************************************************************************************************")
}

ExploratoryDataAnalysis(heart)


#Histograms of variables
hist(heart$age, breaks=20, main="Frequency of age", xlab="age")
hist(heart$cp, breaks=20, main="Frequency of cp", xlab="cp")
hist(heart$trestbps, breaks=20, main="Frequency of trestbps", xlab="trestbps")
hist(heart$chol, breaks=20, main="Frequency of chol", xlab="chol")
hist(heart$fbs, breaks=20, main="Frequency of fbs", xlab="fbs")
hist(heart$restecg, breaks=20, main="Frequency of restecg", xlab="restecg")
hist(heart$thalach, breaks=20, main="Frequency of thalach", xlab="thalch")
hist(heart$exang, breaks=20, main="Frequency of exang", xlab="exang")
hist(heart$oldpeak, breaks=20, main="Frequency of oldpeak", xlab="oldpeak")
hist(heart$slope, breaks=20, main="Frequency of slope", xlab="slope")
hist(heart$ca, breaks=20, main="Frequency of ca", xlab="ca")
hist(heart$thal, breaks=20, main="Frequency of thal", xlab="thal")
hist(heart$target, breaks=20, main="Frequency of target", xlab="target")



#3. Analyzes:
#a. There must be at least 2 of t-tests, ANOVA, and regression analyses.
#b. The assumptions on which the tests are based should be tested before testing.
#c. Test results (e.g. significant or insignificant difference between 2 groups) should be expressed in figures

#To use the t-test, the data must have normal distribution and homogeneity of variance.
#So we must first examine the distribution of the data.

#T-test
#H0 Hypothesis: The data shows normal distribution.
#Ha Hypothesis: The data is not normally distributed.
#If the p value is greater than 0.05, the H0 hypothesis is true.
hist(heart$age)
shapiro.test(heart$age) #The data is not normally distributed

hist(heart$trestbps)
shapiro.test(heart$trestbps) #The data is not normally distributed

hist(heart$chol)
shapiro.test(heart$chol) #The data is not normally distributed

hist(heart$thalach)
shapiro.test(heart$thalach) #The data is not normally distributed

hist(heart$oldpeak)
shapiro.test(heart$oldpeak) #The data is not normally distributed

#Since the data does not show a normal distribution according to the Shapiro test, the Wilcoxon test will be used instead of the t-test.
wilcox.test(heart$trestbps~heart$sex, mu=0, conf.int=T) #The shift between groups equals zero
wilcox.test(heart$chol~heart$sex, mu=0, conf.int=T) #The shift between groups equals zero
wilcox.test(heart$thalach~heart$sex, mu=0, conf.int=T) #The shift between groups equals zero
wilcox.test(heart$oldpeak~heart$sex, mu=0, conf.int=T) #The shift between groups not equals zero

bartlett.test(heart$trestbps~as.character(heart$sex)) #Homogeneous variance
bartlett.test(heart$chol~as.character(heart$sex)) #Non-homogeneous variance
bartlett.test(heart$thalach~as.character(heart$sex)) #Non-homogeneous variance
bartlett.test(heart$oldpeak~as.character(heart$sex)) #Homogeneous variance

fligner.test(heart$trestbps~heart$sex) #Homogeneous variance
fligner.test(heart$chol~heart$sex) #Non-homogeneous variance
fligner.test(heart$thalach~heart$sex) #Homogeneous variance
fligner.test(heart$oldpeak~heart$sex) #Non-homogeneous variance


#ANOVA
#Data preparation
#install.packages("rstatix")
#install.packages("tidyverse")
#install.packages("car")
library(rstatix)
library(tidyverse)
library(car)

outliers<-identify_outliers(heart["chol"])
outliers

heart_data<-heart %>% filter(chol<394)

max(heart_data$chol)

heart_data_new<-heart_data %>% group_by(
  age_groups=cut(age, breaks=seq(min(age)-1, max(age)+1,
                                 length.out=4)))

View(heart_data_new)
levels(heart_data_new$age_groups)

#Assumption check
heart_data_new_1<-heart_data_new %>% select(age_groups, chol)
View(heart_data_new_1)

#Normality assumption
heart_data_new_1 %>% group_by(age_groups) %>% summarise(ShapiroResults=shapiro.test(chol)$p.value)
#All show normal distribution

#Assumption of homogeneity of variance
bartlett.test(heart_data_new_1$chol~heart_data_new_1$age_groups)
leveneTest(heart_data_new_1$chol~heart_data_new_1$age_groups)
#There is homogeneity according to two different tests


#Data analysis
anov<-aov(heart_data_new_1$chol~heart_data_new_1$age_groups)
anov
plot(anov)
#If the residual values are normally distributed, the ANOVA test is valid.


#ANOVA is created with linear functions.
#After the model is created, we get the margin of error when we replace the dependent variables.
summary(anov)
#H0: All groups are equal to each other
#Ha: At least 1 group is different from the others
#Ha is accepted

heart_data_new_1%>%group_by(age_groups)%>%
  summarise(ortalamalar=mean(chol,na.rm = T))

names(anov)

shapiro.test(anov$residuals)
#Residuals have a normal distribution

class(heart_data_new_1)
heart_data_new_1<-as.data.frame(heart_data_new_1)
anova_test(heart_data_new_1, dv=chol, between = age_groups)
#GES value is the influence criterion of how much the independent variable affects the dependent variable.
#Age values are interpreted as affecting cholesterol at a value of 0.03.


#Dependent sample
#install.packages("ggplot2")
#install.packages("ggpubr")
library(ggplot2)
library(ggpubr)

#Pairwise Comparison with Post Hoc Test
heart_data_new_1<-as.data.frame(heart_data_new_1)
anova_1<-aov(heart_data_new_1$chol~heart_data_new_1$age_groups)
summary(anova_1)

anova_2<-anova_test(heart_data_new_1, dv=chol, between = age_groups)

summary(anova_1);anova_2


TukeyHSD(anova_1)
#There is a significant difference as the p value is less than 0.05

tukey_hsd(heart_data_new_1, chol~age_groups)


#Two-Way Analysis of Variance
heart_data_1<-heart%>% filter(chol<400)
heart_data_1%>% group_by(cp, fbs)%>% summarise(Shapiro=shapiro.test(chol)$p.value)
bartlett.test(heart_data_1$chol~interaction(heart_data_1$cp, heart_data_1$fbs))

anova_1<-aov(heart_data_1$chol~as.factor(heart_data_1$cp)*as.factor(heart_data_1$fbs))
summary(anova_1)

anova_test(heart_data_1, dv=chol, between = c(cp,fbs))

anova_2<-aov(heart_data_1$trestbps~as.factor(heart_data_1$cp)*as.factor(heart_data_1$fbs))
summary(anova_2)
anova_test(heart_data_1, dv=trestbps, between = c(cp, fbs))

#Post Hoc Tests in Two-Way Analysis of Variance (TukeyHSD Test)
library("ggpubgr")
identify_outliers(heart_data_1["trestbps"])

heart_data_1_new<-heart%>%filter(trestbps<172)
View(heart_data_1_new)

int_groups<-apply(heart_data_1_new, MARGIN = 1, FUN=function(x){
  r<-paste0(x["cp"],"-",x["fbs"])
  return(r)
})
#apply fonksiyonu teker teker satırları x'e atıyor
#cp ve fbs değerlerini karaktere atadık
heart_data_1_new$int_groups<-int_groups
View(heart_data_1_new)

ggboxplot(heart_data_1_new, x="int_groups", y="trestbps",title = "Boxplot Chart",
          color="orange")


heart_data_1_new%>%group_by(cp, fbs)%>%
  summarise(Shapiro=shapiro.test(trestbps)$p.value)
bartlett.test(heart_data_1$trestbps~interaction(heart_data_1$cp,heart_data_1$fbs))
anova_1<-aov(heart_data_1_new$trestbps~as.factor(heart_data_1_new$cp)*as.factor(heart_data_1_new$fbs))
summary(anova_1)

TukeyHSD(anova_1)
a<-tukey_hsd(anova_1)
View(a)
a_1<-a%>%filter(p.adj<0.05)
View(a_1)

#Non-Parametric-Kruskal Wallis Test
heart_1<-heart
View(heart_1)
heart_1%>% group_by(cp) %>% summarise(Normallik=shapiro.test(oldpeak)$p.value)
bartlett.test(heart$oldpeak~heart$cp)

kruskal.test(x=heart_1$oldpeak,g =as.factor(heart$cp))
#H0: There is no difference between groups
#Ha: At least one group is different from the others 


heart_1%>% group_by(cp) %>% summarise(Medyan=median(oldpeak, na.rm=T))
#The difference between the groups may be due to the difference between groups 0, 1 and 2.


#Pairwise Comparison with Dunn's Test After Kruskal-Wallis (Post Hoc)
#install.packages("dunn.test")
library(dunn.test)

dunn.test(x=heart_1$oldpeak, g=heart_1$cp)
#A negative value indicates that the value on the left is more than the value on the top.
#Values in 2 are more than 1 (for the value in the middle)
#Groups 0 and 3 are very close to each other
#Reject H0 if p <= alpha/2


#Linear Models
model<-lm(chol~age, data=heart)
model
summary(model) #There is a significant relationship

model<-lm(trestbps~age, data=heart)
model
summary(model) #There is a significant relationship

model<-lm(chol~age, data=heart)
model
summary(model) #There is a significant relationship

model<-lm(thalach~age, data=heart)
model
summary(model) #There is a significant relationship

model<-lm(oldpeak~age, data=heart)
model
summary(model) #There is a significant relationship

model<-lm(sex~age, data=heart)
model
summary(model) #No significant relationship exists

model<-lm(trestbps~chol, data=heart)
model
summary(model) #No significant relationship exists

model<-lm(trestbps~thalach, data=heart)
model
summary(model) #No significant relationship exists

model<-lm(trestbps~age, data=heart)
model
summary(model) #There is a significant relationship

heartData<-data.frame(
  age<-heart$age,
  trestbps<-heart$trestbps,
  chol<-heart$chol,
  thalach<-heart$thalach,
  oldpeak<-heart$oldpeak
)

LMmodel<-lm(age~trestbps+chol+thalach+oldpeak, data=heartData)
summary(LMmodel) #There is a significant relationship

LMmodel1<-lm(age~trestbps+chol+thalach, data=heartData)
summary(LMmodel1) #There is a significant relationship

LMmodel2<-lm(age~trestbps+chol, data=heartData)
summary(LMmodel2) #There is a significant relationship

LMmodel3<-lm(age~trestbps, data=heartData)
summary(LMmodel3) #There is a significant relationship

par(mfrow = c(2, 2))
plot(heartData$trestbps, heartData$age, main = "Trestbps vs. Age", xlab = "Trestbps", ylab = "Age", pch = 19)
abline(lm(age ~trestbps, data = veri), col = "red")
plot(heartData$chol, heartData$age, main = "Chol vs. Age", xlab = "Chol", ylab = "Age", pch = 19)
abline(lm(age ~ chol, data = veri), col = "red")
plot(heartData$thalach, heartData$age, main = "Thalach vs. Age", xlab = "Thalach", ylab = "Age", pch = 19)
abline(lm(age ~ thalach, data = veri), col = "red")
plot(heartData$oldpeak, heartData$age, main = "Oldpeak vs. Age", xlab = "Oldpeak", ylab = "Age", pch = 19)
abline(lm(age ~ oldpeak, data = veri), col = "red")

#Hakan Büyüktuncay