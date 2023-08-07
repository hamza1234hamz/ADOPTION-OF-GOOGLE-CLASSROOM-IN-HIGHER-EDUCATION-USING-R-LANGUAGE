
#______________________ADOPTION OF GOOGLE CLASSROOM IN HIGHER EDUCATION ___________________________


# STEP1: PROBLEM STATEMENT
#1.1: Formulation of the problem statement

#1.1.1: Introduction (Context) --> The study aims to examine the adoption of Google Classroom
#as an online learning platform in higher education institutions, using the TAM model.

#1.1.2: Problem/Observation
#With the classroom application playing an increasingly central tool in the ENSAK learning environment, 
#there are growing concerns about how it affects students' behavior.


#STEP2: COLLECTING DATA
library(readxl)
d <- read_excel("C:/Users/Lenovo/Downloads/d (1).xlsx")
View(d)


#STEP3: DATA PREPROCESSING

#1. DATA CONVERSION

if(is.character(d$age)){                
  d$age=as.numeric(d$age)
}

if(is.character(d$Genre)){
d$Genre<- as.factor(d$Genre) }

if(is.character(d$filiere)){
d$filiere<-as.factor(d$filiere) }


if(is.character(d$annee_etd)){
d$annee_etd<- as.factor(d$annee_etd)
}


#PU 
for (i in 1:5) {
  d[[paste0("PU", i)]] <- factor(d[[paste0("PU", i)]], 
                                   levels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d' accord"), 
                                   labels = c(1, 2, 3, 4, 5))
}

#PEOU 
for (i in 1:4) {
  d[[paste0("PEOU", i)]] <- factor(d[[paste0("PEOU", i)]], 
                                  levels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d' accord"), 
                                  labels = c(1, 2, 3, 4, 5))
}

#ATD

for (i in 1:4) {
  d[[paste0("ATD", i)]] <- factor(d[[paste0("ATD", i)]], 
                                  levels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d' accord"), 
                                  labels = c(1, 2, 3, 4, 5))
}

#IB
for (i in 1:3) {
  d[[paste0("IB", i)]] <- factor(d[[paste0("IB", i)]], 
                                   levels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d' accord"), 
                                   labels = c(1, 2, 3, 4, 5))
}

#F
for (i in 1:4) {
  d[[paste0("F", i)]] <- factor(d[[paste0("F", i)]], 
                                   levels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d' accord"), 
                                   labels = c(1, 2, 3, 4, 5))
}


#3.2 Outlier and missing values treatment

#OUTLIER VALUES
boxplot(d$age)

outliar= boxplot.stats(d$age)$out

for(i in 1:length(d$age)){
  if(d$age[i]%in%boxplot.stats(d$age)$out){
    d$age[i]= NA
  }
}


#MISSING VALUES

j=0
for (i in 1: length(d$age)) {
  if(is.na(d$age[i])){
    j=j+1
  }
}

pna=j/length(d$age)

pna #pna<5% WE CAN DELETE THIS VALUE

for(i in 1:length(d$age)){
  if(is.na(d$age[i])){
    d <- d[complete.cases(d$age), ]
  }}

boxplot(d$age)


#3.3 NORMALITY TEST

# age 

#H0: There is no significant difference between the normal distribution and my distribution.
#H1: There is a significant difference between the normal distribution and my distribution.

shapiro.test(d$age) # p-value = 1.115e-06 < 0.05 So we choose H1 -> The age does not follow a normal distribution

#Quasi-normality

library(moments)
skewness(d$age)
kurtosis(d$age) # -3 < skewness & kurtosis < 3 ----> AGE is Quasi-normal 




#3.4 Test of reliability using Cronbach's alpha

#Preprocessing of questionnaire

#PU
for (i in 1:5) {
  d[[paste0("PU", i)]] <- as.numeric(d[[paste0("PU", i)]])
}

#PEOU
for (i in 1:4) {
  d[[paste0("PEOU", i)]] <- as.numeric(d[[paste0("PEOU", i)]])
}

#ATD
for (i in 1:4) {
  d[[paste0("ATD", i)]] <- as.numeric(d[[paste0("ATD", i)]])
}

#IB
for (i in 1:3) {
  d[[paste0("IB", i)]] <- as.numeric(d[[paste0("IB", i)]])
}
#F
for (i in 1:4) {
  d[[paste0("F", i)]] <- as.numeric(d[[paste0("F", i)]])
}



#DATA 
library(psych)
d_alpha=data.frame(d$PU1,d$PU2,d$PU3,d$PU4,d$PU5,d$PEOU1,d$PEOU2,d$PEOU3,d$PEOU4,d$ATD1,d$ATD2,d$ATD3,d$ATD4,d$IB1,d$IB2,d$IB3,d$F1,d$F2,d$F3,d$F4)
alpha(d_alpha)
# Cronbach's Alpha > 0.7--> Our questionnaire is reliable




#STEP4: PROCESSING

#Conversion of numeric values to factors

for (i in 1:5) {
  d[[paste0("PU", i)]] <- factor(d[[paste0("PU", i)]], 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d'accord"))
}

for (i in 1:4) {
  d[[paste0("PEOU", i)]] <- factor(d[[paste0("PEOU", i)]], 
                                   levels = c(1, 2, 3, 4, 5), 
                                   labels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d'accord"))
}

for (i in 1:4) {
  d[[paste0("ATD", i)]] <- factor(d[[paste0("ATD", i)]], 
                                  levels = c(1, 2, 3, 4, 5), 
                                  labels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d'accord"))
}

for (i in 1:3) {
  d[[paste0("IB", i)]] <- factor(d[[paste0("IB", i)]], 
                                 levels = c(1, 2, 3, 4, 5), 
                                 labels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d'accord"))
}


for (i in 1:4) {
  d[[paste0("F", i)]] <- factor(d[[paste0("F", i)]], 
                                levels = c(1, 2, 3, 4, 5), 
                                labels = c("Pas du tout d'accord", "Pas d'accord", "Neutre", "D'accord", "Tout à fait d'accord"))
}

#4.1 Univariate descriptive statistics
#4.1.1 numerical

summary(d)

#4.1.2 Graphical representation

hist(d$age)
plot(d$Genre)
plot(d$filiere)
plot(d$annee_etd)
plot(d$PU1)
plot(d$PU2)
plot(d$PU3)
plot(d$PU4)
plot(d$PU5)
plot(d$PEOU1)
plot(d$PEOU2)
plot(d$PEOU3)
plot(d$PEOU4)
plot(d$ATD1)
plot(d$ATD2)
plot(d$ATD3)
plot(d$ATD4)
plot(d$IB1)
plot(d$IB2)
plot(d$IB3)
plot(d$F1)
plot(d$F2)
plot(d$F3)
plot(d$F4)

#Verification of Response Frequencies

# H0: There is no difference in terms of frequency between the responses.
# H1: There is a difference in terms of frequency between the responses.

chisq.test(table(d$Genre)) # p-value = 0.1521 > 5%  therefore H0 is accepted, indicating that there is no difference in terms of gender.
chisq.test(table(d$filiere)) #p-value = 0.001135  < 5%  rejecting H0
chisq.test(table(d$annee_etd))#p-value =  5.44e-05  < 5% rejecting H0




#Perceived Usefulness

chisq.test(table(d$PU1)) # p-value = 6.779e-12  
chisq.test(table(d$PU2)) # p-value = 1.224e-15 
chisq.test(table(d$PU3)) #p-value = 5.465e-09 
chisq.test(table(d$PU4)) # p-value = 6.969e-05 

#all P-values <5% therefore rejecting the null hypothesis ---> There is a difference in terms of frequency between the responses.

#Perceived ease of use

chisq.test(table(d$PEOU1))    # p-value = 0.000000001182      
chisq.test(table(d$PEOU2))    # p-value = 2.57e-12              
chisq.test(table(d$PEOU3))    # p-value = 0.0000002874        
chisq.test(table(d$PEOU4))    # p-value = 0.00000006905       

#all P-values <5% therefore rejecting the null hypothesis ---> There is a difference in terms of frequency between the responses.

#ATTITUDE

chisq.test(table(d$ATD1))    # p-value =  2.277e-10        
chisq.test(table(d$ATD2))    # p-value = 0.00005758     
chisq.test(table(d$ATD3))    # p-value = 6.504e-14      
chisq.test(table(d$ATD4))    # p-value = 0.00006981

#all P-values <5% therefore rejecting the null hypothesis ---> There is a difference in terms of frequency between the responses.

#INTENTIONAL BEHAVIOUR

chisq.test(table(d$IB1)) #p-value=5.526e-16 <5% 
chisq.test(table(d$IB2)) #p-value=1.346e-05<5% 
chisq.test(table(d$IB3)) #p-value=2.025e-09<5% 

#all P-values <5% therefore rejecting the null hypothesis ---> There is a difference in terms of frequency between the responses.

#EXTERNAL FACTORS
chisq.test(table(d$F1)) #p-value=4.225e-10 <5% 
chisq.test(table(d$F2))#p-value=1.965e-07 <5% 
chisq.test(table(d$F3)) #p-value=2.618e-05 <5% 
chisq.test(table(d$F4)) #p-value= 2.2e-16 <5% 

#all P-values <5% therefore rejecting the null hypothesis ---> There is a difference in terms of frequency between the responses.


#4.1 Bivariate descriptive statistics

#Hypothesis testing between items


#PU x PEOU

for (j in 1:5) {
  for (i in 1:4) {
    test <- chisq.test(d[[paste0("PU", j)]], d[[paste0("PEOU", i)]])
    print(test)
  }
}

# PU4  PEOU1 :  p-value = 0.07745 
# PU5  PEOU1 :  p-value = 0.09302
# PU5  PEOU3 :  p-value = 0.07221
# PU5  PEOU4 :  p-value = 0.1237

#In these 5 tests, H0 is accepted, indicating that there is no significant difference between the two items.
#For the remaining combinations, we find that the p-value is less than 5%, indicating a significant difference.

#_______________________ ITEMS (quali x quali) ____________________________

#   PU x ATD
for (j in 1:5) {
  for (i in 1:4) {
    test <- chisq.test(d[[paste0("PU", j)]], d[[paste0("ATD", i)]])
    print(test)
  }
}
# PU1  ATD2   p-value = 0.2929
# PU1  ATD4   p-value = 0.7064

# PU2 ATD4   p-value = 0.6131

# PU3 ATD2   p-value = 0.5903
# PU3 ATD4  p-value = 0.1953

# PU4 ATD1   p-value = 0.08026
# PU4 ATD2   p-value = 0.5505
# PU4 ATD4   p-value = 0.07996

# PU5 ATD2   p-value = 0.4707
# PU5 ATD4   p-value = 0.1859


#In these  tests, H0 is accepted, indicating that there is no significant association between the two items.
#For the remaining combinations,  we find that the p-value is less than 5%, indicating a significant association.


#PU x IB

for (j in 1:5) {
  for (i in 1:3) {
    test <- chisq.test(d[[paste0("PU", j)]], d[[paste0("IB", i)]])
    print(test)
  }
}

# PU1 IB1  p-value = 0.07551
# PU1 IB2   p-value = 0.1513
# PU1 IB3  p-value = 0.3931

# PU2 IB1  p-value = 0.07692
# PU2 IB3  p-value = 0.1441

# PU3 IB2  p-value = 0.2328

# PU5 IB2  p-value = 0.1066
# PU5 IB3  p-value = 0.639


#In these  tests, H0 is accepted, indicating that there is no significant association between the two items.
#For the remaining combinations,  we find that the p-value is less than 5%, indicating a significant association.



#PU x F

for (j in 1:5) {
  for (i in 1:4) {
    test <- chisq.test(d[[paste0("PU", j)]], d[[paste0("F", i)]])
    print(test)
  }
}

#  PU1  F1    p-value = 0.1688
#  PU1  F3    p-value = 0.6161

#  PU2  F1   p-value = 0.2026
#  PU2  F2   p-value = 0.1335
#  PU2  F3   p-value = 0.5129

#  PU3  F2   p-value = 0.5823
#  PU3  F3   p-value = 0.1364

#  PU4 F2   p-value = 0.3762
#  PU4 F3   p-value = 0.2835

#  PU5 F1   p-value = 0.7205
#  PU5 F2   p-value = 0.06887
#  PU5 F3   p-value = 0.1538
#  PU5 F4   p-value = 0.153


#In these  tests, H0 is accepted, indicating that there is no significant association between the two items.
#For the remaining combinations,  we find that the p-value is less than 5%, indicating a significant association. 


#PEOU x ATD

for (j in 1:4) {
  for (i in 1:4) {
    test <- chisq.test(d[[paste0("PEOU", j)]], d[[paste0("ATD", i)]])
    print(test)
  }
}

# PEOU1 ATD2     p-value = 0.506
# PEOU1 ATD4     p-value = 0.4983

# PEOU2 ATD2     p-value = 0.461
# PEOU2 ATD4     p-value = 0.2445

# PEOU3 ATD2     p-value = 0.1985
# PEOU3 ATD4     p-value = 0.7663

# PEOU4 ATD2     p-value = 0.3507
# PEOU4 ATD4     p-value = 0.8178


#In these  tests, H0 is accepted, indicating that there is no significant association between the two items.
#For the remaining combinations,  we find that the p-value is less than 5%, indicating a significant association.




#PEOU x IB

for (j in 1:4) {
  for (i in 1:3) {
    test <- chisq.test(d[[paste0("PEOU", j)]], d[[paste0("IB", i)]])
    print(test)
  }
}
# PEOU1 IB1    p-value = 0.07728
# PEOU1 IB2    p-value = 0.3027
# PEOU1 IB3    p-value = 0.08421

# PEOU2 IB2     p-value = 0.2463
# PEOU2 IB3     p-value = 0.09383

# PEOU3 IB1     p-value = 0.05124
# PEOU3 IB2     p-value = 0.4591
# PEOU3 IB3     p-value = 0.2872

# PEOU4 IB1     p-value = 0.05345
# PEOU4 IB2     p-value = 0.7057
# PEOU4 IB3     p-value = 0.2399


#In these  tests, H0 is accepted, indicating that there is no significant association between the two items.
#For the remaining combinations,  we find that the p-value is less than 5%, indicating a significant association..



#PEOU x F

for (j in 1:4) {
  for (i in 1:4) {
    test <- chisq.test(d[[paste0("PEOU", j)]], d[[paste0("F", i)]])
    print(test)
  }
}
# PEOU1 F2   p-value = 0.8447
# PEOU1 F3   p-value = 0.3122

# PEOU2 F1   p-value = 0.1452
# PEOU2 F2   p-value = 0.4381
# PEOU2 F3   p-value = 0.1311

# PEOU3 F1   p-value = 0.3087
# PEOU3 F2   p-value = 0.4836

# PEOU4 F2   p-value = 0.3673


#In these  tests, H0 is accepted, indicating that there is no significant association between the two items.
#For the remaining combinations,  we find that the p-value is less than 5%, indicating a significant association.



#ATD x IB

for (j in 1:4) {
  for (i in 1:3) {
    test <- chisq.test(d[[paste0("ATD", j)]], d[[paste0("IB", i)]])
    print(test)
  }
}
#ATD4 IB3  p-value = 0.1137


#In this test, H0 is accepted, indicating that there is no significant association between the two items.
#For the remaining combinations,  we find that the p-value is less than 5%, indicating a significant association.



#ATD x F

for (j in 1:4) {
  for (i in 1:4) {
    test <- chisq.test(d[[paste0("ATD", j)]], d[[paste0("F", i)]])
    print(test)
  }
}
#ATD1 F1  p-value = 0.3919
#ATD1 F3  p-value = 0.1306

#ATD2 F1  p-value = 0.2221
#ATD2 F2  p-value = 0.3964
#ATD2 F3  p-value = 0.1522
#ATD2 F4  p-value = 0.4747

#ATD3 F3  p-value = 0.1053

#ATD4 F1  p-value = 0.1669
#ATD4 F3  p-value = 0.3821
#ATD4 F4  p-value = 0.4286

#In these  tests, H0 is accepted, indicating that there is no significant association between the two items.
#For the remaining combinations,  we find that the p-value is less than 5%, indicating a significant association.


#IB x F

for (j in 1:3) {
  for (i in 1:4) {
    test <- chisq.test(d[[paste0("IB", j)]], d[[paste0("F", i)]])
    print(test)
  }
}
# IB1 F3  p-value = 0.5425

# IB2 F1  p-value = 0.3693
# IB2 F2  p-value = 0.299
# IB2 F3  p-value = 0.5898
# IB2 F4  p-value = 0.2224

# IB3 F1  p-value = 0.2703
# IB3 F2  p-value = 0.6494
# IB3 F3  p-value = 0.7648
# IB3 F4  p-value = 0.06063


#In these  tests, H0 is accepted, indicating that there is no significant association between the two items.
#For the remaining combinations,  we find that the p-value is less than 5%, indicating a significant association.


#___________________ AGE _______________________

#  AGE x F
#AGE is Quasi- normal --> We will use both parametric and non-parametric tests. 

#Parametric test : anova 
for (i in 1:4) {
  model <- aov(age ~ ., data = d[, c("age", paste0("F", i))])
  test <- anova(model)
  print(test)
}

# F1  Pr(>F) = 0.2611 > 0.05  => H0 accepted 
# F2  Pr(>F) = 0.3188 > 0.05  => H0 accepted 
# F3  Pr(>F) = 0.285  > 0.05  => H0 accepted 
# F4  Pr(>F) = 0.1632 > 0.05  => H0 accepted 
# ---> cc There is no significant difference between the  variance of the two variables.

#Non parametric test : 
for (i in 1:4) {
  model <- kruskal.test(age ~ ., data = d[, c("age", paste0("F", i))])
  print(model)
}
# F1  Pr(>F) = 0.2911 > 0.05  => H0 accepted
# F2  Pr(>F) = 0.4022 > 0.05  => H0 accepted 
# F3  Pr(>F) = 0.3322 > 0.05  => H0 accepted 
# F4  Pr(>F) = 0.2013 > 0.05  => H0 accepted 

#----> there are no significant differences between the distributions of the variables

#The results of both parametric and non-parametric tests are consistent, 
#therefore we will consider the results of the parametric test, ANOVA.



#  AGE x PU

for (i in 1:5) {
  model <- aov(age ~ ., data = d[, c("age", paste0("PU", i))])
  test <- anova(model)
  print(test)
}
# PU1  Pr(>F) = 0.1926 > 0.05  => H0 accepted  
# PU2  Pr(>F) = 0.4487 > 0.05  => H0 accepted   
# PU3  Pr(>F) = 0.3089 > 0.05  => H0 accepted   
# PU4  Pr(>F) = 0.03455 < 0.05  => H1 accepted :  There is significant difference between the means of the two variables.
# PU5  Pr(>F) = 0.3206 > 0.05  => H0 accepted  
#---> cc There is no significant difference between the variance of the two variables in all the other cases.

for (i in 1:5) {
  model <- kruskal.test(age ~ ., data = d[, c("age", paste0("PU", i))])
  print(model)
}
# PU1  Pr(>F) = 0.1264 > 0.05  => H0 accepted 
# PU2  Pr(>F) = 0.364 > 0.05  => H0 accepted  
# PU3  Pr(>F) = 0.2781 > 0.05  => H0 accepted 
# PU4  Pr(>F) = 0.05097 < 0.05  => H1 accepted 
# PU5  Pr(>F) = 0.2978 > 0.05  => H0 accepted  

#The results of both parametric and non-parametric tests are consistent, 
#therefore we will consider the results of the parametric test, ANOVA.



#  AGE x PEOU

for (i in 1:4) {
  model <- aov(age ~ ., data = d[, c("age", paste0("PEOU", i))])
  test <- anova(model)
  print(test)
}
# PEOU1  Pr(>F) = 0.2722 > 0.05  => H0 accepted 
# PEOU2  Pr(>F) = 0.5442 > 0.05  => H0 accepted 
# PEOU3  Pr(>F) = 0.7468 > 0.05  => H0 accepted 
# PEOU4  Pr(>F) = 0.5575 > 0.05  => H0 accepted 
#---> cc There is no significant difference between the variance of the two variables

for (i in 1:4) {
  model <- kruskal.test(age ~ ., data = d[, c("age", paste0("PEOU", i))])
  print(model)
}
# PEOU1  Pr(>F) = 0.2594 > 0.05  => H0 accepted 
# PEOU2  Pr(>F) = 0.4493 > 0.05  => H0 accepted 
# PEOU3  Pr(>F) = 0.6629 > 0.05  => H0 accepted 
# PEOU4  Pr(>F) = 0.4431 > 0.05  => H0 accepted 

#--> There is no significant difference between the two variables

#The results of both parametric and non-parametric tests are consistent, 
#therefore we will consider the results of the parametric test, ANOVA.


#  AGE x ATD

for (i in 1:4) {
  model <- aov(age ~ ., data = d[, c("age", paste0("ATD", i))])
  test <- anova(model)
  print(test)
}
# ATD1  Pr(>F) = 0.624 > 0.05  => H0 accepted 
# ATD2  Pr(>F) = 0.2778 > 0.05  => H0 accepted 
# ATD3  Pr(>F) = 0.09416 > 0.05  => H0 accepted 
# ATD4  Pr(>F) = 0.03528 < 0.05  => H1 accepted : there is difference between the two vars


for (i in 1:4) {
  model <- kruskal.test(age ~ ., data = d[, c("age", paste0("ATD", i))])
  print(model)
}
# ATD1  Pr(>F) = 0.2594 > 0.05  => H0 accepted 
# ATD2  Pr(>F) = 0.4493 > 0.05  => H0 accepted 
# ATD3  Pr(>F) = 0.6629 > 0.05  => H0 accepted 
# ATD4  Pr(>F) = 0.4431 >  0.05  => H1 accepted : there are differences between the two vars

#The results of the parametric and non-parametric tests are not consistent for 
#the last test. Therefore, we will rely on the results of the non-parametric test, Kruskal-Wallis.

#  AGE x IB

for (i in 1:3) {
  model <- aov(age ~ ., data = d[, c("age", paste0("IB", i))])
  test <- anova(model)
  print(test)
}
# IB1  Pr(>F) = 0.3274 > 0.05  => H0 accepted 
# IB2  Pr(>F) = 0.6783 > 0.05  => H0 accepted
# IB3  Pr(>F) = 0.5432 > 0.05  => H0 accepted 
#--> there is no difference between the two vars

for (i in 1:3) {
  model <- kruskal.test(age ~ ., data = d[, c("age", paste0("IB", i))])
  print(model)
}
# IB1  Pr(>F) = 0.3666 > 0.05  => H0 accepted 
# IB2  Pr(>F) = 0.6906 > 0.05  => H0 accepted 
# IB3  Pr(>F) = 0.5495 > 0.05  => H0 accepted 

#--> there is no difference between the two vars
#The results of both parametric and non-parametric tests are consistent, 
#therefore we will consider the results of the parametric test, ANOVA.


#__________________ GENRE _____________________

# GENRE x F
for (i in 1:4) {
  test <- chisq.test(d$Genre, d[[paste0("F", i)]])
  print(test) 
} 

# F1  p-value = 0.06871 > 5% H0 accepted 
# F2  p-value= 0.15 > 5% H0 accepted 
# F3  p-value = 0.01371 < 5% H0 rejected --> H1 accepted.
# F4  p-value = 0.4514 > 5% H0 accepted

# in F1 F2 F4  combinations shown above : p-value > 5%, therefore there is no  difference between these two variables
# for the remaining combination F3, we find that p-value < 5% which is indicating a significant difference



# GENRE x PU

for (i in 1:5) {
  test <- chisq.test(d$Genre, d[[paste0("PU", i)]])
  print(test) 
} 
# PU1  p-value = 0.5944 > 5% H0 accepted 
# PU2  p-value = 0.228 > 5% H0 accepted
# PU3  p-value = 0.221 > 5% H0 accepted 
# PU4  p-value = 0.6628 > 5% H0 accepted 
# PU5  p-value = 0.8895 > 5% H0 accepted 

#--> in these combinations shown above : p-value > 5%, therefore there is no difference difference between these two variables



# GENRE x PEOU

for (i in 1:4) {
  test <- chisq.test(d$Genre, d[[paste0("PEOU", i)]])
  print(test) 
} 

# PEOU1  p-value = 0.6524 > 5% H0 accepted 
# PEOU2  p-value = 0.127 > 5% H0 accepted 
# PEOU3  p-value = 0.4578 > 5% H0 accepted 
# PEOU4  p-value = 0.102 > 5% H0 accepted 

# in these combinations shown above : p-value > 5%, therefore there is no  difference between these two variables



# GENRE ATD

for (i in 1:4) {
  test <- chisq.test(d$Genre, d[[paste0("ATD", i)]])
  print(test) 
}
# ATD1  p-value = 0.4315 > 5% H0 accepted 
# ATD2  p-value = 0.4737 > 5% H0 accepted 
# ATD3  p-value = 0.5341 > 5% H0 accepted 
# ATD4  p-value = 0.4208 > 5% H0 accepted 

# in these combinations shown above : p-value > 5%, therefore there is no  difference between these two variables



# GENRE IB

for (i in 1:3) {
  test <- chisq.test(d$Genre, d[[paste0("IB", i)]])
  print(test) 
}
# IB1  p-value = 0.9671 > 5% H0 accepted 
# IB2  p-value = 0.7812 > 5% H0 accepted 
# IB3  p-value = 0.9315 > 5% H0 accepted 

# in these combinations shown above : p-value > 5%, therefore there is no difference between these two variables


# ///////////////////////////////////////////

#STEP4:  REGRESSION

#Between two quantitative variables: regression
#Between two qualitative variables: classification
#We apply regression when we have multiple quantitative explanatory variables.
#We only have one quantitative variable (age), Therefore we cannot perform the regression test.

