#clear environment
rm(list=ls())

#read data
fpath<- "C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-final/data/"
alco_data <- read.table(paste0(fpath, "alcohol_student.csv"), sep=",", header=T)
alco_data<- alco_data[,-1]
#My aim is to demonstrate the use of various statistical techniques in getting insight in to
#the data. Firstly, I will use the basic desctiptive statitstics to understand
#the distribution, correlation and dependencies(cor, barplot, histogram, biplot etc)
#I will also be using the regression models(GLM, GAM and GBM) and test
#my models using 70:30 data split. 
#I will also explore LDA and  MCA.

#necessary packages
#install.packages("dismo")
library(dplyr)
library(ggplot2)
library(corrplot)
library(GGally)
library(tidyr)
library(tidyverse)
library(gbm)
library(dismo)
library(caTools)
library(mgcv)
library(MASS)
library(FactoMineR)

#hypothesis
#grade is affected by study time, parents education level, absence, alcohol consumption

#Absence is affected by same factors
#alcohol consumption is affected by same factors

categ = apply(alco_data, 2, function(x) nlevels(as.factor(x)))
categ
dim(alco_data)
str(alco_data)
summary(alco_data)
glimpse(alco_data)
# ####Grade:

#bar plot of the variables
gather(alco_data) %>% ggplot(aes(value)) + facet_wrap("key", scales = "free") + geom_bar() 


#To avoid, confusion, I will copy the data into a new data frame for MCA
data_mca <- alco_data

#Firstly, I will be extracting the categorical variables, as factors
#now, filter them out for the MCA
data_mca_fac1<-Filter(is.factor, data_mca)

high_use<-factor(alco_data[,"high_use"])
data_mca_fac1 <- cbind.data.frame(data_mca_fac1, high_use)

#par(mfrow=c(1,2))
#now, perform the MCA on the catergorial/qualitative variables
mca_alco1 <- MCA(data_mca_fac1, graph = T)

#plot it
plot(mca_alco1, invisible=c("ind"), habillage="quali")
#par(mfrow=c(1,1))

# Getting a better biplot for MCA using ggplot
## number of categories per variable
categ = apply(data_mca_fac1, 2, function(x) nlevels(as.factor(x)))
categ

# data frame with variable coordinates
mca_vars_df = data.frame(mca_alco1$var$coord, Variable = rep(names(categ), categ))
mca_vars_df


# data frame with observation coordinates
mca_obs_df = data.frame(mca_alco1$ind$coord)


# MCA plot of observations and categories
ggplot(data = mca_obs_df, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.0) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca_vars_df, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca_vars_df), colour = Variable)) +
  ggtitle("MCA plot of variables using R package FactoMineR") +
  scale_colour_discrete(name = "Variable")

#Here, we can see that female students generally attend school
#because of her reputation and get more school support.
#They also have family support and are able to attend paid extra
#classes. Most of their father's job are in the health sector.
#High alcohol consumption is more rampant amongst female students
#compared to their male counterparts with high alcohol use.
#male students also attend the school based on course preference
#and other reasons. By and large, they do not attend paid extra classes
#compared to the female students and also do not get family and
#school support like the frmale students.
#LAstly, they mosly have no intention of pursuing higher education.
#This is explored further by categorising more continuous variables
#such as grades, absences, to allow for comparison with other variables.



#Next, I will be categorising grade(G3), absences, Mother's education(Medu),
#father's education
data_mca2<- alco_data
#now, convert mother and father's education level into something more understandable.
#data_mca$Medu<- factor(data_mca$Medu)
data_mca2$Medu<- factor(data_mca2$Medu, labels=c("no", "pr","5-9th", "sec", "hiE"))
data_mca2$Fedu<- factor(data_mca2$Medu, labels=c("no", "pr","5-9th", "sec", "hiE"))

#Now, let's categorise grades according to quartiles
bins_abs<- quantile(data_mca2$absences)
data_mca2$absences<-cut(data_mca2$absences, breaks=bins_abs, include.lowest=T,
                       labels=c("vlow", "low", "high", "vhigh"))

#same to grade(G3)
bins_gra<- quantile(data_mca2$G3)
data_mca2$G3<-cut(data_mca$G3, breaks=bins_gra, include.lowest=T,
                 labels=c("vlow", "low", "high", "vhigh"))

#Getting the columns that are factors
#since MCA is for categorical variables, I will be filtering the categorical
#variables/factors and also categorise some of the variables of interest
#such as absences, grade(G3). I will also factorise other variables of interest
#that are in integer forms.

#let's first see the variables already in factor format
names(Filter(is.factor, data_mca2))

#now, filter them out for the MCA
data_mca_fac2_<-Filter(is.factor, data_mca2)

#include the high alcohol use column
high_use<-factor(alco_data[,"high_use"])

#join with the dataframe with categorical varianles
data_mca_fac2_<-cbind.data.frame(data_mca_fac2_, high_use)
str(data_mca_fac2_)

#The above can also be done with dplyr which has been loaded already too
#data_mca %>% Filter(f = is.factor)

#names of the variables again ?
names(data_mca_fac2_) 
#Alternative for finding the column name that are factors
# names(data_)[ sapply(data_reg, is.factor) ]
# #or
# data_reg %>% Filter(f = is.factor) %>% names
# or
# data_reg %>% summarise_each(funs(is.factor)) %>% unlist %>% .[.] %>% names


#I will select few of the variables for clarity sake and include the sex too
data_mca_fac2<-data_mca_fac2_[,c("sex",names(data_mca_fac2_[,(10:ncol(data_mca_fac2_))]))]
    
#now, perform the MCA on the catergorial/qualitative variables
mca_alco2 <- MCA(data_mca_fac2, graph = T)

#plot it
plot(mca_alco2, invisible=c("ind"), habillage="quali")



# Getting a better biplot for MCA using ggplot
## number of categories per variable
categ2 = apply(data_mca_fac2, 2, function(x) nlevels(as.factor(x)))
categ2

# data frame with variable coordinates
mca_vars_df2 = data.frame(mca_alco2$var$coord, Variable = rep(names(categ2), categ2))
#mca_vars_df2


# data frame with observation coordinates
mca_obs_df2 = data.frame(mca_alco2$ind$coord)


# MCA plot of observations and categories
ggplot(data = mca_obs_df2, aes(x = Dim.1, y = Dim.2)) +
  geom_hline(yintercept = 0, colour = "gray70") +
  geom_vline(xintercept = 0, colour = "gray70") +
  geom_point(colour = "gray50", alpha = 0.1) +
  geom_density2d(colour = "gray80") +
  geom_text(data = mca_vars_df2, 
            aes(x = Dim.1, y = Dim.2, 
                label = rownames(mca_vars_df2), colour = Variable)) +
  ggtitle("MCA plot of variables") +
  scale_colour_discrete(name = "Variable")


#Here, we can see the categorisation and association
#In the NW quadrant, it shows that those that pay for for extra
#classes in portugese and math, have family support and are mostly
#female students. They also get extra education support and have
#ambition for higher education.

#In the NE quadrant, high alcohol use seem to be associated with
#guardian other than mother or father. Those with high 
#alcohol use also seem to not have ambition for higher education.
#They also perform poorly in studies(low grade). and mostly choose
#school because it is closer to home amongst other reasons, other
#than reputation and course preference. They also do not seem to 
#participate in extracurricular activities and are mostly 
#in romantic relationship. They also high absences.


#In the SW quadrant, it can be seen that those that have low absence
#perform very well in studies do not take excessive alcohol. They
#are also engaged in extracurricular activities and have their mother
#as their guardian. They also motivated to attend the school 
#because of shool's reputation.

#In the SE quadrant, Male students seem to be more moderately absent and
#do not attend paid extra classes in portuguese and math.
#attend the school because of the course preference. They generally also have 
#no internet access. They also mostly do not get school's support.
#It also looks like their father are their guardian.


#This will be further explored with family of regression models
#(GLM, GAM, GBM)





###################################################################
###################################################################
#REGRESSION MODELS(DLM, GAM, GBM)

#As done earlier, the data will be copied into a new dataframe for 
#all the regression analysis, to avoid confusion and alteration of 
#the original data
data_reg <- alco_data

grade_glm<-glm(G3~ sex + age+address+Medu+Fedu+
                 Pstatus+ traveltime+studytime+famsup+activities+higher+
                 internet+famrel+romantic+freetime+goout+ alc_use+ absences
               ,data=data_reg,family ="poisson")

grade_glm<-glm(G3~ Medu + higher,data=data_reg,family ="poisson")
summary(grade_glm)

#for GLM, predictors were selected, by employing the significance test from the model
#summary, anova test(Chi Square), AIC values and stepwise regression. I also checked 
#if any of the predictor is curvillinear(i.e has higher order polynomial)
stepAIC(grade_glm, direction = "both")
anova(grade_glm, test = "Chisq")

#In accordance to the principl of parsimony, I decided to use variables
#with significance level of 0.05
#Final model:  glm(G3~ Medu + higher,data=data_reg,family ="poisson")

##########################################3
#GLM, GAM, GBM
#Here, I will be applying these three kinds of regression model

# function to calculate the mean absolute and RMSE
#function to calculate mean error
mean_error<- function(obs, pred){
  me<-mean(abs(obs-pred))
  return(me)
}

# Function that returns Root Mean Squared Error
rmse <- function(obs, pred){
  rmse<-sqrt(mean((obs-pred)^2))
  return(rmse)
}


#########################################################################
#dividing into 70:30
{rep<-10
  for (i in 1:rep){
    print(i)
    
    #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
    rand<- sample(1:nrow(data_reg), size = 0.7*nrow(data_reg))
    cal<- data_reg[rand,]
    eva<-data_reg[-rand,]
    
    ###GLM
    grade_glm <- glm(G3~Medu + higher, data=cal, family = "poisson") 
    
    grade_glm_pred<- predict.glm(object = grade_glm, newdata = eva, type="response")
    
    cor_glm_grade<-cor(grade_glm_pred, eva$G3, method = "spearman")
    #grade_pred_glm_ras<- predict(model=grade_glm, object = ras_stack, fun=predict.glm, type = "response")
    #plot(grade_pred_glm_ras)
    
    #########
    #mean error and root mean square error
    error_grade_glm<- cbind.data.frame(grade_glm_pred, eva$G3)
    colnames(error_grade_glm) <- c("pred_glm_grade", "obs_grade")
    
    grade_glm_me <- mean_error(error_grade_glm$obs_grade, error_grade_glm$pred_glm_grade)
    grade_glm_rmse <- rmse(error_grade_glm$obs_grade, error_grade_glm$pred_glm_grade)
    
    me_rmse_grade_glm <- rbind.data.frame(grade_glm_me, grade_glm_rmse)
    colnames(me_rmse_grade_glm)<- c("grade_glm")
    
    
    
    
    #GAM
    grade_gam <- gam(G3~ s(Medu, k=3) + higher, data = cal, family = "poisson")
    grade_gam_pred <- predict.gam(grade_gam, newdata = eva, type = "response")
    
    obs_pred_grade_gam<- cbind.data.frame(grade_gam_pred, eva$G3)
    colnames(obs_pred_grade_gam) <- c("pred_gam_grade", "obs_gam_grade")
    #you can just calclate the correlation straight away
    cor_gam_grade <- cor(grade_gam_pred, eva$G3, method = "spearman")
    
    
    #########
    #mean error and root mean square error
    error_grade_gam<- cbind.data.frame(grade_gam_pred, eva$G3)
    colnames(error_grade_gam) <- c("pred_gam_grade", "obs_grade")
    
    grade_gam_me <- mean_error(error_grade_gam$obs_grade, error_grade_gam$pred_gam_grade)
    grade_gam_rmse <- rmse(error_grade_gam$obs_grade, error_grade_gam$pred_gam_grade)
    
    me_rmse_grade_gam <- rbind.data.frame(grade_gam_me, grade_gam_rmse)
    colnames(me_rmse_grade_gam)<- c("grade_gam")
    
    
    
    
    ###################################################################3
    #using the normal gbm, package.
    #GBM
    grade_gbm1<-gbm(formula = G3~ sex + age+address+Medu+Fedu+
                      Pstatus+ traveltime+studytime+famsup+activities+higher+
                      internet+famrel+romantic+freetime+goout+ alc_use+ absences, data=cal,
                   distribution = "poisson",n.trees = 1000, shrinkage = 0.001, interaction.depth = 6,
                   bag.fraction = 0.75)
    
    best.iter<-gbm.perf(grade_gbm1, plot.it = F, method = "OOB")
    grade_gbm1_pred<- predict.gbm(object = grade_gbm1, newdata = eva, best.iter,
                                 type="response")
    cor_gbm1_grade <- cor(grade_gbm1_pred, eva$G3, method = "spearman")
    
    
    
    #########
    #mean error and root mean square error
    error_grade_gbm1<- cbind.data.frame(grade_gbm1_pred, eva$G3)
    colnames(error_grade_gbm1) <- c("pred_gbm1_grade", "obs_grade")
    
    grade_gbm1_me <- mean_error(error_grade_gbm1$obs_grade, error_grade_gbm1$pred_gbm1_grade)
    grade_gbm1_rmse <- rmse(error_grade_gbm1$obs_grade, error_grade_gbm1$pred_gbm1_grade)
    
    me_rmse_grade_gbm1 <- rbind.data.frame(grade_gbm1_me, grade_gbm1_rmse)
    colnames(me_rmse_grade_gbm1)<- c("grade_gbm1")
    
    # par(mfrow=c(2,3))
    # plot.gbm(grade_gbm1, 1, best.iter)
    # plot.gbm(grade_gbm1, 2, best.iter)
    # plot.gbm(grade_gbm1, 3, best.iter)
    # plot.gbm(grade_gbm1, 4, best.iter)
    # plot.gbm(grade_gbm1, 5, best.iter)
    # plot.gbm(grade_gbm1, 6, best.iter)
    # par(mfrow=c(1,1))
    
    
    
    ###################################################
    #dismo package
    
    grade_gbm2 <- gbm.step(data=cal, gbm.x =c("sex", "age","address","Medu"
                 ,"Fedu","Pstatus", "traveltime","studytime","famsup","activities","higher",
                 "internet","famrel","romantic","freetime","goout", "alc_use", "absences"), gbm.y = "G3",
                          bag.fraction=0.75, learning.rate = 0.001,
                          family="poisson",n.trees=50, n.folds=10,
                          max.trees = 1000, tree.complexity = 6)
    
    #This also works but can be done directly as shown in the prediction after this
    #best.iter2<-gbm.perf(grade_gbm1, plot.it = T, method = "OOB")
    # grade_gbm2_pred<- predict.gbm(object = grade_gbm2, newdata = eva, best.iter2,
    #                              type="response")
    
    #
    #the prediction
    grade_gbm2_pred <- predict.gbm(grade_gbm2, newdata = eva, n.trees=grade_gbm2$n.trees, type = "response")
    #grade_pred_gbm2_ras <- predict(object=ras_stack,model=grade_gbm2, fun=predict,
    #                         n.trees=grade_gbm2$n.trees, type="response")
    
    #plot(grade_pred_gbm2_ras)
    cor_gbm2_grade <- cor(grade_gbm2_pred, eva$G3, method = "spearman")
    
    #########
    #mean error and root mean square error
    error_grade_gbm2<- cbind.data.frame(grade_gbm2_pred, eva$G3)
    colnames(error_grade_gbm2) <- c("pred_gbm2_grade", "obs_grade")
    
    grade_gbm2_me <- mean_error(error_grade_gbm2$obs_grade, error_grade_gbm2$pred_gbm2_grade)
    grade_gbm2_rmse <- rmse(error_grade_gbm2$obs_grade, error_grade_gbm2$pred_gbm2_grade)
    
    me_rmse_grade_gbm2 <- rbind.data.frame(grade_gbm2_me, grade_gbm2_rmse)
    colnames(me_rmse_grade_gbm2)<- c("grade_gbm2")
    
    
    
    
  } 
  #####All correlation
  all_cor_grade <- cbind.data.frame(cor_glm_grade,cor_gam_grade,
                                   cor_gbm1_grade, cor_gbm2_grade)
  colnames(all_cor_grade)<- c("grade_glm", "grade_gam", "grade_gbm1", "grade_gbm2")
  
  #####all error
  all_error_grade <- cbind.data.frame(me_rmse_grade_glm, me_rmse_grade_gam,
                                     me_rmse_grade_gbm1, me_rmse_grade_gbm2)
  rownames(all_error_grade)<- c("mean abs error", "RMSE")
  
}



#Using all the models to see the prediction
grade_gbm1<-gbm(formula = G3~ sex + age+address+Medu+Fedu+
                  Pstatus+ traveltime+studytime+famsup+activities+higher+
                  internet+famrel+romantic+freetime+goout+ alc_use+ absences, data=data_reg,
                distribution = "poisson",n.trees = 1000, shrinkage = 0.001, interaction.depth = 6,
                bag.fraction = 0.75)

summary(grade_gbm1)

#I chose GBM because it is able to handle multicollinearity and complex interactions,
#it can also show the response curves and relative importance of predictors.

#here, we can see that higher education pursuit, mother's educaton, alc use,
# and absences seem to be most important factors affecting students performances
#This is quite in line with the results I got from my GLM and GAM, however,
#only mother's education level and higher_use seem to be the significant
#factors. The least important factors are also shown in the GBM's summary of
#relative importance.

#now, let's see the response curves of this


plot(grade_gam, pages=1)
#Here, from the smooth curve from GAM, we can see that mother's education
#level has a positive effect on student's performance. However, the confidence
#interval especially at the lower level. is quite large, which shows that there
#is a wide range of uncertainty. Perharps, more observations needed.
#This will be explored further in the response curves from GBM below.

best.iter1<-gbm.perf(grade_gbm1, plot.it = F, method = "OOB")


par(mfrow=c(2,2))
plot.gbm(grade_gbm1, "Medu", best.iter1)
plot.gbm(grade_gbm1, "higher", best.iter1)
plot.gbm(grade_gbm1, "alc_use", best.iter1)
plot.gbm(grade_gbm1, "absences", best.iter1)
par(mfrow=c(1,1))
#As we can see here, the higher the level of education of the mother,
#their kids seem to perform better. Also, studentsä with intention to
#pursue higher education seem to perform better. 
#Alcohol use and absences reduces performance and can result in dramatic
#reduction if it becomes chronic.


plot(predict.gbm(grade_gbm1, data_reg, best.iter1), data_reg$G3, 
     main="Observed vs Predicted grade")
lines(lowess(predict.gbm(grade_gbm1, data_reg, best.iter1), data_reg$G3), col="red", lwd=3)
r_grade <-cor.test(predict.gbm(grade_gbm1, data_reg, best.iter1), data_reg$G3)
r2grade <- r_grade$estimate^2
r2grade
legend("topleft", paste("r^2=", round(r2grade,3)))
#We can see from the scatterplots that the selected variables, account for only
#31% factors affecting the students' grades.




####################################################################
#####################################################################
######################################################################
#ABSENCE
halc_glm<-glm(absences ~sex + age+address+Medu+Fedu+
                Pstatus+ traveltime+studytime+famsup+activities+higher+
                internet+famrel+romantic+freetime+goout+ high_use+G3
              ,data=data_reg,family ="poisson")
summary(halc_glm)
step(halc_glm)

##Final model 
halc_glm<-glm(absences ~ sex + age + Medu + 
                Pstatus +  studytime +  higher + 
                internet + goout + alc_use + G3
              ,data=data_reg,family ="poisson")

anova(halc_glm, test="Chisq")
summary(halc_glm)


anova(halc_glm, test="Chisq")
#########################################################################
#dividing into 70:30
{rep<-10
for (i in 1:rep){
  print(i)
  
  #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
  rand<- sample(1:nrow(data_reg), size = 0.7*nrow(data_reg))
  cal<- data_reg[rand,]
  eva<-data_reg[-rand,]
  
  ###GLM
  abse_glm <- glm(absences~sex + age + Medu + 
                    Pstatus +  studytime +  higher + 
                    internet + goout + high_use + G3, data=cal, family = "poisson") 
  
  abse_glm_pred<- predict.glm(object = abse_glm, newdata = eva, type="response")
  
  cor_glm_abse<-cor(abse_glm_pred, eva$absences, method = "spearman")
  #abse_pred_glm_ras<- predict(model=abse_glm, object = ras_stack, fun=predict.glm, type = "response")
  #plot(abse_pred_glm_ras)
  
  #########
  #mean error and root mean square error
  error_abse_glm<- cbind.data.frame(abse_glm_pred, eva$absences)
  colnames(error_abse_glm) <- c("pred_glm_abse", "obs_abse")
  
  abse_glm_me <- mean_error(error_abse_glm$obs_abse, error_abse_glm$pred_glm_abse)
  abse_glm_rmse <- rmse(error_abse_glm$obs_abse, error_abse_glm$pred_glm_abse)
  
  me_rmse_abse_glm <- rbind.data.frame(abse_glm_me, abse_glm_rmse)
  colnames(me_rmse_abse_glm)<- c("abse_glm")
  
  
  
  
  #GAM
  abse_gam <- gam(absences~ sex + s(age, k=3) +s(Medu, k=3) + 
                    Pstatus+  s(studytime, k=3) +  higher + 
                    internet + goout + s(alc_use, k=3) +  s(G3, k=3), data = cal, family = "poisson")
  abse_gam_pred <- predict.gam(abse_gam, newdata = eva, type = "response")
  
  obs_pred_abse_gam<- cbind.data.frame(abse_gam_pred, eva$absences)
  colnames(obs_pred_abse_gam) <- c("pred_gam_abse", "obs_gam_abse")
  #you can just calclate the correlation straight away
  cor_gam_abse <- cor(abse_gam_pred, eva$absences, method = "spearman")
  
  
  #########
  #mean error and root mean square error
  error_abse_gam<- cbind.data.frame(abse_gam_pred, eva$absences)
  colnames(error_abse_gam) <- c("pred_gam_abse", "obs_abse")
  
  abse_gam_me <- mean_error(error_abse_gam$obs_abse, error_abse_gam$pred_gam_abse)
  abse_gam_rmse <- rmse(error_abse_gam$obs_abse, error_abse_gam$pred_gam_abse)
  
  me_rmse_abse_gam <- rbind.data.frame(abse_gam_me, abse_gam_rmse)
  colnames(me_rmse_abse_gam)<- c("abse_gam")
  
  
  
  
  ###################################################################3
  #using the normal gbm, package.
  #GBM
  abse_gbm1<-gbm(formula = absences~ sex + age+address+Medu+Fedu+
                   Pstatus+ traveltime+studytime+famsup+activities+higher+
                   internet+famrel+romantic+freetime+goout+ alc_use, data=cal,
                 distribution = "poisson",n.trees = 1000, shrinkage = 0.001, interaction.depth = 6,
                 bag.fraction = 0.75)
  
  best.iter<-gbm.perf(abse_gbm1, plot.it = F, method = "OOB")
  abse_gbm1_pred<- predict.gbm(object = abse_gbm1, newdata = eva, best.iter,
                               type="response")
  cor_gbm1_abse <- cor(abse_gbm1_pred, eva$absences, method = "spearman")
  
  
  
  #########
  #mean error and root mean square error
  error_abse_gbm1<- cbind.data.frame(abse_gbm1_pred, eva$absences)
  colnames(error_abse_gbm1) <- c("pred_gbm1_abse", "obs_abse")
  
  abse_gbm1_me <- mean_error(error_abse_gbm1$obs_abse, error_abse_gbm1$pred_gbm1_abse)
  abse_gbm1_rmse <- rmse(error_abse_gbm1$obs_abse, error_abse_gbm1$pred_gbm1_abse)
  
  me_rmse_abse_gbm1 <- rbind.data.frame(abse_gbm1_me, abse_gbm1_rmse)
  colnames(me_rmse_abse_gbm1)<- c("abse_gbm1")
  
  # par(mfrow=c(2,3))
  # plot.gbm(abse_gbm1, 1, best.iter)
  # plot.gbm(abse_gbm1, 2, best.iter)
  # plot.gbm(abse_gbm1, 3, best.iter)
  # plot.gbm(abse_gbm1, 4, best.iter)
  # plot.gbm(abse_gbm1, 5, best.iter)
  # plot.gbm(abse_gbm1, 6, best.iter)
  # par(mfrow=c(1,1))
  
  
  
  ###################################################
  #dismo package
  
  abse_gbm2 <- gbm.step(data=cal, gbm.x =c("sex", "age","address","Medu"
                                           ,"Fedu","Pstatus", "traveltime","studytime","famsup","activities","higher",
                                           "internet","famrel","romantic","freetime","goout", "alc_use"), gbm.y = "absences",
                        bag.fraction=0.75, learning.rate = 0.001,
                        family="poisson",n.trees=50, n.folds=10,
                        max.trees = 1000, tree.complexity = 6)
  
  #This also works but can be done directly as shown in the prediction after this
  #best.iter2<-gbm.perf(abse_gbm1, plot.it = T, method = "OOB")
  # abse_gbm2_pred<- predict.gbm(object = abse_gbm2, newdata = eva, best.iter2,
  #                              type="response")
  
  #
  #the prediction
  abse_gbm2_pred <- predict.gbm(abse_gbm2, newdata = eva, n.trees=abse_gbm2$n.trees, type = "response")
  #abse_pred_gbm2_ras <- predict(object=ras_stack,model=abse_gbm2, fun=predict,
  #                         n.trees=abse_gbm2$n.trees, type="response")
  
  #plot(abse_pred_gbm2_ras)
  cor_gbm2_abse <- cor(abse_gbm2_pred, eva$absences, method = "spearman")
  
  #########
  #mean error and root mean square error
  error_abse_gbm2<- cbind.data.frame(abse_gbm2_pred, eva$absences)
  colnames(error_abse_gbm2) <- c("pred_gbm2_abse", "obs_abse")
  
  abse_gbm2_me <- mean_error(error_abse_gbm2$obs_abse, error_abse_gbm2$pred_gbm2_abse)
  abse_gbm2_rmse <- rmse(error_abse_gbm2$obs_abse, error_abse_gbm2$pred_gbm2_abse)
  
  me_rmse_abse_gbm2 <- rbind.data.frame(abse_gbm2_me, abse_gbm2_rmse)
  colnames(me_rmse_abse_gbm2)<- c("abse_gbm2")
  
  
  
  
} 
#####All correlation
all_cor_abse <- cbind.data.frame(cor_glm_abse,cor_gam_abse,
                                 cor_gbm1_abse, cor_gbm2_abse)
colnames(all_cor_abse)<- c("abse_glm", "abse_gam", "abse_gbm1", "abse_gbm2")

#####all error
all_error_abse <- cbind.data.frame(me_rmse_abse_glm, me_rmse_abse_gam,
                                   me_rmse_abse_gbm1, me_rmse_abse_gbm2)
rownames(all_error_abse)<- c("mean abs error", "RMSE")

}



#Using all the models to see the prediction
abse_gbm1<-gbm(formula = absences~ sex + age+address+Medu+Fedu+
                 Pstatus+ traveltime+studytime+famsup+activities+higher+
                 internet+famrel+romantic+freetime+goout+ alc_use, data=data_reg,
               distribution = "poisson",n.trees = 1000, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)

summary(abse_gbm1)

#I chose GBM because it is able to handle multicollinearity and complex interactions,
#it can also show the response curves and relative importance of predictors.

#alcohol use, age, mother's education, parents' status, freetime and romantic
#relationship ostensibly, have the most effect on the cause of absences.
#Travel time and address seem to be the least

#now, let's see the response curves of this


plot(abse_gam, pages=1)
#Teenage students are more likely to be absent although, there seems 
#not to be enough data for older students above 20, as the confidence interval'
#seem high. Also. the likelihood of absences reduces with increase in the level
#of education of the mother. Studytime also reduces this. While Alcohol increases
#this. It is quite unsure how grade affects.

#I will be expploring this further with GBM reponse curves

best.iter1<-gbm.perf(abse_gbm1, plot.it = F, method = "OOB")


par(mfrow=c(2,3))
plot.gbm(abse_gbm1, "alc_use", best.iter1)
plot.gbm(abse_gbm1, "age", best.iter1)
plot.gbm(abse_gbm1, "Medu", best.iter1)
plot.gbm(abse_gbm1, "Pstatus", best.iter1)
plot.gbm(abse_gbm1, "freetime", best.iter1)
plot.gbm(abse_gbm1, "romantic", best.iter1)
par(mfrow=c(1,1))

#Alcohol use, age mother's education seem to increase the tendency to be absent
#while freetime does the opposite. This is quite surprising as, I had expected the
#exact opposite.
#Also, students' with parents apart have more tendency to be absent that
#those with their parents together. Likewise, students in romantic relationship
#are more likely to be absent than those without.



plot(predict.gbm(abse_gbm1, data_reg, best.iter1), data_reg$absences, 
     main="Observed vs Predicted absences")
lines(lowess(predict.gbm(abse_gbm1, data_reg, best.iter1), data_reg$absences), col="red", lwd=3)
r_abse <-cor.test(predict.gbm(abse_gbm1, data_reg, best.iter1), data_reg$absences)
r2abse <- r_abse$estimate^2
r2abse
legend("topleft", paste("r^2=", round(r2abse,3)))
#We can see from the scatterplots that the selected variables, account for only
#23% factors affecting the students' absences.




#########################################################################
##########################################################################
###########################################################################








#HIGH ALCOHOL

#Initial model for GLM
halc_glm<-glm(high_use ~sex + age+address+Medu+Fedu+
                Pstatus+ traveltime+studytime+famsup+activities+higher+
                internet+famrel+romantic+freetime+goout+ absences
              ,data=data_reg,family ="binomial")

summary(halc_glm)

stepAIC(halc_glm, direction = "both")

#final model for high alcohol use
halc_glm<-glm(high_use ~sex + studytime + goout + absences
              ,data=data_reg,family ="binomial")

anova(halc_glm, test = "Chisq")

############################################################
#####################################

#dividing into 70:30
{
  
  halc_auc_glm<-halc_auc_gam<-halc_auc_gbm1<-halc_auc_gbm2<-c()  
  rep<-10
  for (i in 1:rep){
    print(i)
    
    #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
    rand<- sample(1:nrow(data_reg), size = 0.7*nrow(data_reg))
    cal<- data_reg[rand,]
    eva<-data_reg[-rand,]
    
    ####
    #GLM
    halc_glm <- glm(high_use~  sex + studytime + goout + absences, data=cal, family = "binomial") 
    
    halc_glm_pred<- predict.glm(object = halc_glm, newdata = eva, type="response")
    #check the AUC of the compared prediction and evaluation
    halc_auc_glm_p<-colAUC(halc_glm_pred, eva$high_use , plotROC=F)
    halc_auc_glm <- c(halc_auc_glm, halc_auc_glm_p[[1]])
    
    #GAM
    halc_gam<-gam(high_use~ sex+ s(studytime, k=3) + s(goout, k=3) + 
                    s(absences, k=3) , data = cal, family = "binomial")
    
    pred_halc_gam<-predict.gam(halc_gam, newdata = eva, type = "response")
    halc_auc_gam_p<-colAUC(pred_halc_gam, eva$high_use , plotROC=F)
    halc_auc_gam <- c(halc_auc_gam, halc_auc_gam_p[[1]])
    
    #GBM1
    halc_gbm1<-gbm(formula = high_use~ sex + age+address+Medu+Fedu+
                     Pstatus+ traveltime+studytime+famsup+activities+higher+
                     internet+famrel+romantic+freetime+goout+ absences, data=cal,
                   distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
                   bag.fraction = 0.75)
    
    best.iter1<-gbm.perf(halc_gbm1, plot.it = F, method = "OOB")
    pred_halc_gbm1<-predict.gbm(halc_gbm1,newdata = eva, best.iter1, type = "response")
    
    halc_auc_gbm_p1<-colAUC(pred_halc_gbm1, eva$high_use , plotROC = F)
    halc_auc_gbm1<- c(halc_auc_gbm1, halc_auc_gbm_p1[[1]])
    
    #GBM2 dismo
    halc_gbm2<-gbm.step(data=cal, gbm.x =
                          c("sex", "age","address","Medu"
                            ,"Fedu","Pstatus", "traveltime","studytime","famsup","activities","higher",
                            "internet","famrel","romantic","freetime","goout", "absences"), gbm.y = "high_use",
                        bag.fraction=0.75, learning.rate = 0.001,
                        family="bernoulli",n.trees=50, n.folds=10,
                        max.trees = 3000, tree.complexity = 6)
    
    #prediction
    pred_halc_gbm2 <- predict.gbm(halc_gbm2, newdata = eva, n.trees=halc_gbm2$n.trees, type = "response")
    #The above can also be done usin the next two steps below.
    # best.iter2<-gbm.perf(halc_gbm2, plot.it = F, method = "OOB")
    # pred_halc_gbm2<-predict.gbm(halc_gbm2,newdata = eva, best.iter2, type = "response")
    # 
    
    #plotting ROC curve and getting the value
    halc_auc_gbm_p2<-colAUC(pred_halc_gbm2, eva$high_use , plotROC = T)
    #coombining t he value into a list
    halc_auc_gbm2<- c(halc_auc_gbm2, halc_auc_gbm_p2[[1]])
    
    
  } 
  compared_model_halc=cbind.data.frame(halc_auc_glm, halc_auc_gam, 
                                       halc_auc_gbm1,halc_auc_gbm2)
}
# compared_model_halc
mean_auc_halc<-colMeans(compared_model_halc)
#  attach(compared_model_halc)
# wilcox.test(halc_auc_gbm1, halc_auc_gam, paird=T)

#Here, I utilised Area Under Curve for evaluating my model. This 
#is because it prevents subjectiveness in selecting thresholds which can
#significantly affect the predictive performance and commission and omission error.
#Although, measures, such as prevalence have been recommended for dealing
#with selection of threshold to conver into binary(e.g, True or False).
#AUC readily takes care of this by considering all the possible thresholds
#and evaluates the performance, accordingly. a value of >0.9 is an excellent model
# while AUC values with range 0.7-0.8, 0.5-0.7, 0.5 are fairly good, poor and
#very poor respectively. 
#Here, my AUC values for all the models thorugh my boosting and resampling
#are about 0.7 for the three different models(GLm, GAM, GBM) which I applied.


####################################################
#Model for alcohol use
#RESPONSE CURVES AND MODEL INTERPRETATIONS.
#Here, i decided to use the entire data for the analysis
#GAM
halc_gam<-gam(high_use~ sex+ s(studytime, k=3) + s(goout, k=3) + 
                s(absences, k=3) , data =data_reg, family = "binomial")
summary(halc_gam)
#plot the response curves from GAM
plot(halc_gam, pages=1)
#From the above, we can see the response of high alcohol use to the various
#predictors and also the confidence interval. There appears to be lesser tendency
#of high alcohol use when study time increases. This is expected, as the student
#has lesser time for social activities and parties. 
#on the other hand, expectedly, going out increases the tendency of high alcohol
#use. In similar vein, absence from school increases the tendency too.
#but as it can be seen from the plot, the confidence interval seems to reduce
#with increase in number of absences, which might be an indication of 
#insufficient data from that region.

#To get a deeper, insight, I will explore this further with GBM

#GBM
halc_gbm1<-gbm(formula = high_use~ sex + age+address+Medu+Fedu+
                 Pstatus+ traveltime+studytime+famsup+activities+higher+
                 internet+famrel+romantic+freetime+goout+ absences, data=data_reg,
               distribution = "bernoulli",n.trees = 2800, shrinkage = 0.001, interaction.depth = 6,
               bag.fraction = 0.75)
summary(halc_gbm1)
###From the relative importance, we can see that goout, absences, sex and family
#relationship seem to have the highest effect on high alcohol use. 
#The least important factors can also be seen from the model summary.



#Now, i'll show the response curve from GBM to see the effects

plot(halc_gbm1)
best.iter1<-gbm.perf(halc_gbm1, plot.it = F, method = "OOB")

# pred_halc_gbm1<-predict.gbm(halc_gbm1,newdata = eva, best.iter1, type = "response")

par(mfrow=c(2,2))
plot.gbm(halc_gbm1, "goout", best.iter1)
plot.gbm(halc_gbm1, "absences", best.iter1)
plot.gbm(halc_gbm1, "sex", best.iter1)
plot.gbm(halc_gbm1, "famrel", best.iter1)
par(mfrow=c(1,1))
plot.gbm(halc_gbm1, "studytime", best.iter1)

#As it can also be seen from the GM reponse curves, absences and going out, 
#increases the tendency of high alcohol use. Going out steeply affects when
#it is more than average. Absences of slighty higher than 10 times can 
#even increase the tendency. Family relationship however, reduces the tendency.
#and overall, male students seem to have relatively more high alcohol use than
#their female counterpart. Also, as shown earlier, more study time appears
#to reduce the tendency of high alcohol use.




###############################################################
#LINEAR DISCRIMINANT ANALYSIS.

#see the numeric variables.
cf<-Filter(is.numeric, alco_data)
mm<- scale(cf)
summary(mm)
lda.fit <- lda(~., data = mm)
