#clear environment
rm(list=ls())

#read data
fpath<- "C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-final/data/"
alco_data <- read.table(paste0(fpath, "alcohol_student.csv"), sep=",", header=T)

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

dim(alco_data)
str(alco_data)
summary(alco_data)

# ####Grade:
# alco_data2<-alco_data[,c("sex", "age","address","Medu","Fedu",
#   "Pstatus", "traveltime","studytime","famsup","activities","higher",
#   "internet","famrel","romantic","freetime","goout", "alc_use",
#   "higgrade_use","G1","G2","G3", "absences")]
# alco_data2[,"absences"]
# colnames(alco_data2)
# 
# str(alco_data2)
# summary(alco_data2)
# 
# alco_facto<- alco_data[,c("sex","address",
#     "Pstatus","famsup","activities","higher",
#       "internet","romantic","higgrade_use")]
# mca_alco <- MCA(alco_facto, graph = T)
# 
# plot(mca_alco, invisible=c("ind"), habillage="quali")
# 
# 
# alco_data3<-alco_data[,c("sex","address","Medu","Fedu",
#       "Pstatus", "traveltime","studytime","famsup","activities","higher",
#        "internet","famrel","romantic","freetime","goout")]
# str(mm)
# mm<-replace(alco_data3, TRUE, lapply(alco_data2, factor))
# ss<- MCA(alco_facto, graph = T)



grade_glm<-glm(G3~ sex + age+address+Medu+Fedu+
                 Pstatus+ traveltime+studytime+famsup+activities+higher+
                 internet+famrel+romantic+freetime+goout+ alc_use+ absences
               ,data=alco_data,family ="poisson")

grade_glm<-glm(G3~ Medu + higher,data=alco_data,family ="poisson")
summary(grade_glm)

#for GLM, predictors were selected, by employing the significance test from the model
#summary, anova test(Chi Square), AIC values and stepwise regression. I also checked 
#if any of the predictor is curvillinear(i.e has higher order polynomial)
stepAIC(grade_glm, direction = "both")
anova(grade_glm, test = "Chisq")

#In accordance to the principl of parsimony, I decided to use variables
#with significance level of 0.05
#Final model:  glm(G3~ Medu + higher,data=alco_data,family ="poisson")

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
    rand<- sample(1:nrow(alco_data), size = 0.7*nrow(alco_data))
    cal<- alco_data[rand,]
    eva<-alco_data[-rand,]
    
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
                  internet+famrel+romantic+freetime+goout+ alc_use+ absences, data=alco_data,
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


plot(predict.gbm(grade_gbm1, alco_data, best.iter1), alco_data$G3, 
     main="Observed vs Predicted grade")
lines(lowess(predict.gbm(grade_gbm1, alco_data, best.iter1), alco_data$G3), col="red", lwd=3)
r_grade <-cor.test(predict.gbm(grade_gbm1, alco_data, best.iter1), alco_data$G3)
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
              ,data=alco_data,family ="poisson")
summary(halc_glm)
step(halc_glm)

##Final model 
halc_glm<-glm(absences ~ sex + age + Medu + 
                Pstatus +  studytime +  higher + 
                internet + goout + alc_use + G3
              ,data=alco_data,family ="poisson")

anova(halc_glm, test="Chisq")
summary(halc_glm)


anova(halc_glm, test="Chisq")
#########################################################################
#dividing into 70:30
{rep<-10
for (i in 1:rep){
  print(i)
  
  #it's not  necessary to use the 1:nrow(data) below. it can be only nrow(data)
  rand<- sample(1:nrow(alco_data), size = 0.7*nrow(alco_data))
  cal<- alco_data[rand,]
  eva<-alco_data[-rand,]
  
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
                 internet+famrel+romantic+freetime+goout+ alc_use, data=alco_data,
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



plot(predict.gbm(abse_gbm1, alco_data, best.iter1), alco_data$absences, 
     main="Observed vs Predicted absences")
lines(lowess(predict.gbm(abse_gbm1, alco_data, best.iter1), alco_data$absences), col="red", lwd=3)
r_abse <-cor.test(predict.gbm(abse_gbm1, alco_data, best.iter1), alco_data$absences)
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
              ,data=alco_data,family ="binomial")

summary(halc_glm)

stepAIC(halc_glm, direction = "both")

#final model for high alcohol use
halc_glm<-glm(high_use ~sex + studytime + goout + absences
              ,data=alco_data,family ="binomial")

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
    rand<- sample(1:nrow(alco_data), size = 0.7*nrow(alco_data))
    cal<- alco_data[rand,]
    eva<-alco_data[-rand,]
    
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
                s(absences, k=3) , data =alco_data, family = "binomial")
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
                 internet+famrel+romantic+freetime+goout+ absences, data=alco_data,
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