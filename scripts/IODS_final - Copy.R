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

#number of times to repeat the models
{rep<-10
  # grade_cor_glm<-c()
  # grade_cor_gam<-c()
  # grade_cor_gbm<-c()
  for (i in 1:rep){
    print(i)
    #sample all the rows, and keep 80%(0.2)
    rand_sam<-sample(1:nrow(alco_data), size = 0.8*nrow(alco_data) )
    cal<- alco_data[rand_sam,]   #get the 80% rows for calibration
    eva<- alco_data[-rand_sam,]  #get the remaining 20% for evaluation
    
    #create the glm for G3 occurences
    grade_glm<-glm(G3~ Medu + higher,data=cal,family ="poisson")
    pred_grade_glm<-predict.glm(grade_glm, newdata = eva, type = "response")
    grade_cor_glm<-cor(pred_grade_glm, eva$G3, method = "spearman")
    
    
    
    #GAM
    grade_gam<-gam(G3~s(Medu, k=3) + higher, data=cal,family ="poisson")
    pred_grade_gam<-predict.gam(grade_gam, newdata = eva, type = "response")
    grade_cor_gam<-cor(pred_grade_gam, eva$G3, method = "spearman")
    
    #GBM1
    grade_gbm1<-gbm(formula = G3~sex + age+address+Medu+Fedu+
                     Pstatus+ traveltime+studytime+famsup+activities+higher+
                     internet+famrel+romantic+freetime+goout+ alc_use+ absences, data=cal,
               distribution = "poisson",n.trees = 3000, shrinkage = 0.001, interaction.depth = 4)
    best.iter1<-gbm.perf(grade_gbm1, plot.it = F, method = "OOB")
    pred_grade_gbm1<-predict.gbm(grade_gbm1,newdata = eva, best.iter1, type = "response")
    grade_cor_gbm1<-cor(pred_grade_gbm1, eva$G3, method = "spearman")
    
    #GBM2
    grade_gbm2 <- gbm.step(data=cal, gbm.x =c("sex", "age","address","Medu","Fedu",
          "Pstatus", "traveltime","studytime","famsup","activities","higher",
           "internet","famrel","romantic","freetime","goout", "alc_use", "absences"
           ), gbm.y = "G3",bag.fraction=0.75, learning.rate = 0.001,
           family="poisson",n.trees=50, n.folds=10,max.trees = 3000, tree.complexity = 6)
    best.iter2<-gbm.perf(grade_gbm2, plot.it = F, method = "OOB")
    pred_grade_gbm2<-predict.gbm(grade_gbm2,newdata = eva, best.iter2, type = "response")
    grade_cor_gbm2<-cor(pred_grade_gbm2, eva$G3, method = "spearman")    
  } 
  compared_model_grade=cbind.data.frame(grade_cor_glm, grade_cor_gam, grade_cor_gbm1, grade_cor_gbm2)
}
compared_model_grade
summary(grade_gbm)


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
    
    #use the calibration data to predict into the raster stack
    #grade_pred_gam_ras<- predict(object=ras_stack, model=grade_gam, fun=predict.gam,type="response")
    #plot(grade_pred_gam_ras)
    
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
                   distribution = "poisson",n.trees = 1300, shrinkage = 0.001, interaction.depth = 6,
                   bag.fraction = 0.75)
    
    best.iter<-gbm.perf(grade_gbm1, plot.it = F, method = "OOB")
    grade_gbm1_pred<- predict.gbm(object = grade_gbm1, newdata = eva, best.iter,
                                 type="response")
    cor_gbm1_grade <- cor(grade_gbm1_pred, eva$G3, method = "spearman")
    #grade_pred_gbm1_ras<- predict(object=ras_stack,model=grade_gbm1, fun=predict,
    #                          n.trees=grade_gbm1$n.trees, type="response")
    # plot(grade_pred_gbm1_ras)
    
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
                          max.trees = 3000, tree.complexity = 6)
    #best.iter<-gbm.perf(grade_gbm1, plot.it = T, method = "OOB")
    # grade_gbm2_pred<- predict.gbm(object = grade_gbm2, newdata = eva, best.iter,
    #                              type="response")
    
    #this immediately does not work as expected, so, i'm using the next
    #grade_gbm2_pred<- predict(object=,model=grade_gbm2, fun=predict,n.trees=grade_gbm2$n.trees, type="response")
    
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



















#ABSENCE








#ALCOHOL
