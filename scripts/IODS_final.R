#clear environment
rm(list=ls())

#read data
fpath<- "C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-final/data/"
alco <- read.table(paste0(fpath, "alc.txt"), sep=",", header=T)

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
