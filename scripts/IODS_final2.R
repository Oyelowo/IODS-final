#clear data
rm(list = ls())

#load data
fpath<- "C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-final/data/"
lrn_data<- read.table(paste0(fpath, "learning2014.txt"), sep="\t", header=T)
