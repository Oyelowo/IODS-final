#Name: Oyedayo Oyelowo
#Data wrangling
#data sourc:  https://archive.ics.uci.edu/ml/machine-learning-databases/00320/
#install.packages("dplyr")
library(dplyr)

#Read both student-mat.csv and student-por.csv
filepath<- "C:/Users/oyeda/Desktop/OPEN_DATA_SCIENCE/IODS-project/data/"
math=read.table(paste0(filepath,"student-mat.csv"),sep=";",header=TRUE)
por=read.table(paste0(filepath,"student-por.csv"),sep=";",header=TRUE)

# Join the two data sets
math_por=merge(math,por,by=c("school","sex","age","address","famsize","Pstatus",
         "Medu","Fedu","Mjob","Fjob","reason","nursery","internet"), suffixes = c(".mat", ".por"))
print(nrow(math_por)) # 382 students

#the structure
str(math_por)

#the dimension
dim(math_por)

join_by <- c("school","sex","age","address","famsize","Pstatus","Medu","Fedu","Mjob","Fjob","reason","nursery","internet")

# join the two datasets by the selected identifiers
#math_por <- inner_join(d1, d2, by = join_by, suffix=c(".math",".por"))


# print out the column names of 'math_por'
colnames(math_por)

# create a new data frame with only the joined columns
alc <- select(math_por, one_of(join_by))
#alc<- math_por[, join_by]      #alternative way


# the columns in the datasets which were not used for joining the data
notjoined_columns <- colnames(math)[!colnames(math) %in% join_by]
notjoined_columns

# for every column name not used for joining...
for(column_name in notjoined_columns) {
  # select two columns from 'math_por' with the same original name
  two_columns <- select(math_por, starts_with(column_name))
  # select the first column vector of those two columns
  first_column <- select(two_columns, 1)[[1]]
  
  # if that first column vector is numeric...
  if(is.numeric(first_column)) {
    # take a rounded average of each row of the two columns and
    # add the resulting vector to the alc data frame
    alc[column_name] <- round(rowMeans(two_columns))
  } else { # else if it's not numeric...
    # add the first column vector to the alc data frame
    alc[column_name] <- first_column
  }
}


# define a new column alc_use by combining weekday and weekend alcohol use
alc <- mutate(alc, alc_use = (Dalc + Walc) / 2)
#alternative way
#alc$alc_use<- (alc$Dalc + alc$Walc) / 2

#define a new column for high use of alcohol
alc <- mutate(alc, high_use = alc_use > 2)

#export the data
write.csv(alc, paste0(filepath, "students_wrangled.csv"))
