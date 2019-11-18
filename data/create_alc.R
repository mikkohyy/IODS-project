# Mikko Hyyryläinen
# 18.11.2019
# Data wrangling part 2
# Source: https://archive.ics.uci.edu/ml/datasets/Student+Performance

library(dplyr)
library(ggplot2)
setwd("IODS-project/data/")

# 3

student_mat <- read.table("student-mat.csv",header=TRUE,sep=";")
student_por <- read.table("student-por.csv",header=TRUE,sep=";")

str(student_mat)
str(student_por)

dim(student_mat)
dim(student_por)

# 4

join_columns <- c("school","sex","age","address",
                  "famsize","Pstatus","Medu","Fedu",
                  "Mjob","Fjob","reason","nursery",
                  "internet")

mat_por <- inner_join(student_mat,student_por,by=join_columns,suffix=c(".mat",".por"))

glimpse(mat_por)

# 5

# data frame with joined columns:
  
alc_data <- select(mat_por,one_of(join_columns))

# The names of the columns that are dublicated because
# of inner join:

notjoined_columns <- colnames(student_mat)[!colnames(student_mat)
                      %in% join_columns]

# We need to combine these columns: 

for(name in notjoined_columns) {
  two_columns <- select(mat_por,starts_with(name))
  first <- select(two_columns,1)[[1]]
  if(is.numeric(first)) {
    alc_data[name] <- round(rowMeans(two_columns))
  } else {
    alc_data[name] <- select(two_columns,1)
  }
}

# 6

alc_data <- mutate(alc_data,alc_use=(Dalc+Walc)/2)
alc_data <- mutate(alc_data,high_use=alc_use>2)

# 7 

glimpse(alc_data)
write.csv(alc_data,"part3_alc_data.csv",row.names=FALSE)

test <- read.csv("part3_alc_data.csv")
glimpse(test)

head(alc_data)==head(test)
