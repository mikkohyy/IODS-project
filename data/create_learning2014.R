# Mikko Hyyryläinen
# 11.11.2019
# Data wrangling of chapter 2

library(dplyr)

# 2:

data.url <- "http://www.helsinki.fi/~kvehkala/JYTmooc/JYTOPKYS3-data.txt"
learning.data <- read.table(file=data.url,sep="\t",header = TRUE)

# Exploring the data: 

dim(learning.data)
str(learning.data)

# The data has 183 rows and 60 colums 
# (or 183 observations of 60 variables)
# Gender is a factor and the other variables are integers. 

# 3: 

# deep

d_sm <- rowMeans(select(learning.data,one_of(c("D03","D11","D19","D27"))))
d_ri <- rowMeans(select(learning.data,one_of(c("D07","D14","D22","D30"))))
d_ue <- rowMeans(select(learning.data,one_of(c("D06","D15","D23","D31"))))
deep <- rowMeans(cbind(d_sm,d_ri,d_ue))

# stra

st_os <- rowMeans(select(learning.data,one_of(c("ST01","ST09","ST17","ST25"))))
st_tm <- rowMeans(select(learning.data,one_of(c("ST04","ST12","ST20","ST28"))))
stra <- rowMeans(cbind(st_os,st_tm))

# surf 

su_lp <- rowMeans(select(learning.data,one_of(c("SU02","SU10","SU18","SU26"))))
su_um <- rowMeans(select(learning.data,one_of(c("SU05","SU13","SU21","SU29"))))
su_sb <- rowMeans(select(learning.data,one_of(c("SU08","SU16","SU24","SU32"))))
surf <- rowMeans(cbind(su_lp,su_um,su_sb))

# additude

names.temp <- c("Da","Db","Dc","Dd","De","Df","Dg","Dh","Di","Dj")
attitude <- rowMeans(select(learning.data,one_of(names.temp)))

# Adding the variables to the data frame:

learning.data$deep <- deep
learning.data$stra <- stra
learning.data$surf <- surf
learning.data$attitude <- attitude

# Creating the analysis data set:

keepers <- c("gender","Age","attitude","deep","stra","surf","Points")
analysis.data <- select(learning.data,one_of(keepers))
colnames(analysis.data)[c(2,7)] <- c("age","points")
analysis.data <- filter(analysis.data, points > 0)

# 4: 

# Writing final.data as csv to "data" folder and testing
# if the structure of the data is correct

setwd("data/")
write.csv(analysis.data,"learning2014.csv",row.names=FALSE)
data.test <- read.csv("learning2014.csv")

str(data.test) # Looks similar to str(analysis.data)
head(data.test) # Looks similar to head(analysis.data)
