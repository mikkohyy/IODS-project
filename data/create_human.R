# Mikko Hyyryläinen
# 25.11.2019
# Data wrangling part 4 and 5
# Data sources:
# http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv
# http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv

library(dplyr)
library(stringr)
setwd("data/")

# 2: Reading the data: 

human_dev <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv", stringsAsFactors = F)
gender_ineq <- read.csv("http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv", stringsAsFactors = F, na.strings = "..")

# 3: Exploring the dataset:

dim(human_dev)
str(human_dev)
summary(human_dev)

dim(gender_ineq)
str(gender_ineq)
summary(gender_ineq)

# 4: Renaming the variables of the data frames

human_dev <- human_dev %>% rename(
              hdi_rank = HDI.Rank,
              country = Country,
              hdi_index = Human.Development.Index..HDI.,
              life_exp = Life.Expectancy.at.Birth,
              exp_edu_yrs = Expected.Years.of.Education,
              mean_edu_yrs = Mean.Years.of.Education,
              gni = Gross.National.Income..GNI..per.Capita,
              gni_rank = GNI.per.Capita.Rank.Minus.HDI.Rank
              )

gender_ineq <- gender_ineq %>% rename(
              gii_rank = GII.Rank,
              country = Country,
              gii_index = Gender.Inequality.Index..GII.,
              mat_mor_rat = Maternal.Mortality.Ratio,
              adol_brate = Adolescent.Birth.Rate,
              parl_rep_pct = Percent.Representation.in.Parliament,
              sec_edu_f = Population.with.Secondary.Education..Female.,
              sec_edu_m = Population.with.Secondary.Education..Male.,
              labr_rate_f = Labour.Force.Participation.Rate..Female.,
              labr_rate_m = Labour.Force.Participation.Rate..Male.
              )

# 5: Creating two new variables 

gender_ineq <- gender_ineq %>% mutate(
          edu_ratio=sec_edu_f/sec_edu_m,
          labr_ratio=labr_rate_f / labr_rate_m
          )

# 6: Joining the the datasets

joined_sets <- inner_join(human_dev,gender_ineq,by="country")

dim(joined_sets) #195 observations and 9 variables

write.csv(joined_sets,file="human.csv",row.names=FALSE)

# Reading and testing the saved data: 

test <- read.csv(file="human.csv")
dim(test)==dim(joined_sets)

#########################################
# 5: Dimensionality reduction techniques#  
#########################################

# 1: Mutate human_data$gni

human_data <- read.csv(file="human.csv")

human_data <- mutate(human_data, gni=str_replace(human_data$gni,
                                                pattern=",",
                                                replace="") 
                                                %>% as.numeric)
# 2: Exclude unneeded variables

# Columns to keep: 

# "Country", "Edu2.FM", "Labo.FM", "Edu.Exp", 
# "Life.Exp", "GNI", "Mat.Mor", "Ado.Birth", 
# "Parli.F"

keep_columns <- c("country","edu_ratio","labr_ratio",
                  "exp_edu_yrs","life_exp","gni",
                  "mat_mor_rat","adol_brate",
                  "parl_rep_pct")

human_data <- dplyr::select(human_data,one_of(keep_columns))

# 3: Remove rows with missing values

human_data_filtered <- filter(human_data,complete.cases(human_data))

# 4: Remove observations which relate to regions

tail(human_data_filtered,n=20)

# It seems that last country is Niger and observations
# after it (156->) should be removed

end <- nrow(human_data_filtered)-7
human_data_filtered <- human_data_filtered[1:end,]

# 5:

# Country names as rownames:

rownames(human_data_filtered) <- human_data_filtered$country

# Remove the country-column (its position is 1):

human_data_filtered <- human_data_filtered[,-1]

# Saving the data frame as .csv with row names:

write.csv(human_data_filtered,"human.csv", row.names=TRUE)

# Testing:

test <- read.csv("human_f.csv",row.names=1)
dim(test) # 155 rows and 8 columns
