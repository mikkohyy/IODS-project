# Mikko Hyyryläinen
# 25.11.2019
# Data wrangling part 3
# Data sources:
# http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/human_development.csv
# http://s3.amazonaws.com/assets.datacamp.com/production/course_2218/datasets/gender_inequality.csv

library(dplyr)
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
