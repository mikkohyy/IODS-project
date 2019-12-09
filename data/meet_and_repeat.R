library(dplyr)
library(tidyr)
library(ggplot2)
setwd("data/")

# 1: 

url.1 <- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/BPRS.txt"
url.2 <- "https://raw.githubusercontent.com/KimmoVehkalahti/MABS/master/Examples/data/rats.txt"

bprs <- read.table(url.1,sep=" ", header=TRUE)
rats <- read.table(url.2,sep="\t", header=TRUE)

names(bprs)
str(bprs)
summary(bprs)

names(rats)
str(rats)
summary(rats)

# 2: Categorical variables to factors 

bprs$treatment <- factor(bprs$treatment)
bprs$subject <- factor(bprs$subject)

rats$Group <- factor(rats$Group)
rats$ID <- factor(rats$ID)

# 3: Convert the data sets to long form

bprs.long <- bprs %>% gather(key=weeks, value=bprs, 
                             -treatment, -subject)

rats.long <- rats %>% gather(key=days, value=weight,
                             -ID, -Group)

# Adding the new variables:

bprs.long <- bprs.long %>% mutate(week=as.integer(substr(weeks,5,5)))

rats.long <- rats.long %>% mutate(Time=as.integer(substr(days,3,4)))

# 4: Taking a very serious look at the data

# comparing the data frames:

names(bprs)
names(bprs.long)

summary(bprs)
summary(bprs.long)

glimpse(bprs)
glimpse(bprs.long)

head(bprs)
head(bprs.long)

# bprs.long plotted:

ggplot(bprs.long, aes(x=week,y=bprs,linetype=subject )) +
      geom_line() + 
      scale_linetype_manual(values=rep(1:10,times=4)) +
      facet_grid(.~treatment,labeller=label_both) +
      theme(legend.position="none") +
      scale_y_continuous(limits=c(min(bprs.long$bprs),
                        max(bprs.long$bprs))) +
      scale_x_continuous(breaks=seq(0,8,by=1))

# Graphical summary of the values of bprs.long:

n.week <- bprs.long$week %>% unique() %>% length()

bprs.long.summary <- bprs.long %>% group_by(treatment, week) %>%
                    summarise(mean=mean(bprs),
                    se=sd(bprs) / sqrt(n.week)) %>%
                    ungroup()

ggplot(bprs.long.summary, aes(x=week,y=mean,linetype=treatment,
                             shape=treatment)) +
                          geom_line(aes(colour=treatment)) +
                          scale_linetype_manual(values=c(1,2)) +
                          geom_point(size=1.5,aes(colour=treatment)) +
                          geom_errorbar(aes(ymin=mean-se,ymax=mean+se,
                                            linetype="1",colour=treatment),width=0.3) +
                          theme(legend.position=c(0.85,0.75), text=element_text(size=8)) +
                          scale_y_continuous(name="mean(bprs) +/- standard error(bprs)") 

# rats.long plotted: 

ggplot(rats.long, aes(x=Time,y=weight,group=ID )) +
  geom_line(aes(linetype=Group,colour=Group)) + 
  geom_point(size=1.5,aes(colour=Group,shape=Group)) +
  theme(legend.position="none") +
  scale_y_continuous(limits=c(min(rats.long$weight),
                              max(rats.long$weight))) +
  theme(legend.position="top")

# Graphical summary of the values:

n.week.r <- rats.long$Time %>% unique() %>% length()

rats.long.summary <- rats.long %>% group_by(Group, Time) %>%
  summarise(mean=mean(weight),
            se=sd(weight) / sqrt(n.week.r)) %>%
  ungroup()

ggplot(rats.long.summary, aes(x=Time,y=mean,linetype=Group,
                              shape=Group)) +
  geom_line(aes(colour=Group)) +
  scale_linetype_manual(values=c(1,2,3)) +
  geom_point(size=1.5,aes(colour=Group)) +
  geom_errorbar(aes(ymin=mean-se,ymax=mean+se,
                    linetype="1",colour=Group),width=0.3) +
  theme(legend.position=c(0.85,0.75), text=element_text(size=8)) +
  scale_y_continuous(name="mean(weight) +/- standard error(weight)") 

# Saving the data in csv-form:

write.csv(bprs.long, file="bprs_long.csv")
test <- read.csv("bprs_long.csv",row.names=1)

write.csv(rats.long, file="rats_long.csv")
test <- read.csv("rats_long.csv",row.names=1)
