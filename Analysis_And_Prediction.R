## importing libraries
##----------------------.
library(dplyr)
library(ggplot2)
library(stringr)
library(Hmisc)

## setting up directory
dir <- 'Data_Directory_Path'
setwd(dir)

#Colors used
ylw <- "#d3a72c"
blu <- "#1ec97f"

## Some information needed in the project
## -------------------------------------------------.
#sibsp : siblings(brothers OR Sisters) / Spouses
#Parch : Parents  / Children
##

## reading data
train <- read.csv('train.csv')
test  <- read.csv('test.csv')

## Viewing Data
head(train)
head(test)
dim(train)
describe(train) 

# adding Survived  Column to test data (New Data-frame)
test.survived <- data.frame(Survived = rep("None" , nrow(test)), test[,])

dim(test.survived)
dim(train)

head(test.survived)

#now both have same no. of Columns ...combine train and test.survived 
#by rows
data.combined <- rbind(train , test.survived)

dim(data.combined)

str(data.combined)

data.combined$Pclass <- as.factor(data.combined$Pclass)
data.combined$Survived <- as.factor(data.combined$Survived)


table(data.combined$Survived)
#   0    1 None
# 549  342  418 
#Data is Skewed ... 



ggplot(data.combined[which(data.combined$Survived != "None"),] , aes(x = Survived))+
  geom_bar(fill = blu)

table(data.combined$Pclass)

#  1   2   3 
# 323 277 709 


train$Pclass <- as.factor(train$Pclass)

str(train)

ggplot(train , aes(x = Pclass, fill = factor(Survived))) +
  geom_bar()+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill = "Survived")




train%>%
  filter(Survived == 1 , Pclass == 3)%>%
  summarise(dead_cl2 = n())

# dead  = 97
# alive = 87


# unique names ..
# should be "1309' 
# nrow(train) + nrow(test)

length(unique(as.character(data.combined$Name)))  # 1307

dim(data.combined)
# 2 names were dubplicated
# let's examine them

# returns the index of duplicated Names
which(duplicated(as.character(data.combined$Name))) 


dup.names <- as.character(data.combined[ which(
                          duplicated(as.character(data.combined$Name)))
                                        , "Name"])
dup.names

# now examine their records
data.combined[which(data.combined$Name %in% dup.names) ,]

data.combined%>%
  select(Name)%>%
  head

# see the Miss. and  Mr. thing ?

# str_detect(String , pattern)
misses <- data.combined[which(str_detect(data.combined$Name , "Miss.")) , ]


nrow(misses)
#260


ggplot(misses[misses$Survived != "None" ,] ,
       aes(x = Pclass , fill = factor(Survived))
      )+
  geom_bar()

#########          Mrs

Mrs <- data.combined[which(str_detect(data.combined$Name , "Mrs.")) ,]

dim(Mrs)

summary(Mrs$Age)

#Min 14
#MAX 76
# Mean 36.8


ggplot(Mrs[Mrs$Survived != "None" , ] ,aes(x = Pclass , fill = factor(Survived)))+
  geom_bar()+
  ggtitle("Ladies")
  



#Mrs 
#total no. of people in 
#           Num  Alive  Dead              
# class 1 :: 44   43      1       98%
# class 2 :: 43   38      5       88%
# class 3 :: 42   21      21      50%

dim(Mrs[ (Mrs$Pclass == 1) & (Mrs$Survived == 1) ,])

  
nrow(Mrs)
# 201


#########          Title Function

# takes array of names
# return String (category of the traveller)
# ["Miss" , "Mrs." , "Mr." ,  "Master" , "Other"]
# should be transformed to be factor ... rather than character


## grep(pattern , String)   ->  return Vector of Indices


assign_titles <- function(name){
  
  name <- as.character(name)
  
  if(length(grep("Miss." , name) > 0))
    return("Miss.")
  if(length(grep("Mrs."  , name) > 0))
    return("Mrs.")
  if(length(grep("Mr." , name) > 0))
    return("Mr.")
  if(length(grep("Master" , name) > 0))
    return("Master.")
  else
    return("Other")
}
titles <- NULL

for(i in 1:nrow(data.combined)){
  titles <- c(titles , assign_titles(data.combined[i , "Name"]))
}


data.combined$titles <- as.factor(titles)

head(data.combined)


nrow(train)  #891

ggplot(data.combined[1:891 , ] , aes(x = titles , fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("PClass")

#########        Distribution of Gender

table(test$Sex)  # train  + test

# female   male 
# 466    843
#double ta2reban 


ggplot(data.combined[1:891,] , aes(x = Sex , fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)+
  ggtitle("Gender")


# Mens always more probable to die 

# Age and Gender can make a Very Good Combination
summary(data.combined$Age)

#  Min.  Median   Mean    Max.   NA's 
#  0.17  28.00   29.88   80.00   263 



# Age Vs Class
ggplot(data.combined[1:891,] , aes(x = Age , fill = Survived))+
  geom_histogram(binwidth = 5)+
  facet_wrap(~Pclass)


ggplot(data.combined[1:891,] , aes(x = Age , fill = Survived))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~Sex + Pclass)


###############             Master (young men)


boys <- data.combined[which(data.combined$titles == "Master.") , ]

ggplot(boys[which(boys$Survived != "None") ,] , aes(x = Age , fill = factor(Survived)))+
  geom_bar(binwidth = 10)+
  facet_wrap(~Pclass + Sex)

nrow(boys)  # 61

summary(boys$Age)

#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#  0.330   2.000   4.000   5.483   9.000  14.500       8



summary(misses$Age)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.17   15.00   22.00   21.77   30.00   63.00      50 

ggplot(misses[misses$Survived != "None" ,] , aes(x =Age , fill = Survived))+
  geom_histogram(binwidth = 10)+
  facet_wrap(~Pclass)+
  ggtitle("Miss ages")



#Appears that young females  have different survival rate
misses.alone = misses[ (misses$SibSp == 0) & (misses$Parch == 0),]

nrow(misses)       #260
nrow(misses.alone) #150


# which means there are 150 women came with NO siblings (Brothers or Sisters)
# Nor parents

summary(misses.alone$Age)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 5.00   21.00   26.00   27.23   32.50   58.00      33 



# Can Treat as Categorical ?
summary(data.combined$SibSp)

length(unique(data.combined$SibSp))

data.combined$SibSp <- as.factor(data.combined$SibSp)

str(data.combined)


ggplot(data.combined[1:891,] ,aes(x = as.numeric(SibSp) ,fill = Survived) )+
  geom_histogram(binwidth = 1)+
  facet_wrap(~Pclass + titles)+
  ylim(0 , 20)+
  xlim(0 , 8)

## same for Parch

data.combined$Parch <- as.factor(data.combined$Parch)

ggplot(data.combined[1:891,] ,aes(x = as.numeric(Parch) ,fill = Survived) )+
  geom_histogram(binwidth = 1 , position = "dodge")+
  facet_wrap(~Pclass + titles)+
  ylim(0 , 20)+
  xlim(0 , 8)



## Create Family Size feature [= SibSP + Parch]


temp.SibSp <- c(train$SibSp , test$SibSp)
temp.Parch <- c(train$Parch , test$Parch)

data.combined$family_size <- as.factor(temp.Parch +temp.SibSp + 1) # 1 = me

summary(data.combined$family_size)

# Visualizing it 
ggplot(data.combined[1:891,] ,aes(x = as.numeric(family_size) ,fill = Survived) )+
  geom_histogram(binwidth = 1 )+
  facet_wrap(~Pclass + titles)+
  ylim(0 , 300)



## We look at the ticket feature



str(data.combined$Ticket)

#So many Levels , so Convert into String

data.combined$Ticket <- as.character(data.combined$Ticket)

data.combined[which(data.combined$Ticket== ""), "Name"]


ticket.first.char <- substr(data.combined$Ticket,1,1)

length(unique(ticket.first.char)) #Can be factor now 


data.combined$tick.fc <-as.factor(ticket.first.char)



ggplot(data.combined[1:891,] , aes(x = tick.fc , fill = Survived))+
  geom_bar()

# Now We're Talkin' ... the Most of

# 1st Class -> ticket begins with 1
# 2nd Class -> ticket begins with 2
# 3rd Class -> ticket begins with 3
ggplot(data.combined[1:891,] , aes(x = tick.fc , fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)


## the Fare
###-----------.

summary(data.combined$Fare)

ggplot(data.combined[1:891,] , aes(x = Fare))+
  geom_bar(color = blu)

data.combined[which(data.combined$Fare > 500),
              c("Name" , "Survived" , "Pclass")]



ggplot(data.combined[1:891,] , aes(x= Fare , fill=Survived))+
  geom_histogram(binwidth = 0.5)+
  facet_wrap(~Pclass + titles)


## The Cabin number
## --------------.

str(data.combined$Cabin)

data.combined$Cabin <- as.character(data.combined$Cabin)

# See Most Data Are Null
data.combined$Cabin[1:100]

data.combined$Cabin[which(data.combined$Cabin == "")] <- "U"

# take First Character
first.cabin.char <- substr(data.combined$Cabin, 1 , 1)

length(unique(first.cabin.char)) # 9 ... factor

data.combined$Cabin.fc <- as.factor(first.cabin.char)

ggplot(data.combined[1:891 , ], aes(x = Cabin.fc , fill= Survived))+
  geom_bar()+
  facet_wrap(~Pclass)


ggplot(data.combined[1:891 , ], aes(x = Cabin.fc , fill= Survived))+
  geom_bar()+
  facet_wrap(~Pclass + titles)



## Multiple Cabins (people have Multiple Cabins)
#####.

mult.cabin <- ifelse(str_detect(data.combined$Cabin , " ") , "Y" , "N")
table(mult.cabin)

data.combined$mult.cabin <- as.factor(mult.cabin)

ggplot(data.combined[1:891 , ] , aes(x =mult.cabin , fill= Survived ))+
  geom_bar()+
  facet_wrap(~Pclass)


# embarks (where people are loaded to the ship) 
#Actually [Not important]
#####----.


str(data.combined$Embarked)

ggplot(data.combined[1:891,], aes(x=Embarked , fill = Survived))+
  geom_bar()+
  facet_wrap(~Pclass)



##############################

# Modeling Using Random Fores

#############################


install.packages("randomForest")

library(randomForest)


rf.train.1 <- data.combined[1:891 , c("Pclass" , "titles")]
rf.label <- factor(train$Survived)


set.seed(1000)

rf.model.1 <- randomForest(x=rf.train.1 , 
                           y = rf.label, 
                           importance = T,
                           ntree = 1000)

rf.model.1


rf.train.2 <- data.combined[1:891 , c("Pclass" , "titles" , "SibSp")]

rf.model.2 <- randomForest(x=rf.train.2 , 
                           y = rf.label, 
                           importance = T,
                           ntree = 1000)


rf.model.2


varImpPlot(rf.model.2)

#

# +Parch           
# +Parch + Sibsb   
# +family size    (18% error) 
# +family size + sibsp (18% error)  same when Parch



rf.train.3 <- data.combined[1:891 , c("Pclass" , "titles" , "family_size")]

rf.model.3 <- randomForest(x=rf.train.3 , 
                           y = rf.label, 
                           importance = T,
                           ntree = 1000)


rf.model.3


test.submit.df <- data.combined[892:1309,c("Pclass" , "titles" , "family_size")]


rf.3.pred <- predict(rf.model.3 , test.submit.df)

table(rf.3.pred)


# Submit file :: ID , Prediction
submit.file <-data.frame(PassengerId = rep(892:1309) , 
                         Survived =rf.3.pred )


write.csv(submit.file , file = "RF_SUB_01.csv" , row.names = F)

## removing data from the memory
rm(list=ls())