## ---- load_libraries ----
library(readxl)
library(dplyr)
library(ggplot2)
library(shiny)
library(knitr)
library(caret)
library(rmarkdown)
library(party)
library(corrplot)

## ---- Set working directory ---- 
Placement_Data_Full_Class <- read_excel("C:/Users/Admin/Desktop/EDICET/STUDENTS PERFORMANCE/Placement_Data_Full_Class.xlsx")
View(Placement_Data_Full_Class)
get
##----Data processing ------
#check for missing values
colSums(is.na(Placement_Data_Full_Class))

#Remove none required columns
Placement_Data_Full_Class$salary<-NULL
Placement_Data_Full_Class$sl_no<-NULL

#Structure of the data
str(Placement_Data_Full_Class)

###----Re-structuring to define the data as numeric strings------
Placement_Data_Full_Class[]<-lapply(Placement_Data_Full_Class,function(x)if(is.character(x))as.factor(x)else{x})
Placement_Data_Full_Class[]<-lapply(Placement_Data_Full_Class,function(x)if(is.factor(x))as.numeric(x)else{x})
str(Placement_Data_Full_Class)

#Statistical implications and correlations
summary(Placement_Data_Full_Class)
summary(Placement_Data_Full_Class)
corrplot(cor(Placement_Data_Full_Class))

###------- Explnatory Data Analysis[EDA] -------------
##1. Does gender affect placements?
count(Placement_Data_Full_Class, vars= 'gender')
Placement_Data_Full_Class %>%
  group_by(gender) %>%
  summarise(gender_count=n())

PnG <- table(Placement_Data_Full_Class$status, Placement_Data_Full_Class$gender)
barplot(PnG, main="Placement by gender and status",
        xlab="Gender Type", 
        col=c("darkblue","red"),
        legend = rownames(PnG), beside=TRUE)

###----. Does Salary affect Gender?------
#count(Placement_Data_Full_Class, vars= 'gender')
#Placement_Data_Full_Class %>%
 # group_by(gender) %>%
#  summarise(gender_count=n())

#SnG <- table(Placement_Data_Full_Class$gender, Placement_Data_Full_Class$salary)
#boxplot(SnG, main="Placement by gender and status",
        #xlab="Gender Type", 
        #col=c("darkblue","red"),
        #legend = rownames(PnG), beside=TRUE)






##2. Does Secondary Education affect placement?#
#Kernel-Density Plot






##2. Does secondary education affect placement?
count(Placement_Data_Full_Class, vars= 'ssc_p')
Placement_Data_Full_Class %>%
  group_by(gender) %>%
  summarise(gender_count=n())

PnSec <- table(Placement_Data_Full_Class$status, Placement_Data_Full_Class$ssc_p)
barplot(PnSec, main="Placement by Secondary and status",
        xlab="secondary grade", 
        col=c("blue","red"),
        legend = rownames(PnSec), beside=TRUE)
#Kernel- Density Plot
#plot first kernel density plot
kd1 <- density(Placement_Data_Full_Class$ssc_p)
plot(kd1, col='blue', lwd=2)

#plot second kernel density plot
kd2 <- density(Placement_Data_Full_Class$status)
lines(kd2, col='red', lwd=2)







#######trial tessting##########
library(dplyr)
library(rpart)
library(rpart.plot)

# Drop variables
clean_Placement_Data_Full_Class <-Placement_Data_Full_Class %>%
  select(-c(status, gender)) %>% 
  #Convert to factor level
  mutate(status = factor(status, levels = c(1, 2), labels = c('palced', 'not placed')),
         gender = factor(dender, levels = c(0, 1), labels = c('m','f'))) %>%
  na.omit()
glimpse(clean_Placement_Data_Full_Class)


#######
create_train <- function(Placement_Data_Full_Class, size = 0.8, train = TRUE) {
  n_row = nrow(Placement_Data_Full_Class)
  total_row = size * n_row
  train_place < - 1: total_row
  if (train == TRUE) {
    return (data[train_place, ])
  } else {
    return (data[-train_place, ])
  }
}

library(rpart)
data_trainplace <- create_train(Placement_Data_Full_Class, 0.8, train = TRUE)
data_test <- create_train_test(Placement_Data_Full_Class , 0.8, train = FALSE)
dim(data_train)




shuffle_index <- sample(1:nrow(Placement_Data_Full_Class))
head(shuffle_index)

titanic <- Placement_Data_Full_Class[shuffle_index, ]
head(titanic)
str(titanic)

library(dplyr)
library(rpart.plot)
# Drop variables
clean_titanic <- titanic %>%
  select(-c(status, salary)) %>% 
  #Convert to factor level
  mutate(pclass = factor(stat, levels = c(1, 2), labels = c('placed', 'not placed')),
         status = factor(status, levels = c(0, 1), labels = c('yes', 'no'))) %>%
  # na.omit()
 
glimpse(clean_titanic)


create_train_test <- function(titanic, size = 0.8, train = TRUE) {
  n_row = nrow(titanic)
  total_row = size * n_row
  train_sample < - 1: total_row
  if (train == TRUE) {
    return (titanic[train_sample, ])
  } else {
    return (titanic[-train_sample, ])
  }
}
data_train <- create_train_test(titanic, 0.8, train = TRUE)
data_test <- create_train_test(titanic, 0.8, train = FALSE)
dim(data_train)
