install.packages("tidyverse")
library(tidyverse)
#### get the working directory
getwd()

### set the working directory
setwd("D:/R programming")
getwd()

### Reading csv files
titanic <- read.csv ("titanic_data.csv",header=T)

##### structure of the data 
str(titanic)

### first six records
head(titanic)

####### last six records
tail(titanic)

#### Summary of the data
summary(titanic)

#### selecting specific columns
titanic$Survived # selecting a single columns

titanic[,c("Name","Gender","Fare")] # selecting multiple columns

titanic[,5:8] ## select Columns by Index

############ Filter Operations (Row Selection)
# Filter and select passenger above the age of 35
head(titanic[titanic$Age > 35,c("Gender","Age","Fare")])

## selecting using SELECT - Select specific columns
sel_set_1 <- titanic %>% select(Pclass,Age,Fare,Survived)

#### Females only
female_passengers <- titanic %>%
  filter(Gender == "female") %>% select(Pclass, Age , Fare, Survived)

titanic$Survival_Status <- ifelse(titanic$Survived == 1,"Survived","Not Survived")

### Create a FamilyCount Column using mutate #####
titanic <- titanic %>% mutate(FamilyMembers=titanic$SibSp+titanic$Parch)

library(tidyverse)

##### Create a adult column
titanic$AgeGroup <- ifelse(titanic$Age>18,"Adult","Child")


##### Sorting ###############3
# Sort by ascending fare
fares_asc <- titanic %>% arrange(Fare)

# sort by desecnding sequence
fares_asc <- titanic %>% arrange(desc(Fare))

### update the age of passenger id 1 to 23
# what is the current age of passenger id 1
titanic[titanic$PassengerId ==1,"Age"]
titanic$Age[titanic$PassengerId ==1]

# Update it to 23
titanic$Age[titanic$PassengerId ==1] <- 23
titanic$Age[titanic$PassengerId ==1]

# Find the number of male/Female passenger
titanic %>% group_by(Gender) %>% summarise(Average = n())
library(tidyverse)

# count survivors by gender
titanic %>% group_by(Gender) %>% summarise(Survivors =sum(Survivors))

## Count passenger by class
titanic %>% group_by(Pclass) %>% summarise(Count = n())

### Count survivors by class 
titanic %>% group_by(Pclass) %>% summarise(Survivors = sum (Survived)) %>%
  arrange(Survivors)

# writing data to a file
write.csv(titanic,"Titanic_Modified.csv")



###############################################################################
### REading from a text file
text_data<- readLines("text_data.txt")
text_data

students<- read.table("student_marks.txt",header = TRUE)
students

### Writing to a text file
student_data<- data.frame(
  Name = c("Abhishek","Mayuri","Pavan","Nuthan"),
  Age = c(20,19,21,18),
  Marks = c(90,85,78,100)
)

write.table(
  student_data,
  "StudentDetails.txt",
  sep = "\t",
  row.names = FALSE
)






















