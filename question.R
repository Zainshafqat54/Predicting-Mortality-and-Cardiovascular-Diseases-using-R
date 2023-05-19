Dr=getwd()
setwd(Dr)
# loading the data
data<-read.csv("heart_failure_clinical_records_dataset .csv")
# Summary of the data
summary(data)

#Question-1

#Subset of people_with_diabetes
people_with_diabetes <- subset(data ,  diabetes=='1')
people_with_diabetes
#create a table of patients with diabetes and high blood pressure only 
table(people_with_diabetes$diabetes,people_with_diabetes$high_blood_pressure) -> temp_table
temp_table
#Draw a bar plot of table 
barplot(temp_table , legend=T , beside = T ,xlab = "Diabetes_Patient",ylab = "Number_Of_Pateints")

ch_test <- chisq.test(temp_table,correct=FALSE)
ch_test$expected
ch_test$observed


# Question-2

#Subset of people_with_anemia
people_with_anemia <- subset(data ,  anaemia=='1')
people_with_anemia
#create a table of patients with  anemia  with gender
table(people_with_anemia$anaemia,people_with_anemia$sex) -> temp_table
temp_table
#Draw a bar plot of table 
barplot(temp_table  , beside = T ,xlab = "Gender",ylab = "Pateints_Having_Anemia")
ch_test <- chisq.test(temp_table,correct=FALSE)
ch_test$expected
ch_test$observed


# Question-3
#Subset of people_with_specific_age
people_with_specific_age <-subset(data , data$age >=55 & data$age <= 65)
people_with_specific_age
#Subset of people_with_blood_pressure
people_with_blood_pressure <- subset(people_with_specific_age, high_blood_pressure=="1")
people_with_blood_pressure
#create a table of patients with  blood pressure and there survival or not 
table(people_with_blood_pressure$high_blood_pressure,people_with_blood_pressure$DEATH_EVENT) -> temp_table
temp_table
#Draw a bar plot of table 
barplot(temp_table  , beside = T ,xlab = "DEATH_EVENT",ylab = "Pateints_Having_High_Blood_Pressure")
ch_test <- chisq.test(temp_table,correct=FALSE)
ch_test$expected
ch_test$observed



# Question-4
#Subset of people_with_specific_age
people_with_diabetes <- subset(data , data$diabetes=="1")
people_with_diabetes
#Subset of people_with_smoking
people_with_smoking <- subset(people_with_diabetes , people_with_diabetes$smoking=="1")
people_with_smoking
#create a table of patients with  blood pressure and there survival or not 
table(people_with_smoking$smoking,people_with_smoking$DEATH_EVENT) -> temp_table
temp_table
#Draw a bar plot of table 
barplot(temp_table  , beside = T ,xlab = "DEATH_EVENT",ylab = "people_with_smoking")
ch_test <- chisq.test(temp_table,correct=FALSE)
ch_test$expected
ch_test$observed
