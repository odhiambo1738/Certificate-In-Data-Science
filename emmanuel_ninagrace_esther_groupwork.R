#STRUCTURE OF THE DATASET----
#setting working directory
setwd("c:/Users/Lenovo/Desktop/R data Science/codes/")

#needed packages
library(readr)
library(dplyr)
library(ggplot2)
library(mosaicData)
library(rpart)
library(rpart.plot)


#loading needed dataset to our environment----
titanic <- read_csv("~/titanic.csv")
str(titanic)
#the dataset uploaded is a tibble

#converting to data frame from tibble
titanic <- as.data.frame(titanic)
str(titanic)
glimpse(titanic)

#STRUCTURE OF THE DATASET
glimpse(titanic)

#converting survived,pclass,sex and embarked to factor----
fact<-c("Survived","Pclass","Sex","Embarked")
for (i in fact){
  titanic[,i] <- as.factor(titanic[,i])
}

glimpse(titanic)

#identifying missing entries----
#missing entries
colSums(is.na(titanic))
#we see age has 177 missing entries,cabin 687 and embarked 2 

#removing missing entries 
titanic_complete <- titanic %>% 
  filter(complete.cases(titanic))

#calculating the percentage of data lost
(1- nrow(titanic_complete)/nrow(titanic))*100 
#we see that we have lost 79% of our data. we cannot drop all missing entries

#age-we decide to replace missing entries with mean age
#calculating mean age 
mean_age <- mean(titanic$Age,na.rm = TRUE)
#mean age is 29.69912

#replacing missing data on age column with the mean age
titanic$Age <- ifelse(is.na(titanic$Age), mean_age,titanic$Age)

#cabin
(sum(is.na(titanic$Cabin))/891)*100
#77% of the cabin column has missing values therefore we decide to drop the column for cabin

#creating new dataset titanic_new where we drop the cabin column
titanic_new <- titanic[,-11]

#confirming that we dropped the cabin column
colSums(is.na(titanic_new))

#embarked
#we replace the two missing values with S
which(is.na(titanic_new$Embarked))

titanic_new$Embarked[c(62,830)]<- "S"

colSums(is.na(titanic_new))
#all missing entries have been settled by dropping the cabin column, replacing missing values of age with mean age and replacing missing values for embarked with S

#DESCRIPTIVE ANALYSIS----
#number of passengers
count(titanic_new)
#there were 891 passengers on the ship 

#Gender----
titanic_new %>% 
  group_by(Sex) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#there were 577(64.8%)males and 314(35.2%) females in the ship

#with regards to survival
titanic_new %>% 
  group_by(Sex,Survived) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#males who survived were 109(12.2%) while 468(52.5%) died
#females who survived were 233(26.2%) while 81(9.09%) died

#with regards to passenger class
titanic_new %>% 
  group_by(Sex,Pclass) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#males in Pclass1 were 122(13.7%),Pclass2 108(12.1%) and Pclass3 347(38.9%)
#females in Pclass1 were 94(10.5%) ,Pclass2 76(8.53%) and Pclass3 144(16.2%)

#with regards to point of departure
titanic_new %>% 
  group_by(Sex,Embarked) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#males from Cherbourg were 95(10.7%),from Queenstown 41(4.6%) and Southampton 441(49.5%)
#females from Cherbourg were 73(8.19%),from Queenstown 36(4.04%) and Southampton 205(23%)

#Passenger Class----
titanic_new %>% 
  group_by(Pclass) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#1st class had 216(24.2%) passengers
#2nd class had 184(20.7%) passengers 
#3rd class had 491(55.1%) passengers

#with regards to survival
titanic_new %>% 
  group_by(Pclass,Survived) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#1st class had 136(15.3%) who survived 80(8.98%) died
#2nd class had 87(9.76%) who survived 97(10.9%) died
#3rd class had 119(13.4%) who survived 372(41.8%) died


#with regards to point of departure
titanic_new %>% 
  group_by(Pclass,Embarked) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#1st class from Cherbourg were 85(9.45%) from Queenstown were 2(0.224%) and from Southhampton were 129(14.5%)
#2nd class from Cherbourg were 17(1.91%) from Queenstown were 3(0.337%) and from Southhampton were 164(18.4%)
#3rd class from Cherbourg were 66(7.41%) from Queenstown were 72(8.08%) and from Southhampton were 353(39.6%)

#Embarked----
titanic_new %>% 
  group_by(Embarked) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#Cherbourg had 168(18.9%) passengers
#Queenstown had 77(8.64%) passengers
#Southampton had 646(72.5%) passengers


#with regards to survival
titanic_new %>% 
  group_by(Embarked,Survived) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#Cherbourg had 93(10.4%) who survived 75(8.42%) died
#Queenstown had 30(3.37%) who survived 47(5.27%) died
#Southampton had 219(24.6%) who survived 427(47.9%) died

#Age----
#statistical analysis
titanic_new %>% 
  summarise(mean=mean(Age),median=median(Age),min=min(Age),max=max(Age),range=max(Age)-min(Age))
#the mean and median age lie at the same value of 29.7 with the minimum and maximum values being 0.42 and 80 years respectively

#age classsification
#creating a new column for age classification(<18=child,>=18=adult)
titanic_new$Adult <- ""
titanic_new$Adult <- ifelse(titanic_new$Age>=18,"Adult","Child")

#distribution
titanic_new %>% 
  group_by(Adult) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#there are 778(87.3%) adults and 113(12.7%) children 

#classification of age with regards to survival
titanic_new %>% 
  group_by(Adult,Survived) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#survived- 281(31.5%)adults and 61(6.85%) children
#died-497(55.8%) adults and 52(5.84%) children


#Family Size----
#we create a new variable for family size
titanic_new$Fsize <- ""
titanic_new$Fsize <- titanic_new$SibSp+titanic_new$Parch+1

#distribution of family size
titanic_new %>% 
  group_by(Fsize) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#passengers with family size of 1 are the largest proportion of passengers at 60.3% (537passengers)

#with regards to survival
titanic_new %>% 
  group_by(Fsize,Survived) %>% 
  summarise(count=n(),percentage=(count=n()/891)*100)
#for passengers with 1 family member 374(42%) died while 163(18.3%) survived


#Fare----
#statistical analysis
titanic_new %>% 
  summarise(mean=mean(Fare),median=median(Fare),min=min(Fare),max=max(Fare),range=max(Fare)-min(Fare))
#average fare on the titanic was 32.2 ,median of 14.5 a maximum value of 512 with the minimum fare being 0

#statistical analytics of fare with regards to gender
titanic_new %>% 
  group_by(Sex) %>% 
  summarise(mean=mean(Fare),median=median(Fare),min=min(Fare),max=max(Fare),range=max(Fare)-min(Fare))
#on average females paid more than males
#average fare females is 44.5 and males is 25.5

#statistical analysis of fare with regards to passenger class
titanic_new %>% 
  group_by(Pclass) %>% 
  summarise(mean=mean(Fare),median=median(Fare),min=min(Fare),max=max(Fare),range=max(Fare)-min(Fare))
#1st class had mean fare of 84.2 with a range of 512
#2nd class had mean fare of 20.7 with range of 73.5
#3rd class had mean fare of 13.7 with range of 69.6

#statistical analysis of fare with regards to point of embarcation
titanic_new %>% 
  group_by(Embarked) %>% 
  summarise(mean=mean(Fare),median=median(Fare),min=min(Fare),max=max(Fare),range=max(Fare)-min(Fare))
#Cherbourg had mean fare of 60 with a range of 508
#Queenstown had mean fare of 13.3 with range of 83.2
#Southampton had mean fare of 127.2 with range of 263
  
#VISUALISATION----
#Gender----
#gender distribution
ggplot(titanic_new,aes(x=Sex))+
  geom_bar(fill="Blue")+
  labs(title="Bar plot showing gender distribution",caption="titanic dataset",x='Gender')
#males are more than females

#gender with regards to survival
#count
ggplot(titanic_new,aes(x=Sex,fill=Survived))+
  geom_bar(position = 'dodge')+
  labs(title="Bar plot showing gender distribution \nwith regards to survived",caption="titanic dataset",x="Gender")

#survival ratios per gender
ggplot(titanic_new,aes(x=Sex,fill=Survived))+
  geom_bar(position = 'fill')+
  labs(title="Bar plot showing gender distribution \n ratio with regards to survived",caption="titanic dataset",x="Gender",y="Frequency")
#males are more than females
# a higher ratio of females survive while a higher ratio of males die
#about 25% of females die,while only about 25% of males survive

#gender with regards to passenger class
ggplot(titanic_new,aes(x=Sex))+
  geom_bar(position = 'dodge')+
  facet_wrap(~Pclass)+
  labs(title="Bar plot showing gender distribution with\n regards to passenger class",caption="titanic dataset",x="Gender")
#highest proportion of males is within the 3rd class followed by the first class then 2nd class
#highest proportion of females is within the 3rd class followed by the first class then 2nd class

#gender with regards to point of embarkation
ggplot(titanic_new,aes(x=Sex))+
  geom_bar()+
  facet_wrap(~Embarked)+
  labs(title="Bar plot showing gender distribution\n with regards to Embarked",caption="titanic dataset",x="Gender")
#highest proportion of males embarked from Southampton followed by C then Q. the trend is similar for females
#Southampton had 4 times the number of males to embark from Queenstown or Cherbourg

#Pclass----
#passenger class disribution
ggplot(titanic_new,aes(x=Pclass))+
  geom_bar(fill="Blue")+
  labs(title="Bar plot showing passenger class distribution",caption="titanic dataset",x="Passenger Class")
#class 3 had the highest number of passengers followed by 1 then class 2

#passenger class with regards to survival
#count
ggplot(titanic_new,aes(x=Pclass,fill=Survived))+
  geom_bar(position = 'dodge')+
  labs(title="Bar plot showing passenger class distribution\n with regards to survived",caption="titanic dataset",x="Passenger Class")
#survival ratios per class
ggplot(titanic_new,aes(x=Pclass,fill=Survived))+
  geom_bar(position = 'fill')+
  labs(title="Bar plot showing passenger class distribution\n ratio with regards to Survived",caption="titanic dataset",x="Passenger Class",y="Frequency")
#class 1 had the highest ratio of survivors at more than 50% followed by class 2 and then 3 

#passenger class with regards to gender
ggplot(titanic_new,aes(x=Pclass))+
  geom_bar(position = 'dodge')+
  facet_wrap(~Sex)+
  labs(title="Bar plot showing passenger class distribution\n with regards to gender",caption="titanic dataset",x="Passenger Class")
#the ratio of passengers per class still follows even when we group according to gender
#this means class 3 had the highest number of passengers followed by 1 then class 2 for both males and females

#passenger class with regards to point of embarkation
ggplot(titanic_new,aes(x=Pclass))+
  geom_bar()+
  facet_wrap(~Embarked)+
  labs(title="Bar plot showing passenger class distribution\n with regards to Embarked",caption="titanic dataset",x="Passenger Class")
#1st & 2nd class-most passengers embarked from S followed by C then Q
#3rd class- most passengers embarked from S followed by Q then C 

#Embarked----
#embarked distribution
ggplot(titanic_new,aes(x=Embarked))+
  geom_bar(fill="Blue")+
  labs(title="Bar plot showing embarked distribution",caption="titanic dataset")
#most passengers embarked from S followed by C then Q

#embarked with regards to survival
#count
ggplot(titanic_new,aes(x=Embarked,fill=factor(Survived)))+
  geom_bar(position = 'dodge')+
  labs(title="Bar plotshowing embarked distribution\n with regards to survived",caption="titanic dataset")
#most passengers who survived embarked from S followed by C then Q, The trend is similar for the number of passengers who died
#survival ratios per embarkation point
ggplot(titanic_new,aes(x=Embarked,fill=factor(Survived)))+
  geom_bar(position = 'fill')+
  labs(title="Bar plot showing embarked distribution\n ratio with regards to survived",caption="titanic dataset",y="Frequency")
#point C had the highest ratio of survivors followed by Q then S 

#embarked with regards to gender
ggplot(titanic_new,aes(x=Embarked))+
  geom_bar()+
  facet_wrap(~Sex)+
  labs(title="Bar plot showing embarked distribution\n with regards to gender",caption="titanic dataset")
#for females most passengers embarked from S followed by C then Q. The trend is similar for males
#generally there was a higher number of males than females who embarked from each station

#embarked with regards to passenger class
ggplot(titanic_new,aes(x=Embarked))+
  geom_bar()+
  facet_wrap(~Pclass)+
  labs(title="Bar plot showing embarked distribution\n with regards to Passenger class",caption="titanic dataset")
#1st & 2nd class-most passengers embarked from S followed by C then Q
#3rd class- most passengers embarked from S followed by Q then C 

#Age----
#age boxplot
ggplot(titanic_new,aes(y=Age))+
  geom_boxplot()+
  labs(title="Boxplot showing age distribution",caption="titanic dataset")
  
#the mean age is 30 with minimum and maximum values of less than 0 and 80 respectively 

#age density graph
ggplot(titanic_new,aes(x=Age))+
  geom_density()+
  labs(title="Density plot showing age distribution",caption="titanic dataset")
#majority of passengers have an age of between 20 and 40 years

#age classification(<18=child,>=18=adult)
#distribution of children and adults
ggplot(titanic_new,aes(x=Adult))+
  geom_bar(fill="Blue")+
  labs(title="Bar plot showing distribution of adults and children ",caption="titanic dataset",x="Adults and Children")
#most of the passengers are adults with adults being around 7 times the number of children

#adults and children with regards to survival
#count
ggplot(titanic_new,aes(x=Adult,fill=Survived))+
  geom_bar(position = 'dodge')+
  labs(title="Bar plot showing distribution of adults and children\nwith regards to survived ",caption="titanic dataset",x="Adults and Children")
#survival ratios
ggplot(titanic_new,aes(x=Adult,fill=Survived))+
  geom_bar(position = "fill")+
  labs(title="Bar plot showing distribution of adults and children\n with survival ratios ",caption="titanic dataset",x="Adults and Children")
#children had a higher ratio of survival as compared to adults

#Family Size----
#distribution of family size
ggplot(titanic_new,aes(x=Fsize))+
  geom_bar(fill="Blue")+
  labs(title="Bar plot showing family size distribution ",caption="titanic dataset",x="Family Size")
#as the family size increases the number of passengers decreases

#family size with regards to survival
#count
ggplot(titanic_new,aes(x=Fsize,fill=factor(Survived)))+
  geom_bar(position = 'dodge')+
  labs(title="Bar plot showing distribution of family size\n with regards to survival ",caption="titanic dataset",x="Family Size")
#survival ratios
ggplot(titanic_new,aes(x=Fsize,fill=factor(Survived)))+
  geom_bar(position = 'fill')+
  labs(title="Bar plot showing family size distribution\n ratios with regards to Gender ",caption="titanic dataset",x="Family Size",y="Frequency")
#passengers with between 2 and 4 as family size have high survival ratios at larger than 50% while passengers with large family sizes have lower survival rates with the highest being no survivors for family sizes of 7 and 10

#Fare ----
#fare boxplot
ggplot(titanic_new,aes(y=Fare))+
  geom_boxplot()+
  labs(title="Box plot showing fare distribution ",caption="titanic dataset")
  
#mean fare lies at around 25 with minimum and maximum values at 0 and larger than 500


#fare density graph
ggplot(titanic_new,aes(x=Fare))+
  geom_density()+
  labs(title="Density plot showing fare distribution ",caption="titanic dataset")
#most passengers pay a fare of less than 50

#fare boxplot separated with regards to gender
ggplot(titanic_new,aes(x=Sex,y=Fare))+
  geom_boxplot()+
  labs(title="Box plot showing fare distribution\n with regards to gender",caption="titanic dataset",x="Gender")

#the range of quartiles is larger for females with a 3rd quartile of above 50 and 1st of greater than zero,males have 3rd quartile of around 25 and 1st quartile of 0

#fare boxplot with regards to passenger class
ggplot(titanic_new,aes(x=Pclass,y=Fare))+
  geom_boxplot()+
  labs(title="Box plot showing fare distribution\n with regards to passenger class ",caption="titanic dataset")
#1st class has the highest mean followed by the 2nd then 3rd class with means of less than 100
#the outlier with a fare value of greater than 500 is in the first class

#fare boxplot with regards to point of embarkation
ggplot(titanic_new,aes(x=Embarked,y=Fare))+
  geom_boxplot()+
  labs(title="Box plot showing fare distribution\n with regards to embarked ",caption="titanic dataset",x="Passenger Class")
#in class S the highest fare is slightly above 250,class Q has the highest fare at slightly below 100 while class C has highest fare at slightly higher than 500
#class C has the highest range of quartiles followed by S then Q

#fare boxplot with regards to point of embarkation,each point of embarkation grouped according to passenger class
ggplot(titanic_new,aes(x=Embarked,y=Fare,fill=factor(Pclass)))+
  geom_boxplot()+
  labs(title = "Fare Boxplot with regards to Point of Embarkation ",
       subtitle ="grouped into passenger class",caption = "titanic dataset" )
#here we can see a boxplot of fare grouped according to point of embarkation and each point of embarkation further grouped into the three passenger classes
#the outlier with fare of more than 500 can be observed to have embarked from C and was in passenger class 1

#scatterplot of fare and age separated by gender,plots coded for survival
ggplot(titanic_new,aes(x=Age,y=Fare,color=Survived))+
  geom_point()+
  facet_wrap(~Sex)+
  labs(title = "A Scatterplot Showing the Relationship Between \n Fare and Age Grouped According to Gender",
       subtitle = "color coded for survival",caption="titanic dataset")
#we can see that females survive more than males do and majority of the passengers pay fare below 100

#INFERENTIAL----
glimpse(titanic_new)
str(titanic_new)

# Creating a decision tree ------
#randomizing the dataset
rand <- sample(1:nrow(titanic_new))

titanic_random <- titanic_new[rand,]

#we split the dataset into 70% train and 30% test
split <- c(1:nrow(titanic_new)*0.7)

train <- titanic_random[split,]

test <- titanic_random[-split,]


# Build a decision tree model using the train data set
rpart(Survived~Pclass+Sex+Embarked, 
      data = train, method = "class") 

dtree_train <- rpart(Survived~Pclass+Sex+Embarked,
                        data = train, method = "class") 
# Visualization
rpart.plot(dtree_train)

# Build a decision tree model using the test data set
rpart(Survived~Pclass+Sex+Embarked, 
      data = test, method = "class") 

dtree_test <- rpart(Survived~Pclass+Sex+Embarked, 
                            data = test, method = "class") 

# Visualization
rpart.plot(dtree_test)

# Predicting the model for the train data set----
predicted_train <- predict(dtree_train, data = train, method = "class")
final_train <- data.frame(PassengerId=train$PassengerId,Survived=predicted_train)
mean(final_train$Survived.1)

# Predicting the model for the test data set----
predicted_test <- predict(dtree_test, data = test, method = "class")
final_test <- data.frame(PassengerId=test$PassengerId,Survived=predicted_test)
mean(final_test$Survived.1)

#END----






