library(tidyverse)
data<-read_csv("lifespan.csv")
head(data)
#Examine structure of data to check each variable has a column 
str(data)
#There are five variables: no variables are missing
#Check that data is complete using summary
summary(data)
#There are 120 samples: dataset is complete
#Check for mistakes in data by converting variables into factors and using levels function
dataf<-data %>%
mutate(linef = as.factor(line)) %>%
mutate(treatmentf = as.factor(treatment)) 
levels(dataf$linef)
levels(dataf$treatmentf)
#There are 6 unique lines and 2 unique treatments: no mistakes found here
#Let's make sure each treatment by line combination has 10 replicates
print(data %>% 
group_by(line, treatment) %>%
summarize(count = n()))
#Looks good to me!
print(ggplot(data, aes(x=lifespan))+ geom_histogram())+geom_histogram(binwidth = 1)
#Histogram of entire data set. Overall data has a (relatively) normal distribtion.
#Next, let's filter all the samples that lived longer than 21 days
#See README for rationale
data2<-filter(data,lifespan>21)
#Lastly, let's break up the data by genetic line in case we want to independently analyze each one
geno1<-filter(data,line=="M239")
geno2<-filter(data,line=="M315")
geno3<-filter(data,line=="M395")
geno4<-filter(data,line=="M596")
geno5<-filter(data,line=="M703")
geno6<-filter(data,line=="M900")