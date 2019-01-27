library(tidyverse)
data<-read_csv("lifespan.csv")
head(data)

## JD: Changed directory name, sorry. Space was making me crazy

## JD: Whitespace can aid legibilty

#Examine structure of data to check each variable has a column 
str(data)
#There are five variables: no variables are missing
#Check that data is complete using summary
summary(data)
#There are 120 samples: dataset is complete
#Check for mistakes in data by converting variables into factors and using levels function

## JD: Come up with an indent style
## This time I'm going to use mine; you don't need to like mine but you need to have one
dataf <- (data
	%>% mutate(linef = as.factor(line))
	%>% mutate(treatmentf = as.factor(treatment))
)

levels(dataf$linef)
levels(dataf$treatmentf)
#There are 6 unique lines and 2 unique treatments: no mistakes found here
#Let's make sure each treatment by line combination has 10 replicates
print(data %>% 
group_by(line, treatment) %>%
summarize(count = n()))
#Looks good to me!
print(ggplot(data, aes(x=lifespan))+ geom_histogram())+geom_histogram(binwidth = 1)
## JD: Not so sure why we get two almost-identical plots here, but I guess you don't want to say geom_histogram twice.

#Histogram of entire data set. Overall data has a (relatively) normal distribtion.
#Next, let's filter all the samples that lived longer than 21 days
#See README for rationale
data2<-filter(data,lifespan>21)

#Lastly, let's break up the data by genetic line in case we want to independently analyze each one
## JD: Probably not a good idea to hard code the line names, and you may not need the broken data at all with all of our modern tools
## If I was convinced I needed to do this, I would make code to make a list of tibbles and _automatically_ assign the line name as the list-element name
geno1<-filter(data,line=="M239")
geno2<-filter(data,line=="M315")
geno3<-filter(data,line=="M395")
geno4<-filter(data,line=="M596")
geno5<-filter(data,line=="M703")
geno6<-filter(data,line=="M900")

## JD: score 2. (1=poor, 2=fine, 3=excellent)
## Nice, clear checking
