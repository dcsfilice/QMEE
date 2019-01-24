library(tidyverse)
dat <- read_csv("remating.csv")
figure <- read_csv("rematingmeans.csv")
#Split tables by treatment
rival <- filter(dat,treatment=="rival")
single <- filter(dat,treatment=="single")
#Change column name to order boxplots correctly
single2 <- single %>%
mutate(first.d = first.d) %>%
mutate(zfirst.d = first.d)

#Creates boxplot for each treatment, and reaction norm plot for each genetic line
ggplot(figure,aes(x= treatment,y= mean.duration))+ 
#Adds border
theme_bw()+theme(panel.border = element_rect(linetype = "solid", colour = "black",size=1))+ 
#Changes text style on plot
theme(text=element_text(family="Times",size=12))+
#Removes unnecessary category names
theme(panel.background = element_rect(fill = "white"),axis.text.x = element_blank())+
#Creates interaction plot
geom_line(aes(group= line))+
geom_point(aes(group= line))+
xlab("Rival                        Single\n Experience phase male type")+
ylab("Duration of first mating (s)")+ 
#Creates boxplots
geom_boxplot(data= single2, aes(x='zfirst.d', y= zfirst.d))+
geom_boxplot(data= rival, aes(x='first.d', y= first.d))

#Creates scatter plot of the genetic correlation between duration of first mating and latency of second
ggplot(figure, aes(x=mean.latency, y=mean.duration, color=treatment))+
#Makes points open circles
geom_point(shape=1) +
#Makes colour scheme darker
scale_colour_hue(l=60) + 
#Adds regression line, removes confidence region
geom_smooth(method=lm,se=FALSE)+
theme(text=element_text(family="Times",size=12))+
theme_bw()+theme(panel.border = element_rect(linetype = "solid", colour = "black",size=1))+
xlab("Latency until second mating (s)")+
ylab("Duration of first mating (s)")

#Binomial blot looking at probability of remating depending on first mating duration
ggplot(dat,aes(x=first.d,y=mating,colour=treatment))+
theme_bw()+theme(panel.border = element_rect(linetype = "solid", colour = "black",size=1))+
scale_colour_hue(l=60) + 
theme(text=element_text(family="Times",size=12))+
xlab("Duration of first mating (s)")+
ylab("Remating")+
stat_sum(alpha=0.25)+
geom_smooth(method="glm",se=FALSE,
              method.args=list(family="binomial"),
              formula=y~poly(x,2),alpha=0.1)
