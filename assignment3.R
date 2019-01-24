library(tidyverse)
dat<-read_csv("lifespan.csv")
figure<-read_csv("means.csv")
#Split tables by treatment
rival<-filter(dat,treatment=="rival")
single<-filter(dat,treatment=="single")
#Change column name to order boxplots correctly
single2<-single %>%
mutate(lifespan = lifespan) %>%
mutate(zlifespan = lifespan)
#Creates boxplot for each treatment, and reaction norm plot for each genetic line
ggplot(figure,aes(x= treatment,y= mean))+ 
theme_bw()+theme(panel.border = element_rect(linetype = "solid", colour = "black",size=1))+ 
theme(text=element_text(family="Times",size=11))+
theme(panel.background = element_rect(fill = "white"),axis.text.x = element_blank())+
geom_line(aes(group= line))+
geom_point(aes(group= line))+
xlab("Rival                        Single\n Experience phase male type")+
ylab("Female lifespan")+ 
geom_boxplot(data= single2, aes(x='zlifespan', y= zlifespan))+
geom_boxplot(data= rival, aes(x='lifespan', y= lifespan))

#Creates survival plot