data<-read.csv("offspringfig.csv")
library(ggplot2)
attach(data)
ggplot(data,aes(x= as.factor(day),y= mean,group=treatment))+
  theme_bw()+theme(panel.border = element_rect(linetype = "solid", colour = "black",size=1))+ 
  theme(text=element_text(family="Times",size=20))+geom_line(aes(linetype= treatment),colour=colour)+
  geom_point(aes(group= treatment))+
  scale_linetype_manual(values=c("solid","dashed","solid","dashed"))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.15)+xlab("Female age (days)")+
  ylab("Mean offspring produciton")
