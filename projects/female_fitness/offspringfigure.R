data<-read.csv("offspringfig.csv")
library(ggplot2)
attach(data)

#Original figure for mean offspring production by female age
ggplot(data,aes(x= as.factor(day),y= mean,group=treatment))+
  theme_bw()+theme(panel.border = element_rect(linetype = "solid", colour = "black",size=1))+ 
  theme(text=element_text(family="Times",size=20))+geom_line(aes(linetype= treatment),colour=colour)+
  geom_point(aes(group= treatment))+
  scale_linetype_manual(values=c("solid","dashed","solid","dashed"))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.15)+xlab("Female age (days)")+
  ylab("Mean offspring production")


#SMR working on fixing the legend.  GETTING CLOSER! 
ggplot(data,aes(x= as.factor(day),y= mean,group=treatment))+
  theme_bw()+theme(panel.border = element_rect(linetype = "solid", colour = "black",size=1))+ 
  theme(text=element_text(family="Times",size=15))+geom_line(aes(linetype= treatment),colour=colour)+
  geom_point(aes(group= treatment))+
  scale_linetype_manual(values=c("solid","dashed","solid","dashed"))+
  geom_errorbar(aes(ymin=mean-sd, ymax=mean+sd), width=.15)+xlab("Female age (days)")+
  ylab("Mean offspring production") +
  theme(
  legend.position = c(0.97, 0.97),
  legend.justification = c("right", "top"),
  legend.box.background = element_rect(colour = "black",size=1),
  legend.text = element_text(size = 8),
  legend.box.margin = margin(4, 4, 4, 4),)
  
  
## Possible ways to work on changing the legend box to have
  # male experience
    # single "grey line"
    # rival  "black line"
  # female population
    # wild  "solid line"
    # lab  "dashed line"


#legend.title = title(colour="male experience", linetype="female population"),

#  guides(fill = colour = "male experience"))

#  legend.text = element_text(colour = "male experience", linetype = "female population"),

#  scale_fill_discrete(name = "Title", labels = c("a", "b") colour = "male experience"),

# label(linetype = "female population")

## theme(legend.title = element_blank()
#labs(fill = "Dose (mg)")
