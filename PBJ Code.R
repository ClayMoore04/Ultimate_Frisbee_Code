library(plyr)

data<-read.csv("/Users/cm693h/Documents/Personal\ Things/TexMix/TexMix-PBJ.csv")

data$Passer_Gender[ data$Passer=="Zack R" |data$Passer=="Austin" |data$Passer=="Maloney" |data$Passer=="Greg M" |data$Passer=="Clay" |data$Passer=="Santi" |data$Passer=="D-mo" |data$Passer=="Neil" |data$Passer=="Whitey" |data$Passer=="Zack M" |data$Passer=="Kidd" |data$Passer=="Jimmy" |data$Passer=="Jimmy"]<- "Male"
data$Passer_Gender[ data$Passer=="Jenna" |data$Passer=="Goose" |data$Passer=="Abbey" |data$Passer=="Anna" |data$Passer=="Kayleigh" |data$Passer=="Libby" |data$Passer=="Morgan" |data$Passer=="Becca" |data$Passer=="Mom" |data$Passer=="Marz"]<- "Female"
data$Receiver_Gender[ data$Receiver=="Zack R" |data$Receiver=="Austin" |data$Receiver=="Maloney" |data$Receiver=="Greg M" |data$Receiver=="Clay" |data$Receiver=="Santi" |data$Receiver=="D-mo" |data$Receiver=="Neil" |data$Receiver=="Whitey" |data$Receiver=="Zack M" |data$Receiver=="Kidd" |data$Receiver=="Jimmy" |data$Receiver=="Jimmy"]<- "Male"
data$Receiver_Gender[ data$Receiver=="Jenna" |data$Receiver=="Goose" |data$Receiver=="Abbey" |data$Receiver=="Anna" |data$Receiver=="Kayleigh" |data$Receiver=="Libby" |data$Receiver=="Morgan" |data$Receiver=="Becca" |data$Receiver=="Mom" |data$Receiver=="Marz"]<- "Female"
data$Gendered_Throw[data$Passer_Gender=="Male" & data$Receiver_Gender=="Male"] <- "Male To Male"
data$Gendered_Throw[data$Passer_Gender=="Female" & data$Receiver_Gender=="Male"] <- "Female To Male"
data$Gendered_Throw[data$Passer_Gender=="Male" & data$Receiver_Gender=="Female"] <- "Male To Female"
data$Gendered_Throw[data$Passer_Gender=="Female" & data$Receiver_Gender=="Female"] <- "Female To Female"
noturnovers<-subset(data,Action!="Throwaway" & Action!="Drop")
goals<-subset(data,Action=="Goal" & Defender!= "Anonymous")

ddply(noturnovers,c("Gendered_Throw"),summarize,N=length(Event.Type))
ddply(goals,c("Gendered_Throw"),summarize,N=length(Event.Type))
total<-ddply(noturnovers,c("Passer"),summarize,N=length(Passer))
breakdown<-ddply(noturnovers,c("Passer","Gendered_Throw"),summarize,N=length(Passer))
all<-merge(total,breakdown,by="Passer",all.x=T)
all<-all[c(1,3,2,4)]
all<-subset(all,Passer!="")
colnames(all)<-c("Passer","Type_of_Throw","Total_Throws","Total_Type")
all$Percentage<-round((all$Total_Type/all$Total_Throws)*100,2)

ggplot(subset(all,Type_of_Throw=="Female To Female" | Type_of_Throw=="Female To Male") ,aes(x=Passer,y=Total_Type,fill=factor(Type_of_Throw,levels = c("Female To Female","Female To Male"))))+
  geom_bar(stat="identity")+
  theme_bw()+
  xlab("\nPlayer") + 
  ylab("Total Throws\n") + 
  ggtitle("Player Throw Interaction (Female)\n")+
  theme(plot.title=element_text(size=18,hjust=0.5))+
  theme(axis.title=element_text(size=14)) +
  theme(axis.text=element_text(size=15)) + 
  scale_fill_manual(name="",values=c("#4C4CFF","#CC0000"),labels=c("Same Gender","Different Gender")) + 
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=14)) + 
  scale_y_continuous(limits = c(0,50))
  

ggplot(subset(all,Type_of_Throw=="Male To Male" | Type_of_Throw=="Male To Female") ,aes(x=Passer,y=Total_Type,fill=factor(Type_of_Throw,levels=c("Male To Male","Male To Female"))))+
  geom_bar(stat="identity") + 
  theme_bw()+
  xlab("\nPlayer") + 
  ylab("Total Throws\n") + 
  ggtitle("Player Throw Interaction (Male)\n")+
  theme(plot.title=element_text(size=18,hjust=0.5))+
  theme(axis.title=element_text(size=14)) +
  theme(axis.text=element_text(size=13)) + 
  scale_fill_manual(name="",values=c("#4C4CFF","#CC0000"),labels=c("Same Gender","Different Gender")) + 
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=14)) + 
  scale_y_continuous(limits = c(0,120))





#### HOOTIE

.libPaths("/Users/cm693h/Documents/R_Stuff")
.libPaths()

library(plyr)
library(ggplot2)
devtools::install_github("jcheng5/bubbles")
library(bubbles)

data<-read.csv("/Users/cm693h/Documents/Personal\ Things/TexMix/TexMixHootie-stats.csv")

data$Passer_Gender[ data$Passer=="JP" |data$Passer=="Neil" |data$Passer=="Maloney" |data$Passer=="Zack M" |data$Passer=="Santi" |data$Passer=="Greg M" |data$Passer=="Rader" |data$Passer=="Dmo" |data$Passer=="Goutham" |data$Passer=="Whitey" |data$Passer=="Clay" |data$Passer=="Dan"]<- "Male"
data$Passer_Gender[ data$Passer=="Goose" |data$Passer=="Morgan" |data$Passer=="Mom" |data$Passer=="Becca" |data$Passer=="Mars" |data$Passer=="Kera"]<- "Female"
data$Receiver_Gender[ data$Receiver=="JP" |data$Receiver=="Neil" |data$Receiver=="Maloney" |data$Receiver=="Zack M" |data$Receiver=="Santi" |data$Receiver=="Greg M" |data$Receiver=="Rader" |data$Receiver=="Dmo" |data$Receiver=="Goutham" |data$Receiver=="Whitey" |data$Receiver=="Clay" |data$Receiver=="Dan"]<- "Male"
data$Receiver_Gender[data$Receiver=="Goose" |data$Receiver=="Morgan" |data$Receiver=="Mom" |data$Receiver=="Becca" |data$Receiver=="Mars" |data$Receiver=="Kera" | data$Receiver=="Karly"|data$Receiver=="Libby"]<- "Female"
data$Gendered_Throw[data$Passer_Gender=="Male" & data$Receiver_Gender=="Male"] <- "Male To Male"
data$Gendered_Throw[data$Passer_Gender=="Female" & data$Receiver_Gender=="Male"] <- "Female To Male"
data$Gendered_Throw[data$Passer_Gender=="Male" & data$Receiver_Gender=="Female"] <- "Male To Female"
data$Gendered_Throw[data$Passer_Gender=="Female" & data$Receiver_Gender=="Female"] <- "Female To Female"
noturnovers<-subset(data,Action!="Throwaway" & Action!="Drop")
goals<-subset(data,Action=="Goal" & Defender!= "Anonymous")

ddply(noturnovers,c("Gendered_Throw"),summarize,N=length(Event.Type))
ddply(goals,c("Gendered_Throw"),summarize,N=length(Event.Type))
total<-ddply(noturnovers,c("Passer"),summarize,N=length(Passer))
breakdown<-ddply(noturnovers,c("Passer","Gendered_Throw"),summarize,N=length(Passer))
all<-merge(total,breakdown,by="Passer",all.x=T)
all<-all[c(1,3,2,4)]
all<-subset(all,Passer!="")
colnames(all)<-c("Passer","Type_of_Throw","Total_Throws","Total_Type")
all$Percentage<-round((all$Total_Type/all$Total_Throws)*100,2)

#Look at Turnovers
throwaway<-subset(data,Event.Type=="Offense" & Action=="Throwaway")
throwaway_rollup<-ddply(throwaway,c("Passer"),summarize, N=length(Passer))
throwaway_rollup$Passer_Gender[ throwaway_rollup$Passer=="JP" |throwaway_rollup$Passer=="Neil" |throwaway_rollup$Passer=="Maloney" |throwaway_rollup$Passer=="Zack M" |throwaway_rollup$Passer=="Santi" |throwaway_rollup$Passer=="Greg M" |throwaway_rollup$Passer=="Rader" |throwaway_rollup$Passer=="Dmo" |throwaway_rollup$Passer=="Goutham" |throwaway_rollup$Passer=="Whitey" |throwaway_rollup$Passer=="Clay" |throwaway_rollup$Passer=="Dan"]<- "#bc1d0f"
throwaway_rollup$Passer_Gender[ throwaway_rollup$Passer=="Goose" |throwaway_rollup$Passer=="Morgan" |throwaway_rollup$Passer=="Mom" |throwaway_rollup$Passer=="Becca" |throwaway_rollup$Passer=="Mars" |throwaway_rollup$Passer=="Kera"]<- "#3e49e8"
throwaway_rollup$Passer<-paste(throwaway_rollup$Passer,throwaway_rollup$N,sep="-")
bubbles(throwaway_rollup$N,label=throwaway_rollup$Passer,color=throwaway_rollup$Passer_Gender)

#Look at Drops
throwaway<-subset(data,Event.Type=="Offense" & (Action=="Drop"|Action=="Throwaway"))
throwaway_rollup<-ddply(throwaway,c("Receiver"),summarize, N=length(Receiver))
throwaway_rollup$Receiver_Gender[ throwaway_rollup$Receiver=="JP" |throwaway_rollup$Receiver=="Neil" |throwaway_rollup$Receiver=="Maloney" |throwaway_rollup$Receiver=="Zack M" |throwaway_rollup$Receiver=="Santi" |throwaway_rollup$Receiver=="Greg M" |throwaway_rollup$Receiver=="Rader" |throwaway_rollup$Receiver=="Dmo" |throwaway_rollup$Receiver=="Goutham" |throwaway_rollup$Receiver=="Whitey" |throwaway_rollup$Receiver=="Clay" |throwaway_rollup$Receiver=="Dan"]<- "#bc1d0f"
throwaway_rollup$Receiver_Gender[ throwaway_rollup$Receiver=="Goose" |throwaway_rollup$Receiver=="Morgan" |throwaway_rollup$Receiver=="Mom" |throwaway_rollup$Receiver=="Becca" |throwaway_rollup$Receiver=="Mars" |throwaway_rollup$Receiver=="Kera" | throwaway_rollup$Receiver=="Libby" | throwaway_rollup$Receiver=="Karly"]<- "#3e49e8"
throwaway_rollup<-throwaway_rollup[throwaway_rollup$Receiver!="Anonymous",]
throwaway_rollup$Receiver<-paste(throwaway_rollup$Receiver,throwaway_rollup$N,sep="-")
bubbles(throwaway_rollup$N,label=throwaway_rollup$Receiver,color=throwaway_rollup$Receiver_Gender)


ggplot(subset(all,Type_of_Throw=="Female To Female" | Type_of_Throw=="Female To Male") ,aes(x=Passer,y=Total_Type,fill=factor(Type_of_Throw,levels = c("Female To Female","Female To Male"))))+
  geom_bar(stat="identity")+
  theme_bw()+
  xlab("\nPlayer") + 
  ylab("Total Throws\n") + 
  ggtitle("Player Throw Interaction (Female)\n")+
  theme(plot.title=element_text(size=18,hjust=0.5))+
  theme(axis.title=element_text(size=14)) +
  theme(axis.text=element_text(size=15)) + 
  scale_fill_manual(name="",values=c("#4C4CFF","#CC0000"),labels=c("Same Gender","Different Gender")) + 
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=14)) + 
  scale_y_continuous(limits = c(0,50))


ggplot(subset(all,Type_of_Throw=="Male To Male" | Type_of_Throw=="Male To Female") ,aes(x=Passer,y=Total_Type,fill=factor(Type_of_Throw,levels=c("Male To Male","Male To Female"))))+
  geom_bar(stat="identity") + 
  theme_bw()+
  xlab("\nPlayer") + 
  ylab("Total Throws\n") + 
  ggtitle("Player Throw Interaction (Male)\n")+
  theme(plot.title=element_text(size=18,hjust=0.5))+
  theme(axis.title=element_text(size=14)) +
  theme(axis.text=element_text(size=13)) + 
  scale_fill_manual(name="",values=c("#4C4CFF","#CC0000"),labels=c("Same Gender","Different Gender")) + 
  theme(legend.position = "bottom")+
  theme(legend.text = element_text(size=14)) + 
  scale_y_continuous(limits = c(0,120))





ddply(noturnovers,c("Gendered_Throw"),summarize,N=length(Event.Type))
ddply(goals,c("Gendered_Throw"),summarize,N=length(Event.Type))
total<-ddply(noturnovers,c("Passer"),summarize,N=length(Passer))
breakdown<-ddply(noturnovers,c("Passer","Gendered_Throw"),summarize,N=length(Passer))
all<-merge(total,breakdown,by="Passer",all.x=T)
all<-all[c(1,3,2,4)]
all<-subset(all,Passer!="")
colnames(all)<-c("Passer","Type_of_Throw","Total_Throws","Total_Type")
all$Percentage<-round((all$Total_Type/all$Total_Throws)*100,2)