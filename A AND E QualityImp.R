method <-data.frame(quiz=c(rep("1a",3),rep("1b",3),rep("1c",3)) ,opnion=rep(c("high","moderate","low"),3),
                    frequency = c(27,5,10,26,13,7,10,4,14))
method
library(ggplot2)
ggplot(data=method,aes(x=factor(quiz),y=prop.table(frequency),fill=factor(opnion)))+
  geom_bar(stat = "identity",position = "dodge")+
  scale_y_continuous(labels=scales::percent)+
  geom_text(label = scales::percent(prop.table(method$frequency)),vjust=-.2,position = position_dodge(.9))+
  theme_classic()+
  scale_fill_viridis_d(name=c("Opinion scale"))+
  labs(x="Method",y="Percentage",title = "Cause of long TAT due to method")
  
# Place/Environment
place <-data.frame(quiz=c(rep("2a",3),rep("2b",3),rep("2c",3),rep("2d",3)) ,opnion=rep(c("high","moderate","low"),4),
                    frequency = c(27,9,10,26,9,12,31,7,7,17,6,7))

ggplot(data=place,aes(x=factor(quiz),y=prop.table(frequency),fill=factor(opnion)))+
  geom_bar(stat = "identity",position = "dodge")+
  scale_y_continuous(labels=scales::percent)+
  geom_text(label = scales::percent(prop.table(place$frequency)),vjust=-.2,position = position_dodge(.9))+
  theme_classic()+
  scale_fill_viridis_d(name=c("Opinion scale"),option = "B",begin = .1,end = .5)+
  labs(x="Method",y="Percentage",title = "Cause of long TAT due to Place/Environment ")


# 3. Material / Equipment
material <-data.frame(quiz=c(rep("3a",3),rep("3b",3),rep("3c",3),rep("3d",3)) ,opnion=rep(c("high","moderate","low"),4),
                      frequency = c(30,12,6,33,7,6,28,10,7,30,5,9))

ggplot(data=material,aes(x=factor(quiz),y=prop.table(frequency),fill=factor(opnion)))+
  geom_bar(stat = "identity",position = "dodge")+
  scale_y_continuous(labels=scales::percent)+
  geom_text(label = scales::percent(prop.table(material$frequency)),vjust=-.2,position = position_dodge(.9))+
  theme_classic()+
  scale_fill_viridis_d(name=c("Opinion scale"),option = "D",begin = 0.2 ,end = .7)+
  labs(x="Method",y="Percentage",title = "Cause of long TAT due to Material / Equipment ")


# Patients/ clients
patients<-data.frame(quiz=c(rep("4a",3),rep("4b",3),rep("4c",3)) ,opnion=rep(c("high","moderate","low"),3),
                     frequency = c(34,6,7,38,4,6,30,9,6))

ggplot(data=patients,aes(x=factor(quiz),y=prop.table(frequency),fill=factor(opnion)))+
  geom_bar(stat = "identity",position = "dodge")+
  scale_y_continuous(labels=scales::percent)+
  geom_text(label = scales::percent(prop.table(patients$frequency)),vjust=-.2,position = position_dodge(.9))+
  theme_classic()+
  scale_fill_viridis_d(name=c("Opinion scale"),option = "C", begin = 0.1 ,end = .6)+
  labs(x="Method",y="Percentage",title = "Cause of long TAT due to Patients/ clients ")

# 5 Provider
provider<-data.frame(quiz=c(rep("5a",3),rep("5b",3),rep("5c",3)) ,opnion=rep(c("high","moderate","low"),3),
                     frequency = c(15,8,22,41,4,1,18,12,17))

ggplot(data=provider,aes(x=factor(quiz),y=prop.table(frequency),fill=factor(opnion)))+
  geom_bar(stat = "identity",position = "dodge")+
  scale_y_continuous(labels=scales::percent)+
  geom_text(label = scales::percent(prop.table(provider$frequency)),vjust=-.2,position = position_dodge(.9))+
  theme_classic()+
  scale_fill_viridis_d(name=c("Opinion scale"),option = 'D' , begin = 0.6 ,end = .9)+
  labs(x="Method",y="Percentage",title = "Cause of long TAT due to Provider")
