therow<-matrix(ncol=2,nrow=0)
cost_cfs(target ~., data=traindata,costs=costs,lamda=thelamdas[1])
row_0<-therow
therow<-matrix(ncol=2,nrow=0)
cost_cfs(target ~., data=traindata,costs=costs,lamda=thelamdas[2])
row_1<-therow
therow<-matrix(ncol=2,nrow=0)
cost_cfs(target ~., data=traindata,costs=costs,lamda=thelamdas[3])
row_2<-therow
row_0<-data.frame(row_0)
row_1<-data.frame(row_1)
row_2<-data.frame(row_2)
row_0$X0<-as.numeric(row_0$X0)
row_1$X0<-as.numeric(row_1$X0)
row_2$X0<-as.numeric(row_2$X0)
row_0$number<-1:nrow(row_0)
row_1$number<-1:nrow(row_1)
row_2$number<-1:nrow(row_2)
row_0<-row_0[,2:3]
row_0$group=3
row_1<-row_1[,2:3]
row_1$group=2
row_2<-row_2[,2:3]
row_2$group=1
all<-rbind(row_2,row_1,row_0)
all$number<-as.numeric(all$number)
all$X0<-as.numeric(all$X0)
all$group<-as.factor(all$group)
ggplot(all, aes(x=number, y=X0,color=group)) + geom_line(alpha=0.4,size=0.5)+theme_bw()+
  scale_color_discrete(breaks=c(3,2,1),name = expression(lambda), labels = thelamdas[1:3])+
  geom_hline(yintercept=max(row_0$X0),linetype = 2,color="#619CFF",size=0.5)+
  geom_hline(yintercept=max(row_1$X0),linetype = 2,color="#00BA38",size=0.5)+
  geom_hline(yintercept=max(row_2$X0),linetype = 2,color="#F8766D",size=0.5)+


#  geom_hline(yintercept=max(row_1$X0[46:90]),linetype = 3,color="#00BA38")+
  
  ylab("Merit")+
  #geom_vline(xintercept=which.max(row_0$X0[1:45]),linetype = 1,alpha=0.6,color="#00BA38",)+
  
  #geom_vline(xintercept=which.max(row_1$X0[1:45]),linetype = 2,alpha=0.6,color="#619CFF",size=0.5)+
  #geom_vline(xintercept=which.max(row_2$X0[1:45]),linetype = 2,alpha=0.6,color="#F8766D",size=0.5)+

 scale_x_continuous("Generations evaluated through best first search", breaks=cumsum(45:39), labels=c(0,1,2,3,4,5,6), c(0,300))

