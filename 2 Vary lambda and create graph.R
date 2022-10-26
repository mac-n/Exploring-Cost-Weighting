lamdalist=c(0,seq(0.001,0.1,by=0.002),0.2,0.3,0.4,0.5,1,2,3,4,5,6,7,8,9,10,15,20)
groups=list()
for (i in 1:length(lamdalist)){
  set.seed(i)
  lamda=as.numeric(lamdalist[i])
  
  groups[[i]]<-cost_cfs_total(target ~., data=traindata,costs=costs,lamda=lamda)
  
}
thewhich<-!duplicated(groups)
thesets<-groups[!duplicated(groups)]
thelamdas<-lamdalist[thewhich]
allgroups<-groups
groups<-groups[!duplicated(groups)]
v<-which(!duplicated(groups))
sums=vector(length=length(v))
testsummary=matrix(nrow=length(v),ncol=5)
fulltraindata<-cbind(targetsshort[trainIndex],traindata)
testdata<-featuresshort[testIndex,]
testoutcome<-targetsshort[testIndex]
colnames(fulltraindata)[1]<-"target"
fulltraindata$target<-as.factor(fulltraindata$target)

forestmodels<-list()
themeans<-vector()
for (i in 1:length(groups)){
  themeans[i]<-mean(costs[groups[[i]]])
  sums[i]<-sum(costs[groups[[i]]])
  set.seed(i)
  f<-as.simple.formula(groups[[i]],"target")
  forestmodels[[i]]<-randomForest(f,data=fulltraindata)
}
aucs<-vector(length=length(v))
for (i in 1:length(groups)){
  aucs[i]<-(auc(response=as.ordered(testoutcome),predictor=as.ordered(predict(forestmodels[[i]],testdata))))
print(aucs[i])
}

require(ggplot2)
d<-data.frame(sums,aucs)

colnames(d)<-c("sums","errates")
d$lamdavalues<-thelamdas
q<-ggplot(d, aes(x=sums,y=errates,colour=lamdavalues))+geom_path(size=1) +xlab("Total Costs") +ylab("Model AUC")+theme_bw()
q+scale_colour_gradient(low = "blue", high = "red", 
                         ) +
  labs(colour = expression(lambda))+
geom_text(data=d,aes(x=sums,y=errates,label=lamdavalues,vjust=1))

table1<- cbind(d,data.frame(sapply(groups,paste,collapse=",")))

table1<-table1[,c(3,1,2,4)]
colnames(table1)[4]<-"feat"
table1$feat <-gsub("X","",as.character(table1$feat))
table1$mean<-themeans
write.csv(table1,"table1_total.csv")

