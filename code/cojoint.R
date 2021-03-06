library(ggplot2)
getwd()
mydata <- read_excel("../data/survey.xlsx",sheet=1,col_names = T,col_types = NULL)
Maxim <- factor(mydata$Maxim)
summary(Maxim)
SU <- factor(mydata$SU)
summary(SU)
BIJAS <- factor(mydata$BIJAS)
summary(BIJAS)
Delifrance <- factor(mydata$Delifrance)
summary(Delifrance)
Swire <- factor(mydata$Swire)
summary(Swire)
temp <- cbind(summary(Maxim),summary(SU),summary(BIJAS),summary(Delifrance),summary(Swire))
write.csv(temp,"analysis.csv")

analysis0<-read.csv("factor_code.csv")
summary(analysis0)

Frequency<-factor(mydata$Frequency)
summary(Frequency)
temp<-table(Frequency,mydata$Maxim)
write.csv(temp,"a1.csv")

temp<-table(Frequency,mydata$SU)
write.csv(temp,"a2.csv")

temp<-table(Frequency,mydata$BIJAS)
write.csv(temp,"a3.csv")

temp<-table(Frequency,mydata$Delifrance)
write.csv(temp,"a4.csv")

temp<-table(Frequency,mydata$Swire)
write.csv(temp,"a5.csv")


mydata2 <- read_excel("../data/survey.xlsx",sheet=3,col_names = T,col_types = NULL)
source("conjanal.R")
sneaker<-mydata2


#define factors
sneaker$COL1<-factor(sneaker$COL1,labels=c("acceptable","high"))
sneaker$COL2<-factor(sneaker$COL2,labels=c("25-35HKD","35-45HKD"))
sneaker$COL3<-factor(sneaker$COL3,labels=c("0-10min","10-20min"))

#set contrasts, sum=0
contrasts(sneaker$COL1)<-contr.sum(2)
contrasts(sneaker$COL2)<-contr.sum(2)
contrasts(sneaker$COL3)<-contr.sum(2)

#define response matrix
resp<-as.matrix(7-sneaker[,4:47])

#define profiles
profile<-sneaker[,1:3]

fit.conj<-conjanal(resp,profile)

#display the first respondent
fit.conj$part[,1]
temp<-fit.conj$part[,1]
write.csv(temp,"a6.csv")

fit.conj$imp[,1]

# standardize relative importance
imp<-data.frame(scale(t(fit.conj$imp)))
#Ward's method
dist<-dist(imp,method="euclidean")^2
clust <- hclust(dist, method="ward.D")

ggplot(mapping=aes(x=1:length(clust$height),y=clust$height))+
  geom_line()+
  geom_point()+
  labs(x="stage",y="height")

par(mar=c(1,4,1,1))
plot(clust,labels=F,hang=-1,sub="",xlab="",main="")

#K-means method
set.seed(12345)
fit1<-kmeans(x=imp,centers=3,algorithm="MacQueen")
fit1
tb<-fit1$centers

tb<-data.frame(cbind(tb,cluster=1:3))
tbm<-melt(tb,id.vars='cluster')
tbm$cluster<-factor(tbm$cluster)
ggplot(tbm,
       aes(x = variable, y = value, group = cluster, colour = cluster)) +
  geom_line(aes(linetype=cluster))+
  geom_point(aes(shape=cluster)) +
  geom_hline(yintercept=0) +
  labs(x=NULL,y="mean")
