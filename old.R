setwd("E:\\workspace\\statistic\\adfailed")
library(data.table)
library(dplyr)
library(ggplot2)

data<-fread("E:\\workspace\\statistic\\adfailed\\data\\2017071921",sep="\t",col.names=c("impId","adpId","dnfNum","dnf70","dnf75","indexNum","index70","index75","null"))
head(data)
#data<-read.csv("/home/timruning/workspace/adfailed/2017072008",sep="\t",col.names=c("impId","adpId","dnfNum","dnf70","dnf75","indexNum","index70","index75","null"))
#head(data)
data<-subset(data,select = -null)
data %>% mutate(adpId = as.character(adpId))
data$adpId <- as.factor(as.character(data$adpId)) 
print(class(data$adpId))
adpSet<-c("12355","13016","12232","12237")
adpData<-filter(data,adpId %in% adpSet)
head(adpData)


tmp <- adpData %>%
  group_by(adpId) %>%
  summarise(count=n()) %>%
  mutate(adpId = as.factor(adpId)) %>%
  mutate(proportion = round(count/sum(count),3)) 
head(tmp)
tmp$proportion <- paste(100*tmp$proportion,"%",sep = "")
tmp %>% ggplot(aes(x=adpId,y=count))+ 
  geom_bar(stat= "identity",fill="red",width = 0.5)+
  geom_text(aes(x=adpId,y=count+5000,label=proportion),col="blue",position = position_dodge(1))+
  labs(y="count",x="adpId",title = "adpidNum20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))

adpData$dnfNum_1 <- 20*(adpData$dnfNum%/%20+1)


ggplot(adpData,aes(x=dnfNum_1,colour=adpId))+
  geom_histogram(stat = "count")
ggplot(adpData,aes(x=dnfNum_1))+
  geom_histogram(aes(stat = "count",fill=adpId))





tmpDnf <- adpData %>%
  group_by(dnfNum_1) %>%
  summarise(count=n()) %>%
  #  mutate(dnfNum_1 = as.factor(dnfNum_1)) %>%
  mutate(dnfProportion = round(count/sum(count),4))

tmpDnf$dnfProportion <- paste(100* tmpDnf$dnfProportion,"%",sep = "")

qplot(data = adpData,dnfNum_1,geom = c("histogram"),fill=adpId,binwidth=10)

head(tmpDnf)

class(adpData$dnfNum_1)

class(tmpDnf$dnfNum_1)
ggplot(adpData) +
  geom_histogram(aes(x=dnfNum_1,fill = adpId),bins = 30)+
  geom_text(aes(x=tmpDnf$dnfNum_1,y=count+1000,label = dnfProportion),data = tmpDnf,col = "blue")
tmpDnf %>% ggplot(aes(x=dnfNum_1,y=count)) +
  geom_bar(stat = "identity",fill = "red") +
  geom_text(aes(x=dnfNum_1,y=count+1000,label=dnfProportion),col="blue",check_overlap = TRUE,position = position_dodge(3))+
  labs(y="count",x="dnfNum",title="dnf_dis20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))

adpData$dnf70_1 <- 20*(adpData$dnf70%/%20+1)

tmpDnf70 <- adpData %>%
  group_by(dnf70_1) %>%
  summarise(count=n()) %>%
  mutate(dnf70_1 = as.factor(dnf70_1)) %>%
  mutate(dnf70Proportion = round(count/sum(count),4))

tmpDnf70$dnf70Proportion <- paste(100* tmpDnf70$dnf70Proportion,"%",sep = "")

qplot(data = adpData,dnf70_1,geom = c("histogram"),fill=adpId,binwidth=10)

head(tmpDnf70)

tmpDnf70 %>% ggplot(aes(x=dnf70_1,y=count)) +
  geom_bar(stat = "identity",fill = "red") +
  geom_text(aes(x=dnf70_1,y=count+1000,label=dnf70Proportion),col="blue",check_overlap = TRUE,position = position_dodge(3))+
  labs(y="count",x="dnf70",title="dnf70_dis20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))

adpData$dnf75_1 <- 20*(adpData$dnf75%/%20+1)

tmpDnf75 <- adpData %>%
  group_by(dnf75_1) %>%
  summarise(count=n()) %>%
  mutate(dnf75_1 = as.factor(dnf75_1)) %>%
  mutate(dnf75Proportion = round(count/sum(count),4))

tmpDnf75$dnf75Proportion <- paste(100* tmpDnf75$dnf75Proportion,"%",sep = "")
qplot(data = adpData,dnf75_1,geom = c("histogram"),fill=adpId)
head(tmpDnf75)

tmpDnf75 %>% ggplot(aes(x=dnf75_1,y=count)) +
  geom_bar(stat = "identity",fill = "red",width = 0.2) +
  geom_text(aes(x=dnf75_1,y=count,label=dnf75Proportion),col="blue",check_overlap = TRUE,position = position_dodge(3))+
  labs(y="count",x="dnf75",title="dnf75_dis20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))


adpData$indexNum_1 <- 20*(adpData$indexNum%/%20+1)
tmpIndex <- adpData %>%
  group_by(indexNum_1) %>%
  summarise(count=n()) %>%
  mutate(indexNum_1 = as.factor(indexNum_1)) %>%
  mutate(indexProportion = round(count/sum(count),4))

tmpIndex$indexProportion <- paste(100* tmpIndex$indexProportion,"%",sep = "")

qplot(data = adpData,indexNum_1,geom = c("histogram"),fill=adpId,binwidth=10)
head(tmpIndex)

tmpIndex %>% ggplot(aes(x=indexNum_1,y=count)) +
  geom_bar(stat = "identity",fill = "red") +
  geom_text(aes(x=indexNum_1,y=count+5000,label=indexProportion),col="blue",check_overlap = TRUE,position = position_dodge(3))+
  labs(y="count",x="indexNum",title="index_dis20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))

adpData$index70_1 <- 20*(adpData$index70%/%20+1)

tmpIndex70 <- adpData %>%
  group_by(index70_1) %>%
  summarise(count=n()) %>%
  mutate(index70_1 = as.factor(index70_1)) %>%
  mutate(index70Proportion = round(count/sum(count),4))

tmpIndex70$index70Proportion <- paste(100* tmpIndex70$index70Proportion,"%",sep = "")
qplot(data = adpData,index70_1,geom = c("histogram"),fill=adpId,binwidth=10)
head(tmpIndex70)



tmpIndex70 %>% ggplot(aes(x=index70_1,y=count)) +
  geom_bar(stat = "identity",fill = "red") +
  geom_text(aes(x=index70_1,y=count+5000,label=index70Proportion),col="blue",check_overlap = TRUE,position = position_dodge(3))+
  labs(y="count",x="index70",title="index70_dis20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))

adpData$index75_1 <- 20*(adpData$index75%/%20+1)

tmpIndex75 <- adpData %>%
  group_by(index75_1) %>%
  summarise(count=n()) %>%
  mutate(index75_1 = as.factor(index75_1)) %>%
  mutate(index75Proportion = round(count/sum(count),4))

tmpIndex75$index75Proportion <- paste(100* tmpIndex75$index75Proportion,"%",sep = "")
qplot(data = adpData,index75_1,geom = c("histogram"),fill=adpId,binwidth=0.1)
head(tmpIndex75)

tmpIndex75 %>% ggplot(aes(x=index75_1,y=count)) +
  geom_bar(stat = "identity",fill = "red",width = 0.2) +
  geom_text(aes(x=index75_1,y=count,label=index75Proportion),col="blue",check_overlap = TRUE,position = position_dodge(3))+
  labs(y="count",x="index75",title="index75_dis20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))


adpData$misDnfIndex <- adpData$dnfNum-adpData$indexNum
adpData$misDnfIndex_1 <- 20*(adpData$misDnfIndex%/%20+1)


qplot(data = adpData,misDnfIndex_1,geom = "histogram",fill = adpId,binwidth=10)

tmpMisAd <- adpData %>%
  group_by(misDnfIndex_1) %>%
  summarise(count=n()) %>%
  #  mutate(misDnfIndex_1 = as.factor(misDnfIndex_1)) %>%
  mutate(misAdPorportion = round(count/sum(count),4))
tmpMisAd$misAdPorportion <- paste(100* tmpMisAd$misAdPorportion,"%",sep = "")
head(tmpMisAd)

adpData %>% ggplot(aes(x=misDnfIndex_1)) +
  geom_histogram(aes(fill=adpId)) +
  geom_text(data = tmpMisAd,aes(x=misDnfIndex_1,y=count+5000,label=misAdPorportion),col="blue",check_overlap = TRUE,position = position_dodge(3))

tmpMisAd %>% ggplot(aes(x=misDnfIndex_1,y=count)) +
  geom_bar(stat = "identity",fill = "red") +
  geom_text(aes(x=misDnfIndex_1,y=count,label=misAdPorportion),col="blue",check_overlap = TRUE,position = position_dodge(3))+
  labs(y="count",x="misAd",title="misAd_dis20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))

adpData$mis70 <- adpData$dnf70-adpData$index70

adpData$mis70_1 <-20 * (adpData$mis70%/%20+1)

head((adpData))
qplot(data=adpData,mis70_1,geom="histogram",fill=adpId)

tmpMis70 <- adpData %>%
  group_by(mis70_1) %>%
  summarise(count=n()) %>%
  mutate(mis70_1 = as.factor(mis70_1)) %>%
  mutate(mis70Proportion = round(count/sum(count),4)) %>%
  mutate(mis70Proportion = paste(mis70Proportion*100,"%",sep=""))
head(tmpMis70)

tmpMis70 %>% ggplot(aes(x=mis70_1,y=count)) +
  geom_bar(stat = "identity",fill="red") +
  geom_text(aes(x=mis70_1,y=count+5000,label=mis70Proportion),col="blue",check_overlap = TRUE,position = position_dodge(3))+
  labs(y="count",x="mis70",title="mis70_dis20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))

adpData$mis75 <- adpData$dnf75-adpData$index75

adpData$mis75_1 <-20 * (adpData$mis75%/%20+1)

head((adpData))
qplot(data=adpData,mis75_1,geom="histogram",fill=adpId)

tmpMis75 <- adpData %>%
  group_by(mis75_1) %>%
  summarise(count=n()) %>%
  mutate(mis75_1 = as.factor(mis75_1)) %>%
  mutate(mis75Proportion = round(count/sum(count),4)) %>%
  mutate(mis75Proportion = paste(mis75Proportion*100,"%",sep=""))
head(tmpMis75)

tmpMis75 %>% ggplot(aes(x=mis75_1,y=count)) +
  geom_bar(stat = "identity",fill="red") +
  geom_text(aes(x=mis75_1,y=count+5000,label=mis75Proportion),col="blue",check_overlap = TRUE,position = position_dodge(3))+
  labs(y="count",x="mis75",title="mis75_dis20170720_03")+
  theme(plot.title = element_text(hjust = 0.5))



