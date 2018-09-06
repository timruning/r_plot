setwd("E:\\workspace\\statistic\\adfailed")
library(data.table)
library(dplyr)
library(ggplot2)
library(discretization)
#
data<-read.csv("E:\\workspace\\statistic\\adfailed\\data\\adfailed"
               ,sep="\t"
               ,col.names=c("mk","failed_num","prior"
                            ,"adpid","dnf_num","dnf70_num","dnf75_num"
                            ,"index_num","index70_num","index75_num"
                            ,"av","click","ctr","ecpm"))
head(data)

adpSet<-c("12355","13016","12232","12237")
data <- data %>% filter(adpid %in% adpSet)

dataTmp <- data[,c("mk","failed_num","prior","av","click","ctr","ecpm")]
nrow(dataTmp)
dataTmp<- unique(dataTmp)

dataTmp[dataTmp=="null"] <- NA

dataTmp$av <- as.numeric(dataTmp$av)
nrow(dataTmp)
dataTmp[is.na(dataTmp)] <- 0
summary(dataTmp)
dataTmp$failed_num_dis <- 2000 * (dataTmp$failed_num %/% 2000+1)

dataTmp$failed_num_dis[dataTmp$failed_num_dis > 50000] <-50000



head(dataTmp)
write.csv(dataTmp,file = "adInfo.csv")

tmp <- dataTmp %>%
  group_by(failed_num_dis) %>%
  summarise(count=n()) %>%
  mutate(per = count/sum(count)) %>%
  mutate(per = paste(100*round(per,4),"%",sep=""))
head(tmp)
dataTmp %>%ggplot(aes(x=failed_num_dis))+
  geom_histogram(bins=90) +
  geom_text(data=tmp,aes(x=failed_num_dis,y=count+100,label=per),check_overlap = TRUE,position = position_dodge(3))

