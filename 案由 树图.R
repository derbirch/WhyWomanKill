##############
#treemap
library(echarts4r)
View(data)
data <- data.frame(cause=mydata$案由)
data$cause <- as.character(data$cause)

data[is.na(data)] <- '其他'
data$cause <- as.factor(data$cause)

data<-table(data)
data<-as.data.frame(data)
data$data <- as.character(data$data)
data$data[data$data==''] = '其他'

setwd('~/homework/WhyWomanKill')
write.csv(data,'~/homework/WhyWomanKill/DATA/案由树图.csv')
df2<-read.csv(file = '~/homework/WhyWomanKill/DATA/案由树图.csv',header =T,sep=',',encoding = 'UTF-8')
#View(df2)


df2 %>% 
  e_charts() %>% 
  e_treemap(data, data, Freq) %>% 
  e_title("案由","树图") %>%
  e_tooltip()