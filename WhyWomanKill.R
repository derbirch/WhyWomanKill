#########################
#期末作业加油！！！
#########################

#连接数据库
install.packages("RODBC")
library(RODBC)

dbhandle <- odbcDriverConnect('driver={SQL Server};server=(local);database=OpenLaw;trusted_connection=true;DBMSencoding = "UTF-8"')

#写入数据集（中文乱码已解决）
mydata <- sqlQuery(dbhandle, 'select * from [new2019]')

#让我康康你
View(mydata)

#关掉数据库连接
odbcClose(dbhandle)


sqlSave(dbhandle,res,"new_weather",append=FALSE)

?sqlQuery

###############################
#整理数据

#拿出一列康康
head(mydata)
mydata[,3]
#####案件类型 频度表
freq1<-table(mydata[,3])
View(freq1)


##############################
#尝 试 画 图

install.packages("tidyverse")
install.packages("ggplot2")
library(ggplot2)
library(tidyverse)


#案件类型 棒棒糖图
#https://www.r-graph-gallery.com/301-custom-lollipop-chart.html


# Library
library(tidyverse)

# Create data
freq2<-as.data.frame(freq1)

# plot
ggplot(freq2, aes(x=Var1, y=Freq)) +
  geom_segment( aes(x=Var1, xend=Var1, y=0, yend=Freq)) +
  geom_point( size=5, color="red", fill=alpha("orange", 0.3), alpha=0.7, shape=21, stroke=2) 

#
ggplot(freq2, aes(x=Var1, y=Freq)) +
  geom_segment( aes(x=Var1, xend=Var1, y=1000, yend=Freq), color="grey") +
  geom_point( aes(y = Freq),size=Freq,color="orange") +
  geom_text(aes(y = Freq + 200, label = Freq),size=2)+
  theme_light() +
  theme(
    panel.grid.major.x = element_blank(),
    panel.border = element_blank(),
    axis.ticks.x = element_blank()
  ) +
  xlab("案件类型") +
  ylab("案件数量")



#Pie

data <- freq2
# Compute percentages
data$fraction <- data$Freq / sum(data$Freq)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$Var1, "\n value: ", data$Freq)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Var1)) +
  geom_rect() +
  geom_text( x=2, aes(y=labelPosition, label=label, color=Var1), size=6) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

##########################
#日期数据清洗

#看看类型
class(mydata$判决日期)
#拿出来
sentenceDate <- mydata['判决日期']
#康康
View(sentenceDate)
#转成Date类型
sentenceDate <- as.Date(sentenceDate$判决日期, format = '%Y年%m月%d日')

mydata['判决日期'] <- sentenceDate

View(mydata)

##########################
#审判日期 案件类型 统计

#取出这两列
df1 <- data.frame(date=mydata$判决日期,type=mydata$案件类型)

#只保留月份
df1$date <- format(df1$date,format='%m')

#康康到底是什么空白
class(df1[20,2])
df1$type <- as.character(df1$type)

#df1[is.na(df1)] <- '其他'   无效
df1$type[df1$type==''] = '其他'

df1$type <- as.factor(df1$type)
df1$date <- as.factor(df1$date)

View(df1)
class(df1$date)

#########################
#案当事人清理数据

df2 <- data.frame(标题=mydata$标题,原告=mydata$原告,被告=mydata$被告)
write.csv(df2,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\待分类.csv',fileEncoding='GBK')
?write.csv
#偷懒环节
df3 <- data.frame(性别=c('男','女'),数量=c(671,3073))
View(df3)

data <- df3
# Compute percentages
data$fraction <- data$数量 / sum(data$数量)

# Compute the cumulative percentages (top of each rectangle)
data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$性别, "\n value: ", data$数量)

# Make the plot
ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=性别)) +
  geom_rect() +
  geom_text( x=1, aes(y=labelPosition, label=label, color=性别), size=4) + # x here controls label position (inner / outer)
  scale_fill_brewer(palette=3) +
  scale_color_brewer(palette=3) +
  coord_polar(theta="y") +
  xlim(c(-1, 4)) +
  theme_void() +
  theme(legend.position = "none")

#########################
#时间案件数
library(sqldf)
library(ggplot2)
library(dplyr)
library(hrbrthemes)
library(plotly)
install.packages('sqldf')
data <- df1

#执行案件分离
data1 <- sqldf("select * from data where type='执行'") 
data <- sqldf("select * from data where type!='执行'") 

View(data)
plot(df1$date)

p<-ggplot(data = data) +
  geom_bar(mapping = aes(x = date,fill=type))+
  geom_line(data=data1,aes(x = date,y = type), color="grey") +
  geom_point(data=data1,aes(x = date,y = type),shape=2, color="black", fill="#69b3a2", size=6) +
  #theme_ipsum() +
  ggtitle("每月案件统计")+
    #coord_flip()+
  xlab("月份") +
  ylab("案件数量")

p

#核密度图
library(viridis)
library(hrbrthemes)

ggplot() + 
  geom_area(data,mapping = aes(x=date, fill=type),alpha=0.6 , size=.5, colour="white",stat = "count") +
  scale_fill_viridis(discrete = T) +
  theme_ipsum() + 
  ggtitle("每月案件统计")


########################
#地区
install.packages('devtools')
install_github('badbye/baidumap')
install.packages('maps')
install.packages('mapdata')
install.packages('ggmap')

library(baidumap)
library(mapdata)
library(maps)
library(devtools)
library(ggmap)
library(parallel)
#
dfmap <- data.frame(location=mydata$法院)
View(dfmap)

library(dplyr)
dfmap1 <- slice(dfmap,4500:n())#未完成
dfmap1 <- as.matrix(dfmap1)

MAPCo10<-getCoordinate(dfmap1,formatted='T')




write.csv(MAPCo1,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo1.csv',fileEncoding='UTF-8')
write.csv(MAPCo2,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo2.csv',fileEncoding='UTF-8')
write.csv(MAPCo3,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo3.csv',fileEncoding='UTF-8')
write.csv(MAPCo4,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo4.csv',fileEncoding='UTF-8')
write.csv(MAPCo5,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo5.csv',fileEncoding='UTF-8')
write.csv(MAPCo6,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo6.csv',fileEncoding='UTF-8')
write.csv(MAPCo7,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo7.csv',fileEncoding='UTF-8')
write.csv(MAPCo8,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo8.csv',fileEncoding='UTF-8')

write.csv(MAPCo9,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo9.csv',fileEncoding='UTF-8')
write.csv(MAPCo10,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCo10.csv',fileEncoding='UTF-8')

getCoordinate('上海')
View(dfmap1)
View(MAPCo7)

#浏览器端
options(baidumap.key = 'mkDcbjjM2sI7wcTonVcpHzHv3Ga14VUR')

#temp
options(baidumap.key = 'zMje6FMGFIHPOslKzIp4sW24qAnbj0vw')

options(baidumap.key = 'sPVanenQf3zPtDwS2uChA7zucQa2H5AK')
#服务器端
options(baidumap.key = 'N09DRbOZVxmozH26oxTTZIH0nD2CGRRU')
getCoordinate(c('杨浦区人民法院','上海理工大学'),formatted='T')


#####################################
#合并
setwd("C:/Users/wardz/OneDrive/Documents/数据新闻/DATA/location")
a = list.files()
dir = paste("./",a,sep="")
n = length(dir)
merge.data = read.csv(file = dir[1],header =T,sep=',',encoding="UTF-8",quote='')
for(i in 2:n){
  new.data = read.csv(file = dir[i],header =T,sep=",",encoding="UTF-8",quote='')
  merge.data = rbind(merge.data,new.data)
}
write.csv(merge.data,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\MAPCoSUM.csv',fileEncoding='UTF-8')

###################################
#成功！


china_data1<-read.csv(file = 'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\DATA\\location\\MAPCoSUM1.csv',header =T,sep=',',encoding="UTF-8",dec = ".",quote='')
View(china_data1)


View(merge.data)


########################
#案件分组
dfloca <- data.frame(location=mydata$案号)
dfloca <- substr(dfloca$location,7,7) 
class(dfloca)
library(dplyr)
tb <- table(dfloca)
tb <- as.data.frame(tb)
tb <- slice(tb,4:n())
tb <- data.frame(name=tb$dfloca,amount=tb$Freq)
write.csv(tb,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\分省案件数量.csv')

View(tb)
View(dfloca)
setwd("C:/Users/wardz/OneDrive/Documents/数据新闻")



#########################
#杀人案件

library(RODBC)
dbhandle <- odbcDriverConnect('driver={SQL Server};server=(local);database=OpenLaw;trusted_connection=true;DBMSencoding = "UTF-8"')

#写入数据集（中文乱码已解决）
dfKill <- sqlQuery(dbhandle, "select * from sum2019 WHERE 案件类型='刑事' AND 标题 LIKE '%杀%'")
View(dfKill)
write.csv(dfKill,'C:\\Users\\wardz\\OneDrive\\Documents\\数据新闻\\dfKill.csv',fileEncoding='UTF-8')

?register_google

#####################
#案由 词云
library(wordcloud2)
data <- data.frame(var1=mydata$案由)
View(data)
data<- table(data)
data <- data[-1,]
data <- as.character(data)
data <- na.omit(data)
data <- as.factor(data)
class(data[1,2])
class(data$var1)
wordcloud2(data,size=100,minSize=10,gridSize=1,
           
           fontFamily=NULL,fontWeight='normal',
           
           color='random-dark',backgroundColor="white",
           
           minRotation=-pi/6,maxRotation=pi/6,rotateRatio=0.6,
           
           shape='circle',ellipticity=0.65,widgetsize=NULL)








install.packages('gganimate')


#########################
#中文乱码尝试解决
Sys.getlocale()

?iconv()

iconv(mydata,to="UTF-8")

mydata1=enc2utf8(mydata)

Encoding(someX)<-'UTF-8' 
Sys.setenv(NLS_LANG="SIMPLIFIED CHINESE_CHINA.AL32UTF8")
Encoding(mydata) ## [1] "unknown" x2 <- iconv(x, 'GB2312', 'UTF-8') Encoding(x2) ## [1] "UTF-8"
