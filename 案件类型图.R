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
#########################

df5<-as.data.frame(table(df1$type))
df5 %>% 
  e_charts(Var1) %>% 
  e_pie(Freq) %>% 
  e_tooltip(formatter = htmlwidgets::JS("
  function(params){
        return('案件类型： ' + params.name+ 
                       '</strong><br />案件数量： ' + params.value+'<br/>占比：'+(params.value/3744*100).toFixed(0)+'%')
      }"))
3744
###############################################

View(df1)
class(df1$date)

library(echarts4r)

df3 <- data.frame(
  source = c("1", "2", "3", "4", "e"),
  target = c("w", "h", "a", "j", "k"),
  value = ceiling(rnorm(5, 10, 1)),
  stringsAsFactors = FALSE
)
df3 %>% 
  e_charts() %>% 
  e_sankey(source, target, value) %>% 
  e_title("Sankey chart")
View(df3)

tb1<- table(df1$date,df1$type)
tb1<- as.data.frame(tb1)



library(sqldf)
df2 <- sqldf("select * from tb1 where Freq!=0") 
df2 <- data.frame(
  Var1=as.character(df2$Var1),
  Var2=as.character(df2$Var2),
  Freq=as.numeric(df2$Freq),
  stringsAsFactors = FALSE)
View(df2)


df2 %>% 
  e_charts() %>% 
  e_sankey(Var1, Var2, Freq) %>% 
  e_tooltip(trigger = "item")%>%
    e_title("每月案件类型数量")
######################################
setwd('~/homework/WhyWomanKill')
#write.csv(df2,'~/homework/WhyWomanKill/DATA/案件类型月份流向.csv')
df2<-read.csv(file = '~/homework/WhyWomanKill/DATA/案件类型月份流向.csv',header =T,sep=',')
View(df2)

