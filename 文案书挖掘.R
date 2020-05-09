library(devtools)
library(jiebaR)
library(cidian)
library(dplyr)
install_github("qinwf/cidian")


wk = worker(user='user.utf8')
seg<-wk[mydata$判决结果]
df1 <- as.character(mydata$判决结果)
View(df1)

freq(seg)
keys = worker("keywords",topn=50)
vector_keywords(seg,keys)


setwd("~/homework/WhyWomanKill")


decode_scel(scel = "./14108.scel",cpp = TRUE)
decode_scel(scel = "./法律词汇.scel",cpp = TRUE)
decode_scel(scel = "./四级行政.scel",cpp = TRUE)

l=length(df1)
mydata[2,1]
df1[1]
View(wordFreq)
merge.data = wk[df1[1]]
for(i in 2:l){
  temp = wk[df1[i]]
  merge.data = rbind(merge.data,temp)
}
data <- merge.data[nchar(merge.data)>1]
wordFreq <- table(data)
wordFreq<-wordFreq[!grepl('[0-9]+',names(wordFreq))]
wordFreq <- as.data.frame(wordFreq)
wordFreq<- slice(wordFreq,21:n())

df2<-wordFreq[order(wordFreq$Freq,decreasing=T),]
View(data)

data<-slice(df2,1:100)

library(echarts4r)

#ln处理

l<-length(data$data)
for(i in 1:l){
  data[i,3]<-log( data[i,2])
}

#词云
data %>%
  tibble::rownames_to_column("model") %>%
  mutate(model = paste(model, Freq, sep = ",")) %>%
  e_color_range(V3, color) %>% 
  e_charts(data) %>% 
  
  e_cloud(data, V3, color, shape = "circle", sizeRange = c(12,60)) %>% 
  e_title("审判结果", "词云") %>%
  #e_scatter(data,V3,bind=model) %>%
  e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        var vals = params.name.split(',')
        return('名称:'+params.name+'<br/>数量:'+'<strong>' + Math.round(Math.pow(2.718281,params.value)) + '</strong>'
          )}")
  )


View(mtcars)
mtcars %>%  
  tibble::rownames_to_column("model")
#####################
#最简单版
#######
data %>% 
  e_color_range(Freq, color) %>% 
  e_charts() %>% 
  e_cloud(data, Freq, color, shape = "circle", sizeRange = c(20,200)) %>% 
  e_title("Wordcloud", "Random strings") %>%
  e_tooltip()


################################
write.csv(df2,'wordFreq1.csv')

data %>%  
  tibble::rownames_to_column("model") %>%
  mutate(model = paste(model, Freq, sep = ",")) %>%
  e_charts(data) %>% 
  e_scatter(V3, bind = model) %>%
    e_tooltip(formatter = htmlwidgets::JS("
                                        function(params){
                                          var vals = params.name.split(',')
                                          return('<strong>' + vals[0] + 
                                          '</strong><br />wt: ' + params.value[0] + 
                                          '<br />mpg: ' +  params.value[1]) +
                                          '<br />qsec: ' + vals[1]}  "))

