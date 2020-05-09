#######################################
#案由词云
library(echarts4r)
View(data)
data <- data.frame(cause=mydata$案由)
data$cause <- as.character(data$cause)
#data$cause[data$cause==NA] = '其他'
data[is.na(data)] <- '其他'
data$cause <- as.factor(data$cause)

data<-table(data)
data<-as.data.frame(data)

l<-length(data$data)
for(i in 1:l){
  data[i,3]<-log( data[i,2])
}
write.csv(data,'~/homework/WhyWomanKill/DATA/案由词云.csv')
View(data)


data<-read.csv(file = '~/homework/WhyWomanKill/DATA/案由词云.csv',header =T,sep=',')


data %>%
  #tibble::rownames_to_column("model") %>%
  #mutate(model = paste(model, Freq, sep = ",")) %>%
  e_color_range(V3, color) %>% 
  e_charts(data) %>% 
  
  e_cloud(data, V3, color, shape = "circle", sizeRange = c(12,70)) %>% 
  e_title("案由", "词云") %>%
  #e_scatter(data,V3,bind=model) %>%
  e_tooltip(formatter = htmlwidgets::JS("
      function(params){
        var vals = params.name.split(',')
        return('案由:'+params.name+'<br/>数量:'+'<strong>' + Math.round(Math.pow(2.718281,params.value)) + '</strong>'
          )}")
  )