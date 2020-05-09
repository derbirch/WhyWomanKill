install.packages('gapminder')
install.packages('magick')
install.packages('googleVis')
#ggrep
library(googleVis)
library(ggplot2)
library(gganimate)
library(gapminder)
library(magick)

setwd('~/homework/WhyWomanKill')




###################################################

df<-read.csv(file = '~/homework/WhyWomanKill/DATA/分省案件数量2009-2011.csv',header =T,sep=',',dec = ".",quote='')
View(df)

p <- ggplot(df, aes(x = name, y = amount)) +
  geom_bar(stat = "identity", aes(fill = name)) +
  geom_text(aes(label=as.character(amount) ,y = amount+750),angle =90)+
  labs( title = 'Year: {frame_time}',x = '省份', y = '案件数量') +
  theme(legend.position='none',axis.text.x = element_text(angle = 75, hjust = 1),panel.background = element_rect(color = NULL))+
  transition_time(year)
  #transition_states(year, transition_length = 2, state_length = 0)


p

#################################################

#动起来！

#################################################
reorder(x,y)

a<-reorder(df$name,df$amount)

p <- ggplot(df, aes(x = reorder(df$name,df$amount), y = amount)) +
  geom_bar(stat = "identity", aes(fill = name)) +
  geom_text(aes(label=as.character(amount) ,y = amount+750),angle =90)+
  labs( title = 'Year: {frame_time}',x = '省份', y = '案件数量') +
  theme(legend.position='none',axis.text.x = element_text(angle = 75, hjust = 1),panel.background = element_rect(color = NULL))+
  transition_time(year)


#############################

library(echarts4r)
df<-read.csv(file = '~/homework/WhyWomanKill/DATA/分省案件数量2009-2011.csv',header =T,sep=',',dec = ".",quote='')


df %>% 
  group_by(year) %>% 
  e_charts(x=name, timeline = TRUE) %>% 
  e_bar(amount) %>% 
  e_title("2009-2019年各省案件数量图") %>%
  e_tooltip(trigger = "item") %>%
  e_timeline_opts(
    autoPlay = TRUE,
    axis_type = "category",
    playInterval = 1500,
    bottom = 0
  ) %>% 
  e_x_axis(axisLabel = list(interval = 0, rotate = 0))
 





df %>% 
  group_by(year) %>% 
  e_charts(x=name, timeline = TRUE) %>% 
  e_polar() %>% 
  e_angle_axis() %>% 
  e_radius_axis(name) %>% # radius = x 
  e_bar(amount,coord_system = "polar") %>% 
  e_title("2009-2019年各省案件数量图") %>%
  e_tooltip(trigger = "item") %>%
  e_timeline_opts(
    autoPlay = TRUE,
    axis_type = "category",
    playInterval = 1500,
    bottom = 0
  ) 




max <- list(
  name = "Max",
  type = "max"
)

min <- list(
  name = "Min",
  type = "min"
)

avg <- list(
  type = "average",
  name = "AVG"
)

df <- data.frame(year=c(2009,2010,2011,2012,2013,2014,2015,2016,2017,2018,2019),
                 数量=c(1482,3609,7379,11021,24409,97054,119306,84924,14735,9629,3737),
                 增长量=c(0,
                            5091,
                            12470,
                            23491,
                            47900,
                            144954,
                            264260,
                            349184,
                            363919,
                            373548,
                            377285
                          ))
df$year <- as.factor(df$year)
#View(df)
df %>%
  e_charts(year) %>%
  e_bar(数量) %>%
  e_area(增长量, y_index = 1) %>%
  e_mark_point(serie = "数量", data = max) %>% 
  e_mark_point(serie = "数量", data = min) %>% 
  e_mark_point(serie = "增长量", data = max) %>% 
  #e_mark_point(serie = "增长量", data = max) %>% 
  e_tooltip(formatter = htmlwidgets::JS("
  function(params){
        return('年份： ' + params.name+ 
                       '</strong><br />案件数量： ' + params.value[1])
      }"))

